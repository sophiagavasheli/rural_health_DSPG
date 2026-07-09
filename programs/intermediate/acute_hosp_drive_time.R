# Calculating the drive time to the nearest acute hospital for ONE state (2023 OSM roads and hospitals)
# this script is meant to be run with

suppressPackageStartupMessages({
  library(dplyr)
  library(osrm)
  library(sf)
  library(stringr)
  library(tidyr)
})


# 1. HARDCODED STATE FIPS LOOKUP (50 states + DC)
state_lookup_all <- tibble::tribble(
  ~state, ~state_code, ~state_name,
  "AL", "01", "Alabama",
  "AK", "02", "Alaska",
  "AZ", "04", "Arizona",
  "AR", "05", "Arkansas",
  "CA", "06", "California",
  "CO", "08", "Colorado",
  "CT", "09", "Connecticut",
  "DE", "10", "Delaware",
  "DC", "11", "District of Columbia",
  "FL", "12", "Florida",
  "GA", "13", "Georgia",
  "HI", "15", "Hawaii",
  "ID", "16", "Idaho",
  "IL", "17", "Illinois",
  "IN", "18", "Indiana",
  "IA", "19", "Iowa",
  "KS", "20", "Kansas",
  "KY", "21", "Kentucky",
  "LA", "22", "Louisiana",
  "ME", "23", "Maine",
  "MD", "24", "Maryland",
  "MA", "25", "Massachusetts",
  "MI", "26", "Michigan",
  "MN", "27", "Minnesota",
  "MS", "28", "Mississippi",
  "MO", "29", "Missouri",
  "MT", "30", "Montana",
  "NE", "31", "Nebraska",
  "NV", "32", "Nevada",
  "NH", "33", "New Hampshire",
  "NJ", "34", "New Jersey",
  "NM", "35", "New Mexico",
  "NY", "36", "New York",
  "NC", "37", "North Carolina",
  "ND", "38", "North Dakota",
  "OH", "39", "Ohio",
  "OK", "40", "Oklahoma",
  "OR", "41", "Oregon",
  "PA", "42", "Pennsylvania",
  "RI", "44", "Rhode Island",
  "SC", "45", "South Carolina",
  "SD", "46", "South Dakota",
  "TN", "47", "Tennessee",
  "TX", "48", "Texas",
  "UT", "49", "Utah",
  "VT", "50", "Vermont",
  "VA", "51", "Virginia",
  "WA", "53", "Washington",
  "WV", "54", "West Virginia",
  "WI", "55", "Wisconsin",
  "WY", "56", "Wyoming"
) %>%
  mutate(state_full_name = state_name %>% tolower() %>% stringr::str_replace_all(" ", "-"))

# 2. GET TARGET STATE FROM COMMAND LINE (SLURM ARRAY TASK)
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("No state supplied.")
}

target_state_full_name <- tolower(args[1])

if (!target_state_full_name %in% state_lookup_all$state_full_name) {
  stop(paste0("'", target_state_full_name, "' is not a recognized state_full_name. ",
              "Check spelling (e.g. 'new-mexico', 'district-of-columbia')."))
}

message(paste("SLURM task received state:", target_state_full_name))

# 3. LOAD SOURCE DATASETS
us_counties  <- readRDS("data/us_counties_2020.rds") %>% st_as_sf()
hosp_sf      <- readRDS("data/clean_UNC_hosps_acute_2023.rds") %>% st_as_sf()
centers_sf   <- readRDS("data/clean_pop_centroids_2020.rds") %>% st_as_sf()


# HELPER: poll a local TCP port until something answers (or timeout)
wait_for_server <- function(port, timeout_sec = 90, poll_every = 3) {
  waited <- 0
  while (waited < timeout_sec) {
    ok <- tryCatch({
      con <- suppressWarnings(socketConnection(
        host = "localhost", port = port, blocking = TRUE, open = "r", timeout = 2
      ))
      close(con)
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(TRUE)
    Sys.sleep(poll_every)
    waited <- waited + poll_every
  }
  FALSE
}


# MAIN PROCESSING FUNCTION FOR A SINGLE STATE

process_state <- function(target_state_full_name) {
  
  state_lookup <- state_lookup_all %>% filter(state_full_name == target_state_full_name)
  
  if (nrow(state_lookup) == 0) {
    message(paste("Skipping invalid state:", target_state_full_name))
    return(invisible(NULL))
  }
  
  state_fips <- state_lookup$state_code[1]
  state_abb  <- tolower(state_lookup$state[1])
  state_full_name <- state_lookup$state_full_name[1]
  
  message(paste0("=== Processing State: ", state_lookup$state_name[1], " (", toupper(state_abb), ") ==="))
  
  # 4. SPATIAL FILTERING & PREPARATION
  state_counties <- us_counties %>% filter(STATEFP == state_fips)
  state_hosps    <- hosp_sf %>% filter(STATEFP == state_fips)
  state_centers  <- centers_sf %>% filter(STATEFP == state_fips)
  
  if (nrow(state_hosps) == 0 || nrow(state_centers) == 0) {
    message("No hospitals or census tracts found for this state. Skipping.")
    return(invisible(NULL))
  }
  
  state_centers <- st_transform(state_centers, 4326)
  state_hosps   <- st_transform(state_hosps, 4326)
  
  
  # 5. INITIALIZE LOCAL OSRM SERVER VIA APPTAINER
  
  data_dir <- ("data/OSM_states_2023")
  data_dir_clean <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
  pbf_file <- paste0(state_full_name, ".osm.pbf")
  
  sif_path <- ("data/containers/osrm-backend.sif")
  if (!file.exists(sif_path)) {
    stop(paste0("Container image not found at ", sif_path,
                " -- run the one-time 'apptainer pull' setup step first."))
  }
  
  # Unique port per array task so concurrent tasks on the same node don't collide
  osrm_port <- Sys.getenv("OSRM_PORT", unset = "5000")
  
  log_dir <- ("logs")
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(log_dir, paste0("osrm_", state_full_name, "_", Sys.getenv("SLURM_ARRAY_TASK_ID", "na"), ".log"))
  pid_file <- file.path(log_dir, paste0("osrm_", state_full_name, "_", Sys.getenv("SLURM_ARRAY_TASK_ID", "na"), ".pid"))
  
  # Use existence of the .osrm.mldgr file as the marker that extract/partition/customize already completed for this state 
  mldgr_file <- file.path(data_dir_clean, paste0(state_full_name, ".osrm.mldgr"))
  
  apptainer_bind <- paste0("--bind ", data_dir_clean, ":/data")
  
  if (file.exists(mldgr_file)) {
    message("Preprocessed OSRM data already exists for this state. Skipping extract/partition/customize.")
  } else {
    message("No preprocessed OSRM data found. Running extract/partition/customize from scratch...")
    
    message("=== Step 1: Extracting OSRM Profile ===")
    system(paste(
      "apptainer exec", apptainer_bind, sif_path,
      "osrm-extract -p /usr/local/share/osrm/profiles/car.lua",
      paste0("/data/", pbf_file)
    ))
    
    message("=== Step 2: Partitioning OSRM Data ===")
    system(paste(
      "apptainer exec", apptainer_bind, sif_path,
      "osrm-partition",
      paste0("/data/", state_full_name, ".osrm")
    ))
    
    message("=== Step 3: Customizing OSRM Data ===")
    system(paste(
      "apptainer exec", apptainer_bind, sif_path,
      "osrm-customize",
      paste0("/data/", state_full_name, ".osrm")
    ))
    
    if (!file.exists(mldgr_file)) {
      message("osrm-customize did not produce the expected .osrm.mldgr file. Skipping state.")
      return(invisible(NULL))
    }
  }
  
  message(paste("=== Step 4: Launching OSRM Server for", state_full_name, "on port", osrm_port, "==="))
  
  # Launch osrm-routed in the background, capture its PID, redirect output to a log file
  cmd_launch <- paste0(
    "nohup apptainer exec ", apptainer_bind, " ", sif_path,
    " osrm-routed --algorithm mld --max-table-size 100000 --port ", osrm_port,
    " /data/", state_full_name, ".osrm",
    " > ", log_file, " 2>&1 & echo $! > ", pid_file
  )
  system(cmd_launch, wait = FALSE)
  
  message("Waiting for OSRM server to come online...")
  server_up <- wait_for_server(port = as.integer(osrm_port), timeout_sec = 90)
  
  if (!server_up) {
    message("OSRM server did not come up in time. Skipping state. See log: ")
    message(log_file)
    if (file.exists(pid_file)) {
      pid <- readLines(pid_file, warn = FALSE)[1]
      if (!is.na(pid) && nzchar(pid)) system(paste("kill", pid), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    return(invisible(NULL))
  }
  
  options(osrm.server = paste0("http://localhost:", osrm_port, "/"))
  
  
  # 6. EFFICIENT ROUTING (10 Nearest hospitals)
  all_tract_results <- list()
  
  message("Calculating 10 nearest hospitals for each census tract via straight-line distance...")
  
  geo_dist_matrix <- st_distance(state_centers, state_hosps)
  
  for (i in seq_len(nrow(state_centers))) {
    current_tract <- state_centers[i, ]
    
    t_id <- current_tract$GEOID
    c_id <- current_tract$COUNTYFP
    
    closest_hosp_idx <- order(geo_dist_matrix[i, ])[1:min(10, ncol(geo_dist_matrix))]
    current_hosps <- state_hosps[closest_hosp_idx, ]
    
    current_tract_sf <- current_tract
    current_hosps_sf <- current_hosps
    
    rownames(current_tract_sf) <- as.character(t_id)
    rownames(current_hosps_sf) <- as.character(current_hosps_sf$id)
    
    osrm_matrix <- tryCatch(
      osrmTable(
        src = current_tract_sf,
        dst = current_hosps_sf,
        measure = c("duration", "distance")
      ),
      error = function(e) {
        message("Failed tract ", t_id, " county ", c_id, ": ", e$message)
        NULL
      }
    )
    
    if (is.null(osrm_matrix)) next
    
    dist_df <- as.data.frame(osrm_matrix$distances) %>%
      mutate(tract_id = t_id) %>%
      pivot_longer(cols = -tract_id, names_to = "hospital_id", values_to = "distance_meters")
    
    dur_df <- as.data.frame(osrm_matrix$durations) %>%
      mutate(tract_id = t_id) %>%
      pivot_longer(cols = -tract_id, names_to = "hospital_id", values_to = "duration_minutes")
    
    complete_tract_df <- left_join(dist_df, dur_df, by = c("tract_id", "hospital_id")) %>%
      mutate(county_id = c_id)
    
    all_tract_results[[as.character(t_id)]] <- complete_tract_df
  }
  
  if (length(all_tract_results) == 0) {
    message("No routing matrices were generated for this state. Skipping export.")
  } else {
    
    # 7. COMBINE AND SUMMARY METRICS
    final_access_df <- bind_rows(all_tract_results)
    
    average_county_access <- final_access_df %>%
      group_by(county_id, tract_id) %>%
      filter(duration_minutes == min(duration_minutes, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(county_id) %>%
      summarize(
        avg_drive_time_minutes = mean(duration_minutes, na.rm = TRUE),
        max_drive_time_minutes = max(duration_minutes, na.rm = TRUE),
        min_drive_time_minutes = min(duration_minutes, na.rm = TRUE),
        total_tracts_evaluated = n_distinct(tract_id),
        .groups = "drop"
      )
    
    # 8. GENERATE EXPORTS
    final_dat <- state_counties %>%
      left_join(average_county_access, by = c("COUNTYFP" = "county_id"))
    
    saveRDS(final_dat, paste0("results/", state_abb, "_acute_hosp_drive_times.rds"))
    
    message(paste("Processing complete for", toupper(state_abb), ". Files saved!"))
  }
  
  # 9. TEARDOWN: kill the backgrounded osrm-routed process
  message("Stopping local OSRM server...")
  if (file.exists(pid_file)) {
    pid <- readLines(pid_file, warn = FALSE)[1]
    if (!is.na(pid) && nzchar(pid)) {
      system(paste("kill", pid), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    file.remove(pid_file)
  }
  message("Server stopped.")
  
  invisible(NULL)
}


# RUN FOR THE SINGLE STATE PASSED TO THIS ARRAY TASK
tryCatch(
  process_state(target_state_full_name),
  error = function(e) {
    message(paste("ERROR processing", target_state_full_name, "-", e$message))
    quit(status = 1)  # non-zero exit so SLURM marks the array task as failed
  }
)

message(paste("=== Task complete for", target_state_full_name, "==="))