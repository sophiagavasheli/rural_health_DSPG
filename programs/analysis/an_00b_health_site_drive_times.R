# Calculating the drive time to the nearest health site (of several types) for ONE state (2023 OSM roads)
# this script is meant to be run by an_00_submit_drive_times.slurm

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

# 1b. HEALTH SITE TYPES TO COMPUTE DRIVE TIMES FOR
# Each of these must appear as a value in the `health_site_type` column of the
# combined health-sites dataset loaded in section 3 below.
sites <- c(
  "acute_care_hospital",
  "doctors_medical_specialists",
  "mental_health",
  "dentist",
  "clinic_urgent_care",
  "pharmacy"
)

# 2. GET TARGET STATE FROM COMMAND LINE (SLURM ARRAY TASK)
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("No state supplied.")
}

target_state_full_name <- tolower(args[1])

if (!target_state_full_name %in% state_lookup_all$state_full_name) {
  stop(paste0("'", target_state_full_name, "' is not a recognized state_full_name. ",
              "Check spelling (e.g. 'new-mexico', 'district-of-columbia').") )
}

message(paste("SLURM task received state:", target_state_full_name))

# 3. LOAD SOURCE DATASETS
us_counties <- readRDS("data/us_counties_2020.rds") %>% st_as_sf()
centers_sf  <- readRDS("data/clean_pop_centroids_2020.rds") %>% st_as_sf()


health_sites_sf <- readRDS("data/drive_time_health_sites_2023.rds") %>% st_as_sf()


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


# HELPER: compute drive-time matrices + county summary for ONE site type,
# reusing an already-running OSRM server. Saves its own results file.
process_site_type <- function(site_type, state_sites_all, state_centers,
                              state_counties, state_abb) {
  
  message(paste0("--- Routing to nearest: ", site_type, " ---"))
  
  state_sites <- state_sites_all %>% filter(health_site_type == site_type)
  
  if (nrow(state_sites) == 0) {
    message(paste0("No '", site_type, "' sites found in this state. Skipping this type."))
    return(invisible(NULL))
  }
  
  all_tract_results <- list()
  
  geo_dist_matrix <- st_distance(state_centers, state_sites)
  
  for (i in seq_len(nrow(state_centers))) {
    current_tract <- state_centers[i, ]
    
    t_id <- current_tract$GEOID
    c_id <- current_tract$COUNTYFP
    
    closest_site_idx <- order(geo_dist_matrix[i, ])[1:min(10, ncol(geo_dist_matrix))]
    current_sites <- state_sites[closest_site_idx, ]
    
    current_tract_sf <- current_tract
    current_sites_sf <- current_sites
    
    rownames(current_tract_sf) <- as.character(t_id)
    rownames(current_sites_sf) <- as.character(current_sites_sf$site_id)
    
    osrm_matrix <- tryCatch(
      osrmTable(
        src = current_tract_sf,
        dst = current_sites_sf,
        measure = c("duration", "distance")
      ),
      error = function(e) {
        message("Failed tract ", t_id, " county ", c_id, " (", site_type, "): ", e$message)
        NULL
      }
    )
    
    if (is.null(osrm_matrix)) next
    
    dist_df <- as.data.frame(osrm_matrix$distances) %>%
      mutate(tract_id = t_id) %>%
      pivot_longer(cols = -tract_id, names_to = "site_id", values_to = "distance_meters")
    
    dur_df <- as.data.frame(osrm_matrix$durations) %>%
      mutate(tract_id = t_id) %>%
      pivot_longer(cols = -tract_id, names_to = "site_id", values_to = "duration_minutes")
    
    complete_tract_df <- left_join(dist_df, dur_df, by = c("tract_id", "site_id")) %>%
      mutate(county_id = c_id)
    
    all_tract_results[[as.character(t_id)]] <- complete_tract_df
  }
  
  if (length(all_tract_results) == 0) {
    message(paste0("No routing matrices were generated for ", site_type, ". Skipping export."))
    return(invisible(NULL))
  }
  
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
  
  final_dat <- state_counties %>%
    left_join(average_county_access, by = c("COUNTYFP" = "county_id")) %>%
    mutate(health_site_type = site_type, .after = COUNTYFP)
  
  out_file <- paste0("results/", state_abb, "_", site_type, "_drive_times.rds")
  dir.create("results", showWarnings = FALSE, recursive = TRUE)
  saveRDS(final_dat, out_file)
  
  message(paste("Saved:", out_file))
  invisible(NULL)
}


# MAIN PROCESSING FUNCTION FOR A SINGLE STATE (all site types)

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
  
  state_sites_all <- health_sites_sf %>%
    filter(STATEFP == state_fips, health_site_type %in% sites)
  
  state_centers <- centers_sf %>% filter(STATEFP == state_fips)
  
  if (nrow(state_sites_all) == 0 || nrow(state_centers) == 0) {
    message("No health sites or census tracts found for this state. Skipping.")
    return(invisible(NULL))
  }
  
  state_centers   <- st_transform(state_centers, 4326)
  state_sites_all <- st_transform(state_sites_all, 4326)
  
  present_site_types <- intersect(sites, unique(state_sites_all$health_site_type))
  missing_site_types <- setdiff(sites, present_site_types)
  if (length(missing_site_types) > 0) {
    message("No sites of these types in this state: ", paste(missing_site_types, collapse = ", "))
  }
  
  
  # 5. INITIALIZE LOCAL OSRM SERVER VIA APPTAINER (once per state, reused across all site types)
  
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
  
  
  # 6. ROUTE FOR EVERY SITE TYPE, REUSING THE SAME SERVER
  for (site_type in sites) {
    process_site_type(
      site_type       = site_type,
      state_sites_all = state_sites_all,
      state_centers   = state_centers,
      state_counties  = state_counties,
      state_abb       = state_abb
    )
  }
  
  
  # 7. TEARDOWN: kill the backgrounded osrm-routed process
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