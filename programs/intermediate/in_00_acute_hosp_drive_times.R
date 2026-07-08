# Calculating the drive time to the nearest acute hospital for ALL states using 2023 OSM roads and hospitals
# this is to run the calculations locally compared to the scripts in the intermediate folder which do this in VT ARC

# make sure you have the state.osm.pbf files in the correct directory generated with cl_01_OSM_state_pbfs.sh

# Load libraries silently for CLI clean output
suppressPackageStartupMessages({
  library(tigris)
  library(dplyr)
  library(osrm)
  library(sf)
  library(stringr)
  library(tidyr)
  library(here)
})

# Force R to the project root directory
setwd(here::here())

# 1. LOAD SOURCE DATASETS
us_counties = readRDS("data/outcome/census/us_counties_2020.rds") %>% st_as_sf()
hosp_sf = readRDS("data/outcome/UNC_shep/clean_UNC_hosps_acute_2023.rds") %>% st_as_sf()
centers_sf = readRDS("data/outcome/census/clean_pop_centroids_2020.rds") %>% st_as_sf()

# 2. BUILD LIST OF ALL 51 STATES (50 states + DC)
state_full_names <- c(
  "alabama", "alaska", "arizona", "arkansas", "california", "colorado",
  "connecticut", "delaware", "florida", "georgia", "hawaii", "idaho",
  "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana",
  "maine", "maryland", "massachusetts", "michigan", "minnesota",
  "mississippi", "missouri", "montana", "nebraska", "nevada",
  "new-hampshire", "new-jersey", "new-mexico", "new-york",
  "north-carolina", "north-dakota", "ohio", "oklahoma", "oregon",
  "pennsylvania", "rhode-island", "south-carolina", "south-dakota",
  "tennessee", "texas","utah", "vermont", "virginia", "washington",
  "west-virginia", "wisconsin", "wyoming", "district-of-columbia"
)

state_lookup_all <- tigris::fips_codes %>%
  distinct(state, state_code, state_name) %>%
  mutate(state_full_name = state_name %>% tolower() %>% stringr::str_replace_all(" ", "-")) %>%
  filter(state_full_name %in% state_full_names)

message(paste("Found", nrow(state_lookup_all), "states/territories to process."))


# MAIN PROCESSING FUNCTION FOR A SINGLE STATE

process_state <- function(target_state) {
  
  state_lookup <- state_lookup_all %>% filter(state == target_state)
  
  if (nrow(state_lookup) == 0) {
    message(paste("Skipping invalid state:", target_state))
    return(invisible(NULL))
  }
  
  state_fips <- state_lookup$state_code[1]
  state_abb  <- tolower(state_lookup$state[1])
  
  # Format full state name e.g., "District of Columbia" -> "district-of-columbia"
  state_full_name <- state_lookup$state_name[1] %>%
    tolower() %>%
    stringr::str_replace_all(" ", "-")
  
  message(paste0("=== Processing State: ", state_lookup$state_name[1], " (", toupper(state_abb), ") ==="))
  
  # 3. SPATIAL FILTERING & PREPARATION
  state_counties <- us_counties %>% filter(STATEFP == state_fips)
  state_hosps <- hosp_sf %>% filter(STATEFP == state_fips)
  state_centers <- centers_sf %>% filter(STATEFP == state_fips)
  
  if (nrow(state_hosps) == 0 || nrow(state_centers) == 0) {
    message("No hospitals or census tracts found for this state. Skipping.")
    return(invisible(NULL))
  }
  
  # Fix coordinate projections for OSRM matrix handling (EPSG:4326 is standard unprojected GPS)
  state_centers <- st_transform(state_centers, 4326)
  state_hosps <- st_transform(state_hosps, 4326)
  
  
  # 4. INITIALIZE / REUSE LOCAL OSRM SERVER ROUTE
  
  
  data_dir <- here::here("data/outcome/OSM/OSM_states_2023")
  data_dir_clean <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
  pbf_file <- paste0(state_full_name, ".osm.pbf")
  container_name <- paste0("osrm_", state_full_name)
  
  # Check if the container already exists (running or stopped)
  check_container <- system(paste0("docker ps -a -q -f name=^", container_name, "$"), intern = TRUE)
  
  if (length(check_container) > 0) {
    # --- CASE A: Container already exists, just turn it on ---
    message(paste("Existing OSRM container found. Starting:", container_name))
    system(paste("docker start", container_name))
    
    # Existing containers take less time to wake up, but still need a brief moment
    Sys.sleep(10)
    
  } else {
    # --- CASE B: Container does not exist, run processing steps from scratch ---
    message("No existing container found. Constructing OSRM maps from scratch...")
    
    message("=== Step 1: Extracting OSRM Profile ===")
    cmd_extract <- paste0(
      "docker run --rm -v ", data_dir_clean, ":/data",
      " osrm/osrm-backend osrm-extract -p /usr/local/share/osrm/profiles/car.lua /data/", pbf_file
    )
    system(cmd_extract)
    
    message("=== Step 2: Partitioning OSRM Data ===")
    cmd_partition <- paste0(
      "docker run --rm -v ", data_dir_clean, ":/data",
      " osrm/osrm-backend osrm-partition /data/", state_full_name, ".osrm"
    )
    system(cmd_partition)
    
    message("=== Step 3: Customizing OSRM Data ===")
    cmd_customize <- paste0(
      "docker run --rm -v ", data_dir_clean, ":/data",
      " osrm/osrm-backend osrm-customize /data/", state_full_name, ".osrm"
    )
    system(cmd_customize)
    
    message(paste("=== Step 4: Launching OSRM Server for", state_full_name, "==="))
    cmd_launch <- paste0(
      "docker run -d --name ", container_name,
      " -p 5000:5000 -v ", data_dir_clean, ":/data",
      " osrm/osrm-backend osrm-routed --algorithm mld --max-table-size 100000 /data/",
      state_full_name, ".osrm"
    )
    exit_code <- system(cmd_launch)
    
    if (exit_code != 0) {
      message("Docker failed to launch the OSRM server. Skipping state.")
      return(invisible(NULL))
    }
    
    # Crucial: Give Docker plenty of time to read the maps into RAM on its first build
    # for large states like California and Texas, I had to increase this time to 60s
    message("Waiting 40 seconds for the new server engine to map into memory...")
    Sys.sleep(40)
  }
  
  options(osrm.server = "http://localhost:5000/")
  
  
  # 5. EFFICIENT ROUTING (10 Nearest hospitals)
  
  all_tract_results <- list()
  
  message("Calculating 10 nearest hospitals for each census tract via straight-line distance...")
  
  # Calculate spatial Euclidean/Great-Circle distance matrix
  geo_dist_matrix <- st_distance(state_centers, state_hosps)
  
  for (i in seq_len(nrow(state_centers))) {
    current_tract <- state_centers[i, ]
    
    t_id <- current_tract$GEOID
    c_id <- current_tract$COUNTYFP
    
    # Find indices of the 10 closest hospitals geometrically
    closest_hosp_idx <- order(geo_dist_matrix[i, ])[1:min(10, ncol(geo_dist_matrix))]
    current_hosps <- state_hosps[closest_hosp_idx, ]
    
    # Isolate standard sf layers for OSRM matrix handling
    current_tract_sf <- current_tract
    current_hosps_sf <- current_hosps
    
    # Force clean character assignment for row names to map cleanly into matrix frames
    rownames(current_tract_sf) <- as.character(t_id)
    rownames(current_hosps_sf) <- as.character(current_hosps_sf$id) 
    
    osrm_matrix <- tryCatch(
      osrmTable(
        src = current_tract_sf,
        dst = current_hosps_sf,
        measure = c("duration", "distance")
      ),
      error = function(e) {
        message(
          "Failed tract ",
          t_id,
          " county ",
          c_id,
          ": ",
          e$message
        )
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
  
  # Safeguard check to ensure data collection loop populated correctly
  if (length(all_tract_results) == 0) {
    message("No routing matrices were generated for this state. Skipping export.")
  } else {
    
    # 6. COMBINE AND SUMMARY METRICS
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
    
    # 7. GENERATE EXPORTS
    final_dat <- state_counties %>%
      left_join(average_county_access, by = c("COUNTYFP" = "county_id"))
    
    saveRDS(final_dat, paste0("data/outcome/OSM/drive_times/", state_abb, "_acute_hosp_drive_times.rds"))
    
    message(paste("Processing complete for", toupper(state_abb), ". Files saved!"))
  }
  
  
  # 8. AUTOMATED OSRM CONTAINER TEARDOWN (STOP ONLY)
  
  message("Stopping local OSRM server (preserving structure)...")
  
  system(paste("docker stop", container_name), ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  message("Server stopped successfully.")
  
  invisible(NULL)
}


# RUN FOR ALL 51 STATES 

for (st in state_lookup_all$state) {
  tryCatch(
    process_state(st),
    error = function(e) {
      message(paste("ERROR processing", st, "-", e$message, "- continuing to next state."))
    }
  )
}

message("=== All states processed. ===")