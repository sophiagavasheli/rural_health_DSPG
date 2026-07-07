# ============================================================
# OSRM DRIVE TIME: CALIFORNIA ONLY
# ============================================================

library(tigris)
library(dplyr)
library(osrm)
library(sf)
library(stringr)
library(tidyr)


setwd(here::here())

# 1. LOAD DATA
us_counties <- readRDS("data/outcome/census/us_counties_2023.rds") %>% st_as_sf()
hosp_sf <- readRDS("data/outcome/UNC_shep/clean_UNC_hosps_acute_2023.rds") %>% st_as_sf()
centers_sf <- readRDS("data/outcome/census/clean_pop_centroids_2020.rds") %>% st_as_sf()

# missing TX and CA
# ============================================================
# 2. FILTER ONLY 
# ============================================================

state_lookup <- tigris::fips_codes %>%
  distinct(state, state_code, state_name) %>%
  mutate(state_full_name = state_name %>%
           tolower() %>%
           stringr::str_replace_all(" ", "-")) %>%
  filter(state == "CA")

state_fips <- state_lookup$state_code[1]
state_abb  <- "ca"
state_full_name <- state_lookup$state_name[1] %>%
  tolower() %>%
  stringr::str_replace_all(" ", "-")

message(paste0("=== Processing==="))

# ============================================================
# 3. SUBSET DATA
# ============================================================

state_counties <- us_counties %>% filter(STATEFP == state_fips)
state_hosps <- hosp_sf %>% filter(STATEFP == state_fips)
state_centers <- centers_sf %>% filter(STATEFP == state_fips)

stopifnot(nrow(state_hosps) > 0, nrow(state_centers) > 0)

state_centers <- st_transform(state_centers, 4326)
state_hosps <- st_transform(state_hosps, 4326)

# ============================================================
# 4. OSRM SERVER SETUP
# ============================================================

data_dir <- here::here("data/outcome/OSM/OSM_states_2023")
data_dir_clean <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
pbf_file <- paste0(state_full_name, ".osm.pbf")

container_name <- paste0("osrm_", state_full_name)

check_container <- system(
  paste0("docker ps -a -q -f name=^", container_name, "$"),
  intern = TRUE
)

if (length(check_container) > 0) {
  
  message("Existing OSRM container found. Starting...")
  system(paste("docker start", container_name))
  
  Sys.sleep(20)  # ↑ increased warm-up time
  
} else {
  
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
  
  message("Waiting for OSRM server warm-up (60 seconds)...")
  Sys.sleep(60)
}

options(osrm.server = "http://localhost:5000/")

# ============================================================
# 5. ROUTING
# ============================================================

all_tract_results <- list()

geo_dist_matrix <- st_distance(state_centers, state_hosps)

message("Computing nearest hospitals...")

for (i in seq_len(nrow(state_centers))) {
  
  current_tract <- state_centers[i, ]
  t_id <- current_tract$GEOID
  c_id <- current_tract$COUNTYFP
  
  closest_idx <- order(geo_dist_matrix[i, ])[1:min(10, ncol(geo_dist_matrix))]
  current_hosps <- state_hosps[closest_idx, ]
  
  rownames(current_tract) <- as.character(t_id)
  rownames(current_hosps) <- as.character(current_hosps$id)
  
  osrm_matrix <- tryCatch(
    osrmTable(
      src = current_tract,
      dst = current_hosps,
      measure = c("duration", "distance")
    ),
    error = function(e) NULL
  )
  
  if (is.null(osrm_matrix)) next
  
  dist_df <- as.data.frame(osrm_matrix$distances) %>%
    mutate(tract_id = t_id) %>%
    pivot_longer(-tract_id, names_to = "hospital_id", values_to = "distance_meters")
  
  dur_df <- as.data.frame(osrm_matrix$durations) %>%
    mutate(tract_id = t_id) %>%
    pivot_longer(-tract_id, names_to = "hospital_id", values_to = "duration_minutes")
  
  all_tract_results[[as.character(t_id)]] <-
    left_join(dist_df, dur_df, by = c("tract_id", "hospital_id")) %>%
    mutate(county_id = c_id)
}

# ============================================================
# 6. SUMMARY
# ============================================================

final_access_df <- bind_rows(all_tract_results)

average_county_access <- final_access_df %>%
  group_by(county_id) %>%
  summarize(
    avg_drive_time_minutes = mean(duration_minutes, na.rm = TRUE),
    max_drive_time_minutes = max(duration_minutes, na.rm = TRUE),
    min_drive_time_minutes = min(duration_minutes, na.rm = TRUE),
    total_tracts_evaluated = n_distinct(tract_id),
    .groups = "drop"
  )

final_dat <- state_counties %>%
  left_join(average_county_access, by = c("COUNTYFP" = "county_id"))

saveRDS(
  final_dat,
  paste0("data/outcome/OSM/drive_times/ca_acute_hosp_drive_times.rds")
)

# ============================================================
# 7. STOP SERVER
# ============================================================

message("Stopping OSRM server...")
system(paste("docker stop", container_name), ignore.stdout = TRUE)

message("DONE: California complete")