# va hospital drive times

library(tigris)
library(dplyr)
library(osrm)
library(sf)
library(stringr)
library(tidyr)
library(here)


# 1. LOAD AND PREPARE SOURCE DATASETS

# Load CMS Hospital Dataset
hosp <- read.csv(here("data/outcome/CMS/cms2022_clean.csv"))
hosp_sf <- hosp %>% st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Load TIGRIS Counties Boundary Dataset
us_counties <- counties(cb = TRUE, year = 2022, class = "sf") %>% 
  st_transform(crs = 4326)

# Load Census Population Centers Dataset
centers = read.table("https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR.txt", 
           header=TRUE, sep=",")
centers_sf <- centers %>% st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)


# 2. SPATIAL INTERSECTIONS AND FILTERS

# Spatial Join to assign County attributes to Hospitals
hospitals_with_counties <- st_join(hosp_sf, us_counties, join = st_intersects)

# Isolate Virginia records (FIPS State Code = 51)
va_hosps <- hospitals_with_counties %>% 
  filter(STATEFP == 51) %>% 
  rename_with(tolower)

va_centers <- centers_sf %>% 
  filter(STATEFP == 51) %>% 
  rename_with(tolower)

# keep as character
va_hosps$countyfp = as.character(va_hosps$countyfp)
va_centers$countyfp = as.character(va_centers$countyfp)

# 3. CONVERT SF GEOMETRIES INTO FLAT COORDINATES

# Prepare Census Tract Centers (Origins)
osrm_origins <- va_centers %>%
  mutate(
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  ) %>%
  sf::st_drop_geometry() %>%
  select(tractce, lon, lat, countyfp) %>% 
  mutate(countyfp = stringr::str_pad(countyfp, 3, pad = "0"))

# Prepare Hospitals (Destinations)
osrm_dests <- va_hosps %>%
  mutate(
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  ) %>%
  sf::st_drop_geometry() %>%
  select(facility.id, lon, lat, countyfp) %>% 
  mutate(countyfp = stringr::str_pad(countyfp, 3, pad = "0"))


# 4. INITIALIZE LOCAL OSRM SERVER ROUTE
options(osrm.server = "http://localhost:5000/")

#command to run in docker
#docker run -d -p 5000:5000 -v "${PWD}:/data"  osrm/osrm-backend osrm-routed --algorithm mld --max-table-size 100000 /data/virginia-260624.osrm


# 5. RUN THE COUNTY-BOUNDED ROUTING LOOP

# Find common county FIPS codes shared across both datasets
#valid_counties <- intersect(osrm_origins$countyfp, osrm_dests$countyfp)
all_county_results <- list()

for (co in unique(osrm_origins$countyfp)) {
  
  # Isolate raw table rows for the current county
  co_origins_raw <- osrm_origins %>% filter(countyfp == co)
  co_dests_raw   <- osrm_dests #%>% filter(countyfp == co)
  
  # Row Check: Skip empty counties to prevent API crashes
  if (nrow(co_origins_raw) == 0 || nrow(co_dests_raw) == 0) {
    message(paste("Skipping County FIPS:", co, "- Missing data pairs."))
    next
  }
  
  # Convert tables back to explicit SF Points inside the loop to fix formatting errors
  co_origins_sf <- st_as_sf(co_origins_raw, coords = c("lon", "lat"), crs = 4326)
  co_dests_sf   <- st_as_sf(co_dests_raw, coords = c("lon", "lat"), crs = 4326)
  
  # Bind specific unique target labels to Row Names for matrix metadata extraction
  rownames(co_origins_sf) <- co_origins_raw$tractce
  rownames(co_dests_sf)   <- co_dests_raw$facility.id
  
  # Query the local OSRM engine instantly
  osrm_matrix <- tryCatch(
    osrmTable(
      src = co_origins_sf,
      dst = co_dests_sf,
      measure = c("duration", "distance")
    ),
    error = function(e) {
      message("County ", co, " failed: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(osrm_matrix)) next
  
  # Process Distance Output Matrix (Meters)
  dist_mat <- osrm_matrix$distances
  county_df <- as.data.frame(dist_mat) %>%
    mutate(tract_id = rownames(dist_mat)) %>%
    pivot_longer(
      cols = -tract_id,
      names_to = "hospital_id",
      values_to = "distance_meters"
    ) %>%
    mutate(county_id = co)
  
  # Process Duration Output Matrix (Minutes)
  dur_mat <- osrm_matrix$durations
  dur_df <- as.data.frame(dur_mat) %>%
    mutate(tract_id = rownames(dur_mat)) %>%
    pivot_longer(
      cols = -tract_id,
      names_to = "hospital_id",
      values_to = "duration_minutes"
    )
  
  # Merge distance and duration vectors cleanly
  complete_county_df <- left_join(county_df, dur_df, by = c("tract_id", "hospital_id"))
  
  # Save current county block to storage list
  all_county_results[[as.character(co)]] <- complete_county_df
}


# 6. COMBINE ALL RESULTS INTO FINAL LONG-FORMAT DATAFRAME
final_va_access_df <- bind_rows(all_county_results)


average_county_access <- final_va_access_df %>%
  # 1. For each tract, find only the single closest hospital
  group_by(county_id, tract_id) %>%
  filter(duration_minutes == min(duration_minutes, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # 2. Average those closest tract times up to the county level
  group_by(county_id) %>%
  summarize(
    avg_drive_time_minutes = mean(duration_minutes, na.rm = TRUE),
    max_drive_time_minutes = max(duration_minutes, na.rm = TRUE),
    total_tracts_evaluated = n_distinct(tract_id)
  )

# data for leaflet
mapdat = us_counties %>% 
  filter(STATEFP == 51) %>% 
  left_join(average_county_access, by = c("COUNTYFP" = "county_id")) %>% 
  select(COUNTYFP, avg_drive_time_minutes)
  
mapdat <- st_drop_geometry(mapdat)
coords <- st_coordinates(st_point_on_surface(hospitals_with_counties))

map_hosp <- cbind(
  st_drop_geometry(hospitals_with_counties),
  lon = coords[,1],
  lat = coords[,2]
)

map_va_centers <- centers %>% 
  filter(STATEFP == 51)


write.csv(mapdat, "shiny_dashboard/drive_map_dat.csv",row.names = FALSE)
write.csv(map_hosp, "shiny_dashboard/hosps.csv",row.names = FALSE)
write.csv(va_centers, "shiny_dashboard/va_centers.csv",row.names = FALSE)
