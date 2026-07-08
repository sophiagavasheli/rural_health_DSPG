# prepare counties and population tract centroids for hospital drive time calculations

library(tigris)
library(sf)
library(dplyr)
library(stringr)

# Load TIGRIS Counties Boundary Dataset
us_counties <- counties(cb = TRUE, year = 2020, class = "sf") %>% 
  st_transform(crs = 4326) %>% 
  filter(as.numeric(STATEFP) < 57)

# Load Census Population Centers Dataset
centers = read.table("data/source/census/CenPop2020_Mean_TR.txt", header=TRUE, sep=",")

state = states(cb=TRUE, year = 2020) %>% select(STATEFP, STUSPS)

centers_sf <- centers %>%
  filter(as.integer(STATEFP) < 57) %>%
  mutate(
    STATEFP = sprintf("%02d", as.integer(STATEFP)),
    COUNTYFP = sprintf("%03d", as.integer(COUNTYFP))
  ) %>%
  left_join(state, by = "STATEFP") %>%
  mutate(COUNTYFIPS = paste0(STATEFP, COUNTYFP)) %>% 
  mutate(
    GEOID = str_c(
      str_pad(STATEFP, 2, pad = "0"),
      str_pad(COUNTYFP, 3, pad = "0"),
      str_pad(TRACTCE, 6, pad = "0")
    )) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)


saveRDS(us_counties, "data/outcome/census/us_counties_2020.rds")
saveRDS(centers_sf, "data/outcome/census/clean_pop_centroids_2020.rds")