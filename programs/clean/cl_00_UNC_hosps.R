# cleaning/geocoding UNC hospital list 2023

library(readxl)
library(dplyr)
library(sf)
library(tidygeocoder)
library(tigris)

acute = read_excel("data/source/UNC_shep/Hospital-List2023.xlsx", sheet="ACUTE")

spec = read_excel("data/source/UNC_shep/Hospital-List2023.xlsx", sheet="SPECIALTY")

filt_acute = acute %>% 
  select(ID, NAME, ADDRESS, CITY, STATE, ZIP, FIPS, `POS TOTAL BEDS`) %>% 
  rename(pos_total_beds = `POS TOTAL BEDS`) %>% 
  rename_with(tolower) %>% 
  mutate(type = "acute")

filt_spec = spec %>% 
  select(ID, NAME, ADDRESS, CITY, STATE, ZIP, FIPS, `POS TOTAL BEDS`, TYPE) %>% 
  rename(pos_total_beds = `POS TOTAL BEDS`) %>% 
  rename_with(tolower) %>% 
  mutate(type = case_when(
    type == "LTACH" ~ "long term care",
    type == "REHAB" ~ "rehabilitation",
    type == "CHILD" ~ "children's",
    type == "PSYCH" ~ "psychiatric",
    type == "RELIGIOUS NON-MED" ~ "religious non med",
    TRUE ~ type
  ))

#the two sheets don't have duplicated hospitals, check with
# intersect(acute$NAME, spec$NAME)

all = bind_rows(
  filt_acute,
  filt_spec
)

all_w_addy = all %>% 
  mutate(location = paste0(address,', ', city,', ', state,', ', zip)) %>% 
  # filter out us territories
  filter(as.numeric(fips) < 57000)

# geocode first with census then missing adresses with OSM
geo <- all_w_addy %>%
  geocode_combine(
    queries = list(
      list(method = "census"),
      list(method = "osm"),
      list(method = "arcgis")
    ),
    global_params = list(address = "location"),
    cascade = TRUE
  )

#make sure nothing missing
geo %>% filter(is.na(lat) | is.na(long)) 


# preparing all data to calculate hospital drive times
hosp_sf <- geo %>% st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Load TIGRIS Counties Boundary Dataset
us_counties <- counties(cb = TRUE, year = 2023, class = "sf") %>% 
  st_transform(crs = 4326) %>% 
  filter(as.numeric(COUNTYFP) < 57)

# Load Census Population Centers Dataset
centers = read.table("https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR.txt", header=TRUE, sep=",")

state = states(cb=TRUE) %>% select(STATEFP, STUSPS)

centers_sf <- centers %>% 
  filter(as.numeric(COUNTYFP) < 57) %>%
  mutate(STATEFP = sprintf("%02d", as.integer(STATEFP))) %>%
  left_join(state, by = c("STATEFP" = "STATEFP")) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% 
  mutate(COUNTYFP = sprintf("%03d", as.integer(COUNTYFP)))
  

# acute hospitals for analysis
geo_acute = hosp_sf %>% 
  filter(type == "acute") %>% 
  rename(STUSPS = state) %>% 
  mutate(COUNTYFP = sprintf("%03d", as.integer(substr(fips, 3, 5)))) %>% 
  mutate(STATEFP = sprintf("%02d", as.integer(substr(fips, 1, 2))))

saveRDS(hosp_sf, "data/outcome/UNC_shep/clean_UNC_hosps_all_2023.rds")
saveRDS(geo_acute, "data/outcome/UNC_shep/clean_UNC_hosps_acute_2023.rds")
saveRDS(us_counties, "data/outcome/census/us_counties_2023.rds")
saveRDS(centers_sf, "data/outcome/census/clean_pop_centroids_2020.rds")
