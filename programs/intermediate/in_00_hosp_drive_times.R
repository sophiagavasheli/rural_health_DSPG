# calculating hospital drive times

library(here)
library(leaflet)
library(tigris)
library(dplyr)
library(osrm)
library(sf)

# hospitals
hosp = read.csv(here("data", "outcome", "CMS", "cms2022_clean.csv"))
hosp_sf <- hosp %>% st_as_sf(coords = c('long', 'lat'), crs = 4326)

# pop centers
centers = read.table("https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR.txt", header=TRUE, sep=",")

centers = centers %>% 
  filter(COUNTYFP < 57) %>% 
  rename_with(tolower)

centers_sf = centers %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

dist_table = osrmTable(src = centers_sf, dst = hosp_sf)


