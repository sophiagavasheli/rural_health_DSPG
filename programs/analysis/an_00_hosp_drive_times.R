# calculating hospital drive times

library(here)
library(tidygeocoder)
library(leaflet)
library(tigris)
library(dplyr)
library(osrm)
library(sf)

cms_geo = read.csv(here("data", "outcome", "CMS", "cms2022_clean.csv"))
datax <- cms_geo %>% st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Get counties shapefile with tigris
cntys <- counties(year = 2022) %>%
  filter(GEOID < 57000)

# Match CRS
cntys <- st_transform(cntys, st_crs(datax))