library(sf)

health <- st_read("data/outcome/OSM/us_health_deduplicated.geojson")

filetered = health %>% 
  filter(!(is.na(name) & is.na(amenity)))

