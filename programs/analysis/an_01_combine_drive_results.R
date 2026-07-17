# combine state level drive times
library(dplyr)
library(purrr)
library(sf)
library(rmapshaper)

files <- list.files(
  "data/outcome/OSM/drive_times",
  pattern="*.rds",
  full.names=TRUE
)
drive_times <- map_dfr(
  files,
  readRDS
)

# filter out the connecticut planning regions, using the older counties instead
final = drive_times %>% 
  filter(LSAD != "PL")

final <- st_transform(final, 4326)

# simplify polygons to cut down on file size
final <- ms_simplify(final, keep = 0.05, keep_shapes = TRUE)

saveRDS(
  final,
  "data/analysis/health_site_drive_times_2023.rds"
)
saveRDS(
  final,
  "shiny_dashboard/health_site_drive_times_2023.rds"
)