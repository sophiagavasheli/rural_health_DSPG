# combine state level drive times

library(dplyr)
library(purrr)

files <- list.files(
    "data/outcome/OSM/drive_times",
    pattern="*.rds",
    full.names=TRUE
)

drive_times <- map_dfr(
    files,
    readRDS
)

#filter out the connecticut planning regions, using the older counties instead
final = drive_times %>% 
  filter(LSAD != "PL") 

saveRDS(
  final,
    "data/outcome/OSM/us_acute_hosp_drive_times_2023.rds"
)

saveRDS(
  final,
  "shiny_dashboard/us_acute_hosp_drive_times_2023.rds"
)