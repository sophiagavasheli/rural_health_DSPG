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

saveRDS(
    drive_times,
    "data/outcome/OSM/drive_times/us_acute_hosp_drive_times.rds"
)
