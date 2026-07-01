library(dplyr)
library(purrr)

files <- list.files(
    "results",
    pattern="*.rds",
    full.names=TRUE
)

drive_times <- map_dfr(
    files,
    readRDS
)

saveRDS(
    drive_times,
    "us_drive_times.rds"
)

write.csv(
    drive_times,
    "us_drive_times.csv",
    row.names=FALSE
)