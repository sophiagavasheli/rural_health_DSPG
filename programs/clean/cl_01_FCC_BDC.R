# cleaning FCC bdc
# Sophia

library(dplyr)
library(tidyr)
library(here)
library(purrr)

folders <- c(
  "FCC_BDC_2022-12-31",
  "FCC_BDC_2023-12-31",
  "FCC_BDC_2024-12-31",
  "FCC_BDC_2025-12-31"
)

clean_fcc <- function(folder){
  
  message("Processing ", folder)
  
  folder_path <- here("data", "source", "FCC", folder)
  
  # find the files
  fixed_file <- list.files(
    folder_path,
    pattern = "^bdc_us_fixed_broadband_summary_by_geography.*\\.csv$",
    full.names = TRUE
  )
  
  mobile_file <- list.files(
    folder_path,
    pattern = "^bdc_us_mobile_broadband_summary_by_geography.*\\.csv$",
    full.names = TRUE
  )
  
  fixed <- read.csv(fixed_file)
  mobile <- read.csv(mobile_file)
  
  # extract year from folder name
  year <- substr(folder, 8, 12)
  
  # clean fixed
  clean_fixed <- fixed %>%
    filter(geography_type == "County") %>%
    mutate(
      geography_desc_full = trimws(
        sub(".*,", "", geography_desc_full)
      )
    ) %>%
    rename(state = geography_desc_full) %>%
    mutate(
      geography_desc = gsub(" County", "", geography_desc)
    ) %>%
    rename(county = geography_desc) %>%
    filter(area_data_type %in% c("Total", "Urban", "Rural")) %>%
    filter(technology == "Any Technology") %>%
    select(-c(geography_type, technology)) %>%
    rename(GEOID = geography_id) %>%
    mutate(year = year)
  
  # clean mobile
  clean_mobile <- mobile %>%
    filter(geography_type == "County") %>%
    separate(
      geography_desc,
      into = c("county", "state"),
      sep = ", "
    ) %>%
    mutate(
      county = gsub(" County", "", county)
    ) %>%
    filter(area_data_type %in% c("Total", "Urban", "Rural")) %>%
    select(-geography_type) %>%
    rename(GEOID = geography_id) %>%
    mutate(year = year)
  
  # save
  write.csv(
    clean_fixed,
    here(
      "data", "outcome", "FCC",
      paste0("clean_FCC_BDC_fixed_broadband_", year, ".csv")
    ),
    row.names = FALSE
  )
  
  write.csv(
    clean_mobile,
    here(
      "data", "outcome", "FCC",
      paste0("clean_FCC_BDC_mobile_broadband_", year, ".csv")
    ),
    row.names = FALSE
  )
}

#loop
walk(folders, clean_fcc)