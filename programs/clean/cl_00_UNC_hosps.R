# cleaning/geocoding UNC hospital list 2023

library(readxl)
library(dplyr)
library(sf)
library(tidygeocoder)
library(purrr)

process_hospitals <- function(year) {
  
  file <- paste0("data/source/UNC_shep/Hospital-List", year, ".xlsx")
  
  acute <- read_excel(file, sheet = "ACUTE")
  
  spec <- read_excel(file, sheet = "SPECIALTY")
  
  filt_acute <- acute %>%
    select(ID, NAME, ADDRESS, CITY, STATE, ZIP, FIPS, `POS TOTAL BEDS`) %>%
    rename(pos_total_beds = `POS TOTAL BEDS`) %>%
    rename_with(tolower) %>%
    mutate(
      type = "acute",
      year = year
    )
  
  filt_spec <- spec %>%
    select(ID, NAME, ADDRESS, CITY, STATE, ZIP, FIPS, `POS TOTAL BEDS`, TYPE) %>%
    rename(pos_total_beds = `POS TOTAL BEDS`) %>%
    rename_with(tolower) %>%
    mutate(
      type = case_when(
        type == "LTACH" ~ "long term care",
        type == "REHAB" ~ "rehabilitation",
        type == "CHILD" ~ "children's",
        type == "PSYCH" ~ "psychiatric",
        type == "RELIGIOUS NON-MED" ~ "religious non med",
        TRUE ~ type
      ),
      year = year
    )
  
  all <- bind_rows(filt_acute, filt_spec)
  
  all_w_addy <- all %>%
    mutate(location = paste(address, city, state, zip, sep = ", ")) %>%
    filter(as.numeric(fips) < 57000)
  
  # Geocode
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
  
  hosp_sf <- st_as_sf(geo, coords = c("long", "lat"), crs = 4326) 
  
  list(
    all = hosp_sf,
  )
}

years <- 2019:2023

results <- map(years, process_hospitals)

all_hospitals <- map(results, "all") %>%
  bind_rows()

saveRDS(
  all_hospitals,
  "data/outcome/UNC_shep/clean_UNC_hosps_all_2019_2023.rds"
)