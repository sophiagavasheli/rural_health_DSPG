# making filtered dataset for dashboard map

library(dplyr)
library(tigris)
library(sf)
library(tidyr)

dat <- readRDS("data/analysis/clean_ALL_data.rds")
dash <- readRDS("shiny_dashboard/dashboard_data.rds")

topics = c("Characteristics of health care providers", "Characteristics of health care facilities", "Transportation", "Broadband Adoption", "Health outcomes")

counties_sf <- counties(year = 2023, cb = TRUE) %>%
  select(GEOID, geometry)

# variables that are available by year
available_vars <- dash %>%
  filter(
    Topic %in% topics,
    Available == 1,
    Yearly.County.Coverage.Level == "Mostly Full Coverage",
    Data.Type == "num") %>%
  select(Year, Variable.Name, Variable.Label)

vars = unique(available_vars$Variable.Name)

map_dat <- dat %>%
  select(YEAR, COUNTYFIPS, COUNTY, all_of(vars)) %>% 
  mutate(
    COUNTYFIPS = sprintf("%05d", as.numeric(COUNTYFIPS))
  ) %>%
  left_join(
    counties_sf,
    by = c("COUNTYFIPS" = "GEOID")
  ) %>%
  st_as_sf() %>%
  st_transform(4326)

saveRDS(map_dat, "shiny_dashboard/map_dat.rds")
saveRDS(available_vars, "shiny_dashboard/available_health_inf_vars.rds")