# making filtered dataset for dashboard maps

library(dplyr)
library(tigris)
library(sf)

dat <- readRDS("data/analysis/clean_ALL_data.rds")
dash = readRDS("shiny_dashboard/dashboard_data.rds")

cons = counties(year = 2023, cb = TRUE) %>%
  select(GEOID, geometry)

keep_vars = dash %>%
  filter(Year == 2023, Available == 1,
         Yearly.County.Coverage.Level == "Mostly Full Coverage")

map_vars = unique(keep_vars$Variable.Name)

dat_filt = dat %>%
  select(YEAR, COUNTYFIPS, COUNTY, all_of(map_vars)) %>%
  filter(YEAR == 2023) %>%
  mutate(COUNTYFIPS = sprintf("%05d", as.numeric(COUNTYFIPS))) %>%
  left_join(
    cons,
    by = c("COUNTYFIPS" = "GEOID")
  ) %>%
  st_as_sf() %>%
  st_transform(4326)


saveRDS(dat_filt, "shiny_dashboard/clean_2023_filtered_dat.rds")
