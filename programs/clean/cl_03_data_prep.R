# make filtered dataset for dashboard maps and random forest models

library(dplyr)
library(tigris)
library(sf)

dat <- readRDS("shiny_dashboard/clean_ALL_data.rds")
dash = readRDS("shiny_dashboard/dashboard_data.rds")

cons = counties(year == 2023, cb = TRUE) %>% 
  select(GEOID, geometry)

keep_vars = dash %>% 
  filter(Year == 2023, Available == 1, 
         Yearly.County.Coverage.Level == "Mostly Full Coverage") %>% 
  select(Variable.Name, Variable.Label)

dat_filt = dat %>% 
  select(keep_vars$Variable.Name) %>% 
  filter(Year == 2023) %>% 
  mutate(COUNTYFIPS = sprintf("%05d", as.numeric(COUNTYFIPS))) %>% 
  left_join(
    cons,
    by = c("GEOID" = "COUNTYFIPS")
  ) %>% 
  st_transform(4326)


saveRDS(dat_filt, "shiny_dashboard/clean_2023_filtered_dat.rds")
