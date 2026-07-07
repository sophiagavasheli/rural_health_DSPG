# make filtered dataset for dashboard maps and random forest models

library(dplyr)
library(tigris)
library(sf)

dat <- readRDS("data/analysis/clean_ALL_data.rds")
dash = readRDS("shiny_dashboard/dashboard_data.rds")

# random forest data prep
predictor_topics = c("People", "Income", "Attainment", "Health insurance status", 
           "Characteristics of health care providers", 
           "Characteristics of health care facilities", "Broadband Adoption",
           "Transportation")

rf_keep = dash %>% 
  filter(Topic %in% predictor_topics | Variable.Name == "USDA_rural_indicator_2013") %>% 
  filter(Global.County.Coverage.Level == "Mostly Full Coverage")

rf_predictors = unique(rf_keep$Variable.Name)

rf_dat_pred = dat %>% 
  select(YEAR, COUNTYFIPS, all_of(rf_predictors)) %>% 
  select(YEAR, COUNTYFIPS, where(is.numeric)) %>% 
  filter(YEAR > 2009 & YEAR < 2022)

saveRDS(rf_dat_pred, "data/analysis/random_forest_predictor_dat.rds_2010_2021")


rf_dat_outcome = dat %>% 
  select(YEAR, COUNTYFIPS, CDCA_HEART_DTH_RATE_ABOVE35, CDCW_crude_death_rate, CDCW_INJURY_DTH_RATE, CDCW_SELFHARM_DTH_RATE, CDCAP_HIVDIAG_RATE_ABOVE13, CDCA_STROKE_DTH_RATE_ABOVE35) %>% 
  filter(YEAR > 2009 & YEAR < 2022)

saveRDS(rf_dat_outcome, "data/analysis/random_forest_outcome_dat.rds_2010_2021")

# future dashboard map
cons = counties(year = 2023, cb = TRUE) %>%
  select(GEOID, geometry)

keep_vars = dash %>%
  filter(Year == 2023, Available == 1,
         Yearly.County.Coverage.Level == "Mostly Full Coverage")

map_vars = unique(keep_vars$Variable.Name)

dat_filt = dat %>%
  select(YEAR, COUNTYFIPS, COUNTY, map_vars) %>%
  filter(YEAR == 2023) %>%
  mutate(COUNTYFIPS = sprintf("%05d", as.numeric(COUNTYFIPS))) %>%
  left_join(
    cons,
    by = c("COUNTYFIPS" = "GEOID")
  ) %>%
  st_as_sf() %>% 
  st_transform(4326)


saveRDS(dat_filt, "shiny_dashboard/clean_2023_filtered_dat.rds")
