# make filtered dataset for dashboard maps and random forest models

library(dplyr)
library(tigris)
library(sf)

dat <- readRDS("shiny_dashboard/clean_ALL_data.rds")
dash = readRDS("shiny_dashboard/dashboard_data.rds")

predictor_topics = c("People", "Income", "Attainment", "Health insurance status", 
           "Characteristics of health care providers", 
           "Characteristics of health care facilities", "Broadband Adoption",
           "Transportation")

# random forest data prep
rf_keep = dash %>% 
  filter(Topic %in% predictor_topics | Variable.Name == "USDA_rural_indicator_2013") %>% 
  filter(Global.County.Coverage.Level == "Mostly Full Coverage")

rf_predictors = unique(rf_keep$Variable.Name)

rf_dat_pred = dat %>% 
  select(YEAR, COUNTYFIPS, rf_predictors) %>% 
  filter(YEAR > 2009 & YEAR < 2022)

saveRDS(rf_dat_pred, "data/random_forest_predictor_dat.rds_2010_2021")

health_outcomes = c("CDCA_STROKE_HOSP_RATE_ABOVE65", "CDCA_STROKE_DTH_RATE_ABOVE35", "CDCAP_HIVDIAG_RATE_ABOVE13", "CDCW_SELFHARM_DTH_RATE", "CDCW_INJURY_DTH_RATE", "CDCW_crude_death_rate", "CDCA_HEART_HOSP_RATE_ABOVE65", "CDCA_HEART_DTH_RATE_ABOVE35", "CHR_PCT_ADULT_OBESITY", "CHR_PCT_LOW_BIRTH_WT", "CHR_PCT_ALCOHOL_DRIV_DEATH", "CHR_PCT_DIABETES")

rf_dat_outcome = dat %>% 
  select(YEAR, COUNTYFIPS, health_outcomes) %>% 
  filter(YEAR > 2009 & YEAR < 2022)

saveRDS(rf_dat_outcome, "data/random_forest_outcome_dat.rds_2010_2021")

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
