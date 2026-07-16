# joining 2023 drive times to the 2023 final dataset for an RF model

library(dplyr)
library(tidyr)
library(stringr)
library(missRanger)

drv = readRDS("data/analysis/health_site_drive_times_2023.rds")
dat = readRDS("data/analysis/clean_ALL_data.rds")
dash = readRDS("shiny_dashboard/dashboard_data.rds")

outcomes <- c(
  "CDCW_DRUG_DTH_RATE",
  "CHR_PCT_LOW_BIRTH_WT",
  "CDCW_INJURY_DTH_RATE",
  "CDCW_SELFHARM_DTH_RATE",
  "CDCW_crude_death_rate"
)

# clean drive data
drv_clean = drv %>% 
  select(GEOID, health_site_type, avg_drive_time_minutes, max_drive_time_minutes, min_drive_time_minutes) %>% 
  pivot_wider(
    names_from = health_site_type,
    values_from = c(
      avg_drive_time_minutes,
      max_drive_time_minutes,
      min_drive_time_minutes
    ),
    names_glue = "{health_site_type}_{.value}"
  )

# some AK and HI counties are missing drive time, maybe because they're disconnected island counties
# we'll remove from analysis
remove_counties = drv_clean %>% 
  filter(if_any(everything(), is.na)) %>%
  select(GEOID) %>% 
  pull()
 

# data from final dataset
predictor_topics = c("Characteristics of health care providers", "Characteristics of health care facilities", "Transportation")

additional_vars <- c(
  "USDA_rural_indicator_2013",
  "FCC_res_connections_10_mbps",
  "ACS_MEDIAN_AGE", "ACS_PCT_AIAN", "ACS_PCT_ASIAN", "ACS_PCT_BLACK", 
  "ACS_PCT_HISPANIC", "ACS_PCT_WHITE", "ACS_PCT_POSTHS_ED", "ACS_PCT_UNINSURED"
  
)

rf_predictors <- dash %>% 
  filter(Topic %in% predictor_topics | Variable.Name %in% additional_vars) %>%
  filter(Year == 2023) %>% 
  filter(Yearly.County.Coverage.Level == "Mostly Full Coverage") %>%
  filter(str_detect(Variable.Name, "RATE") | str_detect(Variable.Name, "PCT") |
           Variable.Name %in% additional_vars) %>% 
  pull(Variable.Name) %>%
  unique()

# Combine outcomes + predictors
rf_vars <- unique(c(outcomes, additional_vars, rf_predictors))

rf_dat <- dat %>% 
  select(YEAR, COUNTYFIPS, all_of(rf_vars)) %>% 
  filter(YEAR == 2023) %>% 
  filter(as.numeric(COUNTYFIPS) < 57000) %>% # filter out us territories
  filter(!COUNTYFIPS %in% remove_counties) %>% 
  select(-YEAR)

# imputation
imputed <- rf_dat %>%
  arrange(COUNTYFIPS) %>%
  mutate(COUNTYFIPS = factor(COUNTYFIPS)) %>%
  missRanger(
    pmm.k = 5,
    num.trees = 300,
    maxiter = 5,
    seed = 123,
    verbose = TRUE,
    data_only = TRUE
  ) 
  

saveRDS(
  imputed,
  "data/analysis/rf_drive_time_dat_2023.rds"
)