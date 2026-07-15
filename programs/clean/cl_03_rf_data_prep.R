# make filtered dataset for random forest models, data imputation

library(dplyr)
library(purrr)
library(missRanger)
library(stringr)

dat <- readRDS("data/analysis/clean_ALL_data.rds")
dash = readRDS("shiny_dashboard/dashboard_data.rds")

outcomes <- c(
  "CHR_PCT_MENTAL_DISTRESS",
  "CHR_PCT_LOW_BIRTH_WT",
  "CHR_PCT_ADULT_OBESITY",
  "CDCW_INJURY_DTH_RATE",
  "CDCW_SELFHARM_DTH_RATE",
  "CDCA_STROKE_DTH_RATE_ABOVE35"
)

predictor_topics = c("Characteristics of health care providers", "Characteristics of health care facilities", "Transportation")

additional_vars <- c(
  "USDA_rural_indicator_2013",
  "FCC_res_connections_10_mbps",
  "ACS_MEDIAN_AGE", "ACS_PCT_AIAN", "ACS_PCT_ASIAN", "ACS_PCT_BLACK", "ACS_PCT_HISPANIC", "ACS_PCT_WHITE", "ACS_PCT_POSTHS_ED", "SAHIE_PCT_UNINSURED64"
)

rf_predictors <- dash %>% 
  filter(Topic %in% predictor_topics | Variable.Name %in% additional_vars) %>%
  filter(Global.County.Coverage.Level == "Mostly Full Coverage") %>%
  filter(str_detect(Variable.Name, "RATE") | str_detect(Variable.Name, "PCT") |
           Variable.Name %in% additional_vars) %>% 
  pull(Variable.Name) %>%
  unique()

# Combine outcomes + predictors
rf_vars <- unique(c(outcomes, additional_vars, rf_predictors))

rf_dat <- dat %>% 
  select(YEAR, COUNTYFIPS, all_of(rf_vars)) %>% 
  filter(YEAR > 2009 & YEAR < 2023) %>% 
  filter(as.numeric(COUNTYFIPS) < 57000) # filter out us territories

#some of the variables are entirely missing from a year so we'll remove them
vars_to_remove <- rf_dat %>%
  select(-all_of(outcomes)) %>%           # ignore health outcomes
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), ~ all(is.na(.))), .groups = "drop") %>%
  select(-YEAR) %>%  # don't want to exclude year
  select(where(any)) %>%
  names()

rf_dat <- rf_dat %>%
  select(-all_of(vars_to_remove))

# imputation
imputed <- rf_dat %>%
  arrange(COUNTYFIPS, YEAR) %>%
  mutate(
    COUNTYFIPS = factor(COUNTYFIPS),
    YEAR = factor(YEAR)
  ) %>%
  missRanger(
    pmm.k = 5,
    num.trees = 300,
    maxiter = 5,
    seed = 123,
    verbose = TRUE
  )


saveRDS(
  imputed,
  "data/analysis/random_forest_dat_2010_2022.rds"
)
