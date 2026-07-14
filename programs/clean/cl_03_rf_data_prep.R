# make filtered dataset for random forest models, data imputation

library(dplyr)
library(purrr)
library(randomForest)

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

predictor_topics = c(#"People", "Income", "Attainment", "Health insurance status",
  "Characteristics of health care providers", "Characteristics of health care facilities", "Transportation")

additional_vars <- c(
  "USDA_rural_indicator_2013",
  "FCC_res_connections_10_mbps"
)

rf_predictors <- dash %>% 
  filter(
    Topic %in% predictor_topics |
      Variable.Name %in% additional_vars) %>%
  filter(
    Global.County.Coverage.Level == "Mostly Full Coverage") %>%
  pull(Variable.Name) %>%
  unique()

# Combine outcomes + predictors
rf_vars <- unique(c(outcomes, additional_vars, rf_predictors))

rf_dat <- dat %>% 
  select(YEAR, COUNTYFIPS, all_of(rf_vars)) %>%
  select(YEAR, COUNTYFIPS, USDA_rural_indicator_2013,
         FCC_res_connections_10_mbps, contains("RATE"), contains("PCT")) %>% 
  filter(YEAR > 2009) %>% 
  filter(as.numeric(COUNTYFIPS) < 57000) # filter out us territories


# imputation (replace with median)
imputed = rf_dat %>% 
  arrange(YEAR, COUNTYFIPS) %>% 
  mutate(COUNTYFIPS = as.factor(COUNTYFIPS)) %>% 
  group_split(YEAR) %>%
  map_dfr(~ na.roughfix(.x))

#some of the variables are entirely missing from a year
vars_to_remove <- imputed %>%
  select(-all_of(outcomes)) %>%           # ignore health outcomes
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), ~ all(is.na(.))), .groups = "drop") %>%
  select(-YEAR) %>%  # don't want to exclude year
  select(where(any)) %>%
  names()

clean <- imputed %>%
  select(-all_of(vars_to_remove))

saveRDS(
  clean,
  "data/analysis/random_forest_dat_2010_2023.rds"
)
