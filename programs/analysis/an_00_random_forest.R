# random forest models

library(grf)
library(dplyr)
library(randomForest)
library(purrr)

# data prep
predictors = readRDS("data/random_forest_predictor_dat.rds_2010_2021")
outcome = readRDS("data/random_forest_outcome_dat.rds_2010_2021")

# very time consuming, maybe try on ARC
# library(mice)
#imputed_pred = mice(pred_filt, method = "rf")

# faster imputation (replace with median)
pred_clean = predictors %>% 
  filter(as.numeric(COUNTYFIPS) < 57000)%>% 
  arrange(YEAR, COUNTYFIPS) %>% 
  mutate(COUNTYFIPS = as.factor(COUNTYFIPS)) %>% 
  group_split(YEAR) %>%
  map_dfr(~ na.roughfix(.x))

outcome_clean = outcome %>% 
  select(YEAR, COUNTYFIPS, CDCA_HEART_DTH_RATE_ABOVE35, CDCW_crude_death_rate, CDCW_INJURY_DTH_RATE, CDCW_SELFHARM_DTH_RATE, CDCAP_HIVDIAG_RATE_ABOVE13, CDCA_STROKE_DTH_RATE_ABOVE35) %>% 
  filter(as.numeric(COUNTYFIPS) < 57000) %>% 
  arrange(YEAR, COUNTYFIPS) %>% 
  mutate(COUNTYFIPS = as.factor(COUNTYFIPS)) %>% 
  group_split(YEAR) %>%
  map_dfr(~ na.roughfix(.x))






