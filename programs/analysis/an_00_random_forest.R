# random forest models

library(grf)
library(dplyr)
library(randomForest)
library(purrr)

## data prep
predictors = readRDS("data/analysis/random_forest_predictor_dat_2010_2023.rds")
outcome = readRDS("data/analysis/random_forest_outcome_dat_2010_2023.rds")

# very time consuming, maybe try on ARC
#imputed_pred = mice(pred_filt, method = "rf")

# faster imputation (replace with median)
pred_clean = predictors %>% 
  filter(as.numeric(COUNTYFIPS) < 57000)%>% 
  arrange(YEAR, COUNTYFIPS) %>% 
  mutate(COUNTYFIPS = as.factor(COUNTYFIPS)) %>% 
  group_split(YEAR) %>%
  map_dfr(~ na.roughfix(.x)) %>% 
  arrange(YEAR, COUNTYFIPS)


#some of the variables are entirely missing from a year
vars_to_remove <- pred_clean %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), ~ all(is.na(.))), .groups = "drop") %>%
  # Keep only columns where at least one year returned TRUE (all NA)
  select(where(~ any(. == TRUE))) %>%
  names()

pred_clean <- pred_clean %>%
  select(-all_of(vars_to_remove), -COUNTYFIPS)

# function to build RF model based on passed in health outcome
rf_model <- function(pred_dat, outcome_dat, start_yr = 2010, end_yr = 2023, outcome = "CDCW_crude_death_rate") {
  
  # filter predictor data
  pred_mat = pred_dat %>% 
    filter(YEAR >= start_yr & YEAR <= end_yr) %>% 
    as.matrix()
  
  # extract/clean chosen outcome
  outcome_clean = outcome_dat %>% 
    select(YEAR, COUNTYFIPS, all_of(outcome)) %>% 
    filter(YEAR >= start_yr & YEAR <= end_yr) %>% 
    filter(as.numeric(COUNTYFIPS) < 57000)%>% 
    arrange(YEAR, COUNTYFIPS) %>% 
    mutate(COUNTYFIPS = as.factor(COUNTYFIPS)) %>% 
    group_split(YEAR) %>%
    map_dfr(~ na.roughfix(.x)) %>% 
    arrange(YEAR, COUNTYFIPS)
  
  y <- outcome_clean[[outcome]]
  
  mod = regression_forest(pred_mat, y)
  
  importance <- variable_importance(mod)
  
  importance_df <- data.frame(
    variable = colnames(pred_mat),
    importance = importance
  ) 
  
  pred <- predict(mod)
  
  results <- data.frame(
    observed = y,
    predicted = pred$predictions
  )
  
  
  list(
    model = mod,
    importance = importance_df,
    predictions = results
  )
}



set.seed(67) #reproducibility

# run modles
## dates from data availability dashboard
mortality = rf_model(pred_clean, outcome)
stroke_dth = rf_model(pred_clean, outcome, 2010, 2021, "CDCA_STROKE_DTH_RATE_ABOVE35")
hiv_rate = rf_model(pred_clean, outcome, 2010, 2023, "CDCAP_HIVDIAG_RATE_ABOVE13")
self_harm_dth = rf_model(pred_clean, outcome, 2010, 2023, "CDCW_SELFHARM_DTH_RATE")
injury_dth = rf_model(pred_clean, outcome, 2010, 2023, "CDCW_INJURY_DTH_RATE")
heart_dth = rf_model(pred_clean, outcome, 2010, 2021, "CDCA_HEART_DTH_RATE_ABOVE35")
obesity = rf_model(pred_clean, outcome, 2010, 2017, "CHR_PCT_ADULT_OBESITY")
diabetes = rf_model(pred_clean, outcome, 2010, 2017, "CHR_PCT_DIABETES")
low_birth = rf_model(pred_clean, outcome, 2010, 2014, "CHR_PCT_LOW_BIRTH_WT")
mental = rf_model(pred_clean, outcome, 2014, 2022, "CHR_PCT_MENTAL_DISTRESS")
alc_drv_death = rf_model(pred_clean, outcome, 2012, 2022, "CHR_PCT_ALCOHOL_DRIV_DEATH")

save(
  mortality,
  stroke_dth,
  hiv_rate,
  self_harm_dth,
  injury_dth,
  heart_dth,
  obesity,
  diabetes,
  low_birth,
  mental,
  alc_drv_death,
  file = "random_forest_models.RData"
)
