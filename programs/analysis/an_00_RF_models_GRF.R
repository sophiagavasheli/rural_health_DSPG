# random forest models with grf package

library(grf)
library(dplyr)
library(tibble)

# data with demographic variables
dat_w_dem = readRDS("data/analysis/random_forest_dat_2010_2022.rds")

# without demographics
dat_wo_dem = dat_w_dem %>% 
  select(-c("ACS_MEDIAN_AGE", "ACS_PCT_AIAN", "ACS_PCT_ASIAN", "ACS_PCT_BLACK", "ACS_PCT_HISPANIC", "ACS_PCT_WHITE", "ACS_PCT_POSTHS_ED", "SAHIE_PCT_UNINSURED64"))

not_predictors = c("CHR_PCT_MENTAL_DISTRESS", "CHR_PCT_LOW_BIRTH_WT", "CHR_PCT_ADULT_OBESITY", "CDCW_INJURY_DTH_RATE", "CDCW_SELFHARM_DTH_RATE",  "CDCA_STROKE_DTH_RATE_ABOVE35", "COUNTYFIPS")

predictors_dem <- setdiff(names(dat_w_dem), not_predictors)
predictors_wo_dem <- setdiff(names(dat_wo_dem), not_predictors)

# model performance helper
model_metrics <- function(observed, predicted) {
  tibble(
    RMSE = sqrt(mean((observed - predicted)^2, na.rm = TRUE)),
    MAE  = mean(abs(observed - predicted), na.rm = TRUE),
    R2   = cor(observed, predicted, use = "complete.obs")^2
  )
}


# function to build RF model based on passed in health outcome
rf_model <- function(data, predictors, start_yr, end_yr, outcome, top_n = 10, num_trees = 2000, tune_setting = "all", dir) {
  
  clean = data %>% 
    mutate(YEAR = as.numeric(as.character(YEAR)),
           COUNTYFIPS = as.numeric(as.character(COUNTYFIPS))
           ) %>% 
    filter(YEAR >= start_yr & YEAR <= end_yr)
  
  
  # County-level 70/30 train/test split
  train_share <- 0.70
  seed <- 67 # reproducibility
  
  set.seed(seed)
  
  county_ids <- unique(clean$COUNTYFIPS)
  
  train_counties <- sample(
    county_ids,
    size = floor(train_share * length(county_ids))
  )
  
  test_counties <- setdiff(county_ids, train_counties)
  
  train_df <- clean %>%
    filter(COUNTYFIPS %in% train_counties)
  
  test_df <- clean %>%
    filter(COUNTYFIPS %in% test_counties)
  
  # Check that no county appears in both training and testing.
  overlap_counties <- intersect(unique(train_df$COUNTYFIPS), unique(test_df$COUNTYFIPS))
  stopifnot(length(overlap_counties) == 0)
  
  # y
  y_train <- train_df[[outcome]]
  
  y_test <- test_df[[outcome]]
  
  # X
  xtrain_dat <- train_df %>%
    select(all_of(predictors)) 
  
  xtest_dat <- test_df %>%
    select(all_of(predictors)) 
  
  # convert years to dummy variables to capture year effects
  xtrain_dat$YEAR <- as.factor(xtrain_dat$YEAR)
  xtest_dat$YEAR  <- as.factor(xtest_dat$YEAR)
  
  X_train <- model.matrix(~ . - 1, data = xtrain_dat)
  X_test <- model.matrix(~ . - 1, data = xtest_dat)

  message("data setup done")
  
  # full RF model
  rf_full <- regression_forest(
    X = X_train,
    Y = y_train,
    num.trees = num_trees,
    tune.parameters = tune_setting,
    seed = seed,
    clusters = train_df$COUNTYFIPS
  )
  
  message("full RF done")
  
  # predict w/ full model
  train_pred <- predict(rf_full)$predictions
  test_pred <- predict(rf_full, X_test)$predictions
  
  train_performance <- model_metrics(y_train, train_pred)
  test_performance  <- model_metrics(y_test, test_pred)
  
  importance_df <- tibble(
    variable = colnames(X_train),
    importance = variable_importance(rf_full)
  ) %>%
    arrange(desc(importance))
  
  top_variables <- importance_df %>%
    slice_head(n = top_n) %>%
    pull(variable)
  
  message("Top variables:")
  print(top_variables)
  
  # refit with important vars
  X_train_selected <- X_train[, top_variables, drop = FALSE]
  X_test_selected  <- X_test[, top_variables, drop = FALSE]
  
  rf_selected <- regression_forest(
    X = X_train_selected,
    Y = y_train,
    num.trees = num_trees,
    tune.parameters = tune_setting,
    seed = seed,
    clusters = train_df$COUNTYFIPS
  )
  
  message("selected RF done")
  
  selected_test_pred <- predict(rf_selected, X_test_selected)$predictions
  selected_train_pred <- predict(rf_selected)$predictions
  
  selected_test_performance <- model_metrics(y_test, selected_test_pred)
  selected_train_performance <- model_metrics(y_train, selected_train_pred)
  
  
  performance_summary <- bind_rows(
    full_model_test = test_performance,
    full_model_train = train_performance,
    selected_model_test = selected_test_performance,
    selected_model_train = selected_train_performance,
    .id = "model"
  )
  
  performance_summary = performance_summary %>% 
    mutate(health_outcome = outcome)
  
  predictions_output <- tibble(
    YEAR = test_df$YEAR,
    COUNTYFIPS = test_df$COUNTYFIPS,
    observed = y_test,
    predicted_full_model = test_pred,
    predicted_selected_model = selected_test_pred
  )
  
  # Create output folder if needed.
  path = paste0("data/output/", dir)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  #parameter vals
  tuned_params_full <- rf_full$tunable.params
  tuned_params_selected <- rf_selected$tunable.params
  
  save(
    rf_full,
    rf_selected,
    tuned_params_full,
    tuned_params_selected,
    performance_summary,
    importance_df,
    top_variables,
    predictions_output,
    file = file.path(path, paste0(outcome, "_grf_results.RData"))  
    )
  
} 


# run models
## dates from data availability dashboard

name = "with_demographics_year_dummies" #folder

stroke_dth <- rf_model(dat_w_dem, predictors_dem,2010,2021,"CDCA_STROKE_DTH_RATE_ABOVE35", dir = name)
self_harm_dth <- rf_model(dat_w_dem, predictors_dem,2010,2022,"CDCW_SELFHARM_DTH_RATE", dir = name)
injury_dth <- rf_model(dat_w_dem, predictors_dem,2010,2022,"CDCW_INJURY_DTH_RATE", dir = name)
obesity <- rf_model(dat_w_dem, predictors_dem,2010,2017,"CHR_PCT_ADULT_OBESITY", dir = name)
low_birth <- rf_model(dat_w_dem, predictors_dem,2010,2014,"CHR_PCT_LOW_BIRTH_WT", dir = name)
mental <- rf_model(dat_w_dem, predictors_dem,2014,2022,"CHR_PCT_MENTAL_DISTRESS", dir = name)


name = "without_demographics_year_dummies"

stroke_dth <- rf_model(dat_wo_dem, predictors_wo_dem,2010,2021,"CDCA_STROKE_DTH_RATE_ABOVE35", dir = name)
self_harm_dth <- rf_model(dat_wo_dem, predictors_wo_dem,2010,2022,"CDCW_SELFHARM_DTH_RATE", dir = name)
injury_dth <- rf_model(dat_wo_dem, predictors_wo_dem,2010,2022,"CDCW_INJURY_DTH_RATE", dir = name)
obesity <- rf_model(dat_wo_dem, predictors_wo_dem,2010,2017,"CHR_PCT_ADULT_OBESITY", dir = name)
low_birth <- rf_model(dat_wo_dem, predictors_wo_dem,2010,2014,"CHR_PCT_LOW_BIRTH_WT", dir = name)
mental <- rf_model(dat_wo_dem, predictors_wo_dem,2014,2022,"CHR_PCT_MENTAL_DISTRESS", dir = name)
