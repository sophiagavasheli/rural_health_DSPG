# random forest model with 2023 data, including drive times

library(grf)
library(dplyr)
library(randomForest)
library(tibble)
library(purrr)

dat = readRDS("data/analysis/rf_drive_time_dat_2023.rds")

# health outcomes
outcomes <- c(
  "CDCW_DRUG_DTH_RATE",
  "CHR_PCT_LOW_BIRTH_WT",
  "CDCW_INJURY_DTH_RATE",
  "CDCW_SELFHARM_DTH_RATE",
  "CDCW_crude_death_rate"
)

# imputation (replace with median)
imputed = dat %>% 
  arrange(COUNTYFIPS) %>% 
  mutate(COUNTYFIPS = as.factor(COUNTYFIPS)) %>% 
  na.roughfix()


not_predictors = c("CDCW_DRUG_DTH_RATE", "CHR_PCT_LOW_BIRTH_WT",
                   "CDCW_INJURY_DTH_RATE","CDCW_SELFHARM_DTH_RATE",
                   "CDCW_crude_death_rate", "COUNTYFIPS", "YEAR")

predictors <- setdiff(names(imputed), not_predictors)


# model performance helper
model_metrics <- function(observed, predicted) {
  tibble(
    RMSE = sqrt(mean((observed - predicted)^2, na.rm = TRUE)),
    MAE  = mean(abs(observed - predicted), na.rm = TRUE),
    R2   = cor(observed, predicted, use = "complete.obs")^2
  )
}


# function to build RF model based on passed in health outcome
rf_model <- function(clean, outcome, top_n = 10, num_trees = 2000, tune_setting = "all", dir) {
  
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
  
  X_train <- train_df %>%
    select(all_of(predictors)) %>%
    as.matrix()
  
  y_train <- train_df[[outcome]]
  
  X_test <- test_df %>%
    select(all_of(predictors)) %>%
    as.matrix()
  
  y_test <- test_df[[outcome]]
  
  
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
    variable = predictors,
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
drug = rf_model(imputed, "CDCW_DRUG_DTH_RATE", dir = "drive_time_model")
birth = rf_model(imputed, "CHR_PCT_LOW_BIRTH_WT", dir = "drive_time_model")
injury = rf_model(imputed, "CDCW_INJURY_DTH_RATE", dir = "drive_time_model")
self_harm = rf_model(imputed, "CDCW_SELFHARM_DTH_RATE", dir = "drive_time_model")
mort = rf_model(imputed, "CDCW_crude_death_rate", dir = "drive_time_model")
