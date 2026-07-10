# random forest models

library(grf)
library(dplyr)
library(randomForest)
library(tibble)
library(purrr)

dat = readRDS("data/analysis/random_forest_dat_2010_2023.rds")

# health outcomes
outcomes = c("CHR_PCT_MENTAL_DISTRESS", "CHR_PCT_LOW_BIRTH_WT", "CHR_PCT_ADULT_OBESITY", "CDCW_INJURY_DTH_RATE", "CDCW_SELFHARM_DTH_RATE",  "CDCA_STROKE_DTH_RATE_ABOVE35")

# imputation (replace with median)
imputed = dat %>% 
  filter(as.numeric(COUNTYFIPS) < 57000)%>% 
  arrange(YEAR, COUNTYFIPS) %>% 
  mutate(COUNTYFIPS = as.factor(COUNTYFIPS)) %>% 
  group_split(YEAR) %>%
  map_dfr(~ na.roughfix(.x)) %>% 
  arrange(YEAR, COUNTYFIPS)


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

not_predictors = c("CHR_PCT_MENTAL_DISTRESS", "CHR_PCT_LOW_BIRTH_WT", "CHR_PCT_ADULT_OBESITY", "CDCW_INJURY_DTH_RATE", "CDCW_SELFHARM_DTH_RATE",  "CDCA_STROKE_DTH_RATE_ABOVE35", "YEAR", "COUNTYFIPS")

predictors <- setdiff(names(clean), not_predictors)


# model performance helper
model_metrics <- function(observed, predicted) {
  tibble(
    RMSE = sqrt(mean((observed - predicted)^2, na.rm = TRUE)),
    MAE  = mean(abs(observed - predicted), na.rm = TRUE),
    R2   = cor(observed, predicted, use = "complete.obs")^2
  )
}


# function to build RF model based on passed in health outcome
rf_model <- function(clean, start_yr, end_yr, outcome, top_n = 10, num_trees = 2000, tune_setting = "all", dir) {
  
  clean = clean %>% 
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
  
  message("Training counties: ", length(unique(train_df$COUNTYFIPS)))
  message("Testing counties: ", length(unique(test_df$COUNTYFIPS)))
  message("Training rows: ", nrow(train_df))
  message("Testing rows: ", nrow(test_df))
  
  
  X_train <- train_df %>%
    select(all_of(predictors)) %>%
    as.matrix()
  
  y_train <- train_df[[outcome]]
  
  X_test <- test_df %>%
    select(all_of(predictors)) %>%
    as.matrix()
  
  y_test <- test_df[[outcome]]
  
  message("X_train dimensions: ", paste(dim(X_train), collapse = " x "))
  message("X_test dimensions: ", paste(dim(X_test), collapse = " x "))
  
  # full model
  rf_full <- regression_forest(
    X = X_train,
    Y = y_train,
    num.trees = num_trees,
    tune.parameters = tune_setting,
    seed = seed
  )
  
  # predict w/ full model
  train_pred <- predict(rf_full)$predictions
  test_pred <- predict(rf_full, X_test)$predictions
  
  train_performance <- model_metrics(y_train, train_pred)
  test_performance  <- model_metrics(y_test, test_pred)
  
  message("Training performance:")
  print(train_performance)
  
  message("Testing performance:")
  print(test_performance)
  
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
    seed = seed
  )
  
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
  
  save(
    rf_full,
    rf_selected,
    performance_summary,
    importance_df,
    top_variables,
    predictions_output,
    file = paste0("data/output/", outcome, "_grf_results.RData")
  )
  
} 


# run models
## dates from data availability dashboard
stroke_dth <- rf_model(clean,2010,2021,"CDCA_STROKE_DTH_RATE_ABOVE35", "all_params_tuned")

self_harm_dth <- rf_model(clean,2010,2023,"CDCW_SELFHARM_DTH_RATE", "all_params_tuned")

injury_dth <- rf_model(clean,2010,2023,"CDCW_INJURY_DTH_RATE", "all_params_tuned")

obesity <- rf_model(clean,2010,2017,"CHR_PCT_ADULT_OBESITY", "all_params_tuned")

low_birth <- rf_model(clean,2010,2014,"CHR_PCT_LOW_BIRTH_WT", "all_params_tuned")

mental <- rf_model(clean,2014,2022,"CHR_PCT_MENTAL_DISTRESS", "all_params_tuned")
