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

# function to remove year effects for X
remove_year_effects <- function(train, test, predictors) {
  
  X_train_adj <- train[, predictors, drop = FALSE]
  X_test_adj  <- test[, predictors, drop = FALSE]
  
  for (v in predictors) {
    
    fit <- lm(
      reformulate("YEAR", response = v),
      data = transform(train, YEAR = factor(YEAR))
    )
    
    X_train_adj[[v]] <- residuals(fit)
    
    X_test_adj[[v]] <- test[[v]] -
      predict(
        fit,
        newdata = data.frame(
          YEAR = factor(test$YEAR,
                        levels = levels(factor(train$YEAR)))
        )
      )
  }
  
  list(
    train = as.matrix(X_train_adj),
    test = as.matrix(X_test_adj)
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
  
  
  # y train/test
  y_train <- train_df[[outcome]]
  y_test <- test_df[[outcome]]
  
  
  # remove year effects for y
  year_model <- lm(y_train ~ factor(train_df$YEAR))
  
  # Updated training outcome
  fit_y <- lm(
    outcome ~ YEAR,
    data = data.frame(
      outcome = y_train,
      YEAR = factor(train_df$YEAR)
    )
  )
  
  y_train_updated <- residuals(fit_y)
  
  y_test_updated <- y_test -
    predict(
      fit_y,
      newdata = data.frame(
        YEAR = factor(test_df$YEAR,
                      levels = levels(factor(train_df$YEAR)))
      )
    )
  
 # X train/test and year effects removed
  adj <- remove_year_effects(train_df, test_df, predictors)
  
  X_train_updated <- adj$train
  X_test_updated  <- adj$test
  
  message("data setup done")
  
  # full RF model
  rf_full <- regression_forest(
    X = X_train_updated,
    Y = y_train_updated,
    num.trees = num_trees,
    tune.parameters = tune_setting,
    seed = seed,
    clusters = train_df$COUNTYFIPS
  )
  
  message("full RF done")
  
  # predict w/ full model
  train_pred <- predict(rf_full)$predictions
  test_pred <- predict(rf_full, X_test_updated)$predictions
  
  train_performance <- model_metrics(y_train_updated, train_pred)
  test_performance  <- model_metrics(y_test_updated, test_pred)
  
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
  X_train_selected <- X_train_updated[, top_variables, drop = FALSE]
  X_test_selected  <- X_test_updated[, top_variables, drop = FALSE]
  
  rf_selected <- regression_forest(
    X = X_train_selected,
    Y = y_train_updated,
    num.trees = num_trees,
    tune.parameters = tune_setting,
    seed = seed,
    clusters = train_df$COUNTYFIPS
  )
  
  message("selected RF done")
  
  selected_test_pred <- predict(rf_selected, X_test_selected)$predictions
  selected_train_pred <- predict(rf_selected)$predictions
  
  selected_test_performance <- model_metrics(y_test_updated, selected_test_pred)
  selected_train_performance <- model_metrics(y_train_updated, selected_train_pred)
  
  
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
    observed = y_test_updated,
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
stroke_dth <- rf_model(clean,2010,2021,"CDCA_STROKE_DTH_RATE_ABOVE35", dir="all_params_tuned_plus_year_effects")

self_harm_dth <- rf_model(clean,2010,2023,"CDCW_SELFHARM_DTH_RATE", dir="all_params_tuned_plus_year_effects")

injury_dth <- rf_model(clean,2010,2023,"CDCW_INJURY_DTH_RATE", dir="all_params_tuned_plus_year_effects")

obesity <- rf_model(clean,2010,2017,"CHR_PCT_ADULT_OBESITY", dir="all_params_tuned_plus_year_effects")

low_birth <- rf_model(clean,2010,2014,"CHR_PCT_LOW_BIRTH_WT", dir="all_params_tuned_plus_year_effects")

mental <- rf_model(clean,2014,2022,"CHR_PCT_MENTAL_DISTRESS", dir="all_params_tuned_plus_year_effects")
