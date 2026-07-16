# RF model for 2023 data with VSURF

library(grf)
library(dplyr)
library(tibble)
library(purrr)
library(VSURF)

# get num cores
ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))

if (is.na(ncores) || ncores < 1) {
  ncores <- parallel::detectCores(logical = FALSE)
}

dat_w_dem = readRDS("data/analysis/rf_drive_time_dat_2023.rds")

dat_wo_dem = dat_w_dem %>% 
  select(-c("ACS_MEDIAN_AGE", "ACS_PCT_AIAN", "ACS_PCT_ASIAN", "ACS_PCT_BLACK", 
            "ACS_PCT_HISPANIC", "ACS_PCT_WHITE", "ACS_PCT_POSTHS_ED", "ACS_PCT_UNINSURED"))

# health outcomes
outcomes <- c(
  "CDCW_DRUG_DTH_RATE",
  "CHR_PCT_LOW_BIRTH_WT",
  "CDCW_INJURY_DTH_RATE",
  "CDCW_SELFHARM_DTH_RATE",
  "CDCW_crude_death_rate"
)


not_predictors = c("CDCW_DRUG_DTH_RATE", "CHR_PCT_LOW_BIRTH_WT",
                   "CDCW_INJURY_DTH_RATE","CDCW_SELFHARM_DTH_RATE",
                   "CDCW_crude_death_rate", "COUNTYFIPS")

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
rf_model <- function(clean, predictors, outcome, num_trees = 2000, tune_setting = "all", dir) {
  
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
  
  # feature selection
  vsurf_fit <- VSURF(
    x = X_train,
    y = y_train,
    parallel = TRUE,
    ncores = ncores,
    ntree.pred = 500, # default = 2000, took too long
    clusterType = "FORK"
  )
  
  message("feature selection done")
  
  summary(vsurf_fit)
  
  # vars for interpretation
  interp_vars <- colnames(X_train)[vsurf_fit$varselect.interp]
  
  # vars for prediction
  pred_vars <- colnames(X_train)[vsurf_fit$varselect.pred]
  
  # variable importance
  imp <- data.frame(
    variable = colnames(X_train)[vsurf_fit$imp.mean.dec.ind],
    importance = vsurf_fit$imp.mean.dec,
    sd = vsurf_fit$imp.sd.dec
  )
  
  # selected model
  X_train_selected <- X_train[, pred_vars, drop = FALSE]
  X_test_selected  <- X_test[, pred_vars, drop = FALSE]
  
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
    imp,
    interp_vars,
    pred_vars,
    predictions_output,
    file = file.path(path, paste0(outcome, "_vsurf_results.RData"))  
  )
  
} 


# run models
name = "drive_vsurf_w_dem"
drug = rf_model(dat_w_dem, predictors_dem, "CDCW_DRUG_DTH_RATE", dir = name)
birth = rf_model(dat_w_dem, predictors_dem, "CHR_PCT_LOW_BIRTH_WT", dir = name)
injury = rf_model(dat_w_dem, predictors_dem, "CDCW_INJURY_DTH_RATE", dir = name)
self_harm = rf_model(dat_w_dem, predictors_dem, "CDCW_SELFHARM_DTH_RATE", dir = name)
mort = rf_model(dat_w_dem, predictors_dem, "CDCW_crude_death_rate", dir = name)

message("w demographics models done")

name2 = "drive_vsurf_wo_dem"
drug = rf_model(dat_wo_dem, predictors_wo_dem, "CDCW_DRUG_DTH_RATE", dir = name2)
birth = rf_model(dat_wo_dem, predictors_wo_dem, "CHR_PCT_LOW_BIRTH_WT", dir = name2)
injury = rf_model(dat_wo_dem, predictors_wo_dem, "CDCW_INJURY_DTH_RATE", dir = name2)
self_harm = rf_model(dat_wo_dem, predictors_wo_dem, "CDCW_SELFHARM_DTH_RATE", dir = name2)
mort = rf_model(dat_wo_dem, predictors_wo_dem, "CDCW_crude_death_rate", dir = name2)

