# ============================================================
# GRF County-Level Prediction and Variable Importance Tutorial
# Project: Rural Infrastructure and Health Outcomes
#
# Purpose:
#   1. Train a GRF prediction model for a selected health outcome
#   2. Evaluate predictive performance using unseen counties
#   3. Rank variable importance
#   4. Refit a reduced model using the most important variables
#
# Main rule:
#   Merge first, clean second, split counties third, train only on
#   training counties, and evaluate only on testing counties.
# ============================================================


# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(grf)
library(dplyr)
library(randomForest)  # for na.roughfix()
library(tibble)


# ------------------------------------------------------------
# 2. Load data
# ------------------------------------------------------------

# Predictor data should contain YEAR, COUNTYFIPS, and candidate predictors.
predictors <- readRDS("data/analysis/random_forest_predictor_dat_2010_2023.rds")

# Outcome data should contain YEAR, COUNTYFIPS, and health outcome variables.
outcomes <- readRDS("data/analysis/random_forest_outcome_dat_2010_2023.rds")


# ------------------------------------------------------------
# 3. Set editable options
# ------------------------------------------------------------

# Health outcome to predict. Intern should change this for different outcomes.
outcome_name <- "CDCW_crude_death_rate"

# Study years. Choose based on outcome and predictor availability.
start_yr <- 2010
end_yr   <- 2023

# County-level train/test split.
# 0.70 means 70% of counties are used for training and 30% for testing.
train_share <- 0.70

# Number of important variables to keep for the reduced model.
top_n <- 10

# Number of trees in the GRF model.
# More trees give more stable predictions and variable importance,
# but increase runtime. 2000 is a good default for final models.
num_trees <- 2000

# Reproducibility seed.
seed <- 67

# Whether to let GRF tune model-building parameters automatically.
# Use "none" or FALSE for fast debugging; use "all" for final analysis.
tune_setting <- "all"


# ------------------------------------------------------------
# 4. Define helper function: model performance
# ------------------------------------------------------------

model_metrics <- function(observed, predicted) {
  tibble(
    RMSE = sqrt(mean((observed - predicted)^2, na.rm = TRUE)),
    MAE  = mean(abs(observed - predicted), na.rm = TRUE),
    R2   = cor(observed, predicted, use = "complete.obs")^2
  )
}

# Interpretation:
#   RMSE: Root Mean Squared Error. Lower is better.
#         Larger prediction errors are penalized more heavily.
#   MAE:  Mean Absolute Error. Lower is better.
#         Easier to explain because it is average absolute error.
#   R2:   Squared correlation between observed and predicted values.
#         Higher is better. It measures how well predictions track outcomes.


# ------------------------------------------------------------
# 5. Merge predictors and outcome
# ------------------------------------------------------------

outcome_clean <- outcomes %>%
  select(YEAR, COUNTYFIPS, all_of(outcome_name))

# Important: merge before cleaning so predictor rows and outcome rows stay aligned.
model_df <- predictors %>%
  left_join(outcome_clean, by = c("YEAR", "COUNTYFIPS"))


# ------------------------------------------------------------
# 6. Clean data
# ------------------------------------------------------------

model_df <- model_df %>%
  filter(YEAR >= start_yr,
         YEAR <= end_yr) %>%
  filter(as.numeric(COUNTYFIPS) < 57000) %>%
  filter(!is.na(.data[[outcome_name]])) %>%
  arrange(YEAR, COUNTYFIPS)

# Median/mode imputation by year.
# na.roughfix() replaces numeric missing values with medians and factor missing
# values with the most common category. We apply it within each year.
model_df <- model_df %>%
  group_by(YEAR) %>%
  group_modify(~ as.data.frame(na.roughfix(.x))) %>%
  ungroup()

# Optional check: how many rows and counties remain?
message("Rows after cleaning: ", nrow(model_df))
message("Counties after cleaning: ", length(unique(model_df$COUNTYFIPS)))


# ------------------------------------------------------------
# 7. Define predictor variables
# ------------------------------------------------------------

id_vars <- c("YEAR", "COUNTYFIPS")

predictor_names <- model_df %>%
  select(-all_of(id_vars), -all_of(outcome_name)) %>%
  select(where(is.numeric)) %>%
  names()

# Optional: manually remove variables that should not be used as predictors.
# Example:
# predictor_names <- setdiff(predictor_names, c("bad_variable_1", "bad_variable_2"))

message("Number of predictors: ", length(predictor_names))


# ------------------------------------------------------------
# 8. County-level 70/30 train/test split
# ------------------------------------------------------------

# Why split by county?
#   This is panel data: counties are observed over multiple years.
#   If we split by row, the same county may appear in both train and test sets.
#   That can make test performance look too optimistic.
#   A county-level split keeps every year of a county together.

set.seed(seed)

county_ids <- unique(model_df$COUNTYFIPS)

train_counties <- sample(
  county_ids,
  size = floor(train_share * length(county_ids))
)

test_counties <- setdiff(county_ids, train_counties)

train_df <- model_df %>%
  filter(COUNTYFIPS %in% train_counties)

test_df <- model_df %>%
  filter(COUNTYFIPS %in% test_counties)

# Check that no county appears in both training and testing.
overlap_counties <- intersect(unique(train_df$COUNTYFIPS), unique(test_df$COUNTYFIPS))
stopifnot(length(overlap_counties) == 0)

message("Training counties: ", length(unique(train_df$COUNTYFIPS)))
message("Testing counties: ", length(unique(test_df$COUNTYFIPS)))
message("Training rows: ", nrow(train_df))
message("Testing rows: ", nrow(test_df))


# ------------------------------------------------------------
# 9. Create X and Y matrices
# ------------------------------------------------------------

X_train <- train_df %>%
  select(all_of(predictor_names)) %>%
  as.matrix()

y_train <- train_df[[outcome_name]]

X_test <- test_df %>%
  select(all_of(predictor_names)) %>%
  as.matrix()

y_test <- test_df[[outcome_name]]

# Check dimensions.
message("X_train dimensions: ", paste(dim(X_train), collapse = " x "))
message("X_test dimensions: ", paste(dim(X_test), collapse = " x "))


# ------------------------------------------------------------
# 10. Train the full GRF model
# ------------------------------------------------------------

# Main model:
#   Health outcome_it = f(county-year predictors_it) + error_it
#
# Key options:
#   num.trees:
#     Number of trees in the forest. More trees improve stability.
#   tune.parameters:
#     If "all", GRF searches for better hyperparameters using OOB error.
#     These include mtry, sample.fraction, min.node.size, honesty.fraction,
#     honesty.prune.leaves, alpha, and imbalance.penalty.
#   seed:
#     Makes results reproducible.

rf_full <- regression_forest(
  X = X_train,
  Y = y_train,
  num.trees = num_trees,
  tune.parameters = tune_setting,
  seed = seed
)


# ------------------------------------------------------------
# 11. Predict using the full model
# ------------------------------------------------------------

# Training prediction uses out-of-bag prediction.
# OOB prediction means each training observation is predicted using trees that
# did not use that observation during training.
train_pred <- predict(rf_full)$predictions

# Testing prediction uses counties that were never used in training.
test_pred <- predict(rf_full, X_test)$predictions


# ------------------------------------------------------------
# 12. Evaluate full model performance
# ------------------------------------------------------------

train_performance <- model_metrics(y_train, train_pred)
test_performance  <- model_metrics(y_test, test_pred)

message("Training performance:")
print(train_performance)

message("Testing performance:")
print(test_performance)

# Guidance:
#   Use test_performance as the main evaluation result.
#   If training performance is strong but testing performance is weak,
#   the model may not generalize well to unseen counties.


# ------------------------------------------------------------
# 13. Variable importance
# ------------------------------------------------------------

importance_df <- tibble(
  variable = predictor_names,
  importance = variable_importance(rf_full)
) %>%
  arrange(desc(importance))

top_variables <- importance_df %>%
  slice_head(n = top_n) %>%
  pull(variable)

message("Top variables:")
print(top_variables)


# ------------------------------------------------------------
# 14. Refit model using selected variables
# ------------------------------------------------------------

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

selected_test_performance <- model_metrics(y_test, selected_test_pred)

message("Selected-variable model testing performance:")
print(selected_test_performance)


# ------------------------------------------------------------
# 15. Compare full model and selected-variable model
# ------------------------------------------------------------

performance_summary <- bind_rows(
  full_model = test_performance,
  selected_model = selected_test_performance,
  .id = "model"
)

print(performance_summary)

# Interpretation:
#   If the selected-variable model performs close to the full model,
#   the selected variables may be sufficient for index construction.
#   If the selected-variable model performs much worse, increase top_n or
#   revisit predictor cleaning and outcome choice.


# ------------------------------------------------------------
# 16. Save outputs
# ------------------------------------------------------------

predictions_output <- tibble(
  YEAR = test_df$YEAR,
  COUNTYFIPS = test_df$COUNTYFIPS,
  observed = y_test,
  predicted_full_model = test_pred,
  predicted_selected_model = selected_test_pred
)

# Create output folder if needed.
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

save(
  rf_full,
  rf_selected,
  train_performance,
  test_performance,
  selected_test_performance,
  performance_summary,
  importance_df,
  top_variables,
  predictions_output,
  file = "data/output/grf_county_split_prediction_results.RData"
)

write.csv(importance_df,
          "data/output/grf_variable_importance.csv",
          row.names = FALSE)

write.csv(performance_summary,
          "data/output/grf_performance_summary.csv",
          row.names = FALSE)

write.csv(predictions_output,
          "data/output/grf_test_predictions.csv",
          row.names = FALSE)

message("Done. Outputs saved in data/output/.")
