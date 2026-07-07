# random forest models

library(grf)
library(dplyr)
library(randomForest)
library(purrr)
library(skimr)
library(ggplot2)

## data prep
predictors = readRDS("data/analysis/random_forest_predictor_dat.rds_2010_2021")
outcome = readRDS("data/analysis/random_forest_outcome_dat.rds_2010_2021")

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
  filter(as.numeric(COUNTYFIPS) < 57000) %>% 
  arrange(YEAR, COUNTYFIPS) %>% 
  mutate(COUNTYFIPS = as.factor(COUNTYFIPS)) %>% 
  group_split(YEAR) %>%
  map_dfr(~ na.roughfix(.x))

# skim(pred_clean)
# skim(outcome_clean)

#some of the variables are entirely missing from a year
vars_to_remove <- pred_clean %>%
  group_by(YEAR) %>%
  summarise(across(where(is.numeric), ~ all(is.na(.))), .groups = "drop") %>%
  # Keep only columns where at least one year returned TRUE (all NA)
  select(where(~ any(. == TRUE))) %>%
  names()

pred_mat <- pred_clean %>%
  select(-all_of(vars_to_remove), -COUNTYFIPS) %>% 
  as.matrix()

# rf model
mod = regression_forest(pred_mat, outcome_clean$CDCW_crude_death_rate)

importance <- variable_importance(mod)

importance_df <- data.frame(
  variable = colnames(pred_mat),
  importance = importance
) 

importance_df %>%
  slice_head(n = 20) %>%
  ggplot(
    aes(
      x = reorder(variable, importance),
      y = importance
    )
  ) +
  geom_col() +
  coord_flip()

pred <- predict(mod)

results <- data.frame(
  observed = outcome_clean$CDCW_crude_death_rate,
  predicted = pred$predictions
)

rmse <- sqrt(mean(
  (results$observed - results$predicted)^2
))

rmse

r2 <- 1 - sum((results$observed - results$predicted)^2) /
  sum((results$observed - mean(results$observed))^2)

r2