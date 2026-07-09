# analyzing fit of random forest models

library(dplyr)
library(purrr)
library(tidyr)
library(tibble)

files <- list.files("data/output",
                    pattern = "_grf_results.RData",
                    full.names = TRUE)

var_lookup <- read.csv("reference/all_codebook.csv") %>%
  select(Variable.Name, Variable.Label)

health_labels <- tribble(
  ~variable,                          ~label,
  "CDCA_STROKE_DTH_RATE_ABOVE35",     "Stroke Mortality Rate (Age 35+)",
  "CDCW_SELFHARM_DTH_RATE",           "Self-Harm Mortality Rate",
  "CDCW_INJURY_DTH_RATE",             "Injury Mortality Rate",
  "CHR_PCT_ADULT_OBESITY",            "Adult Obesity Prevalence",
  "CHR_PCT_LOW_BIRTH_WT",             "Low Birth Weight Prevalence",
  "CHR_PCT_MENTAL_DISTRESS",          "Frequent Mental Distress Prevalence"
)

# all performance
performance <- map_dfr(files, function(f){
  
  load(f)
  
  performance_summary
})

perf_long = performance %>% 
  pivot_longer(
    cols = c(RMSE, MAE, R2),
    names_to = "statistic",
    values_to = "value"
  ) %>% 
  left_join(
    health_labels,
    by = c("health_outcome" = "variable")
  ) %>%
  mutate(
    health_outcome = coalesce(label, health_outcome)
  )

# all variable importance
importance_all <- map_dfr(files, function(f){
  
  load(f)
  
  importance_df %>%
    mutate(outcome = unique(performance_summary$health_outcome))
})

mean_importance = importance_all %>%
  group_by(variable) %>%
  summarize(
    mean_importance = mean(importance),
    appearances = sum(importance > 0)
  ) %>%
  arrange(desc(mean_importance)) %>% 
  left_join(
    var_lookup,
    by = c("variable" = "Variable.Name")
  ) %>%
  mutate(
    variable = coalesce(Variable.Label, variable)
  )



# performance summary plots
perf_long %>% 
  filter(statistic == "RMSE" | statistic == "MAE") %>% 
ggplot(aes(x = statistic, y = value, fill = model)) + 
  geom_col(position= "dodge") +
  facet_wrap(~health_outcome) +
  scale_fill_viridis_d(
    name = "Model",
    labels = c(
      "full_model_test" = "Full Model, Test Data",
      "full_model_train" = "Full Model, Train data",
      "selected_model_test" = "Selected Model, Test Data",
      "selected_model_train" = "Selected Model, Train Data"
    )) + 
  labs(
    title = "Performance Summaries for Random Forest Models by Health Outcome",
    x = "Statistic",
    y = "Value",
    caption = "MAE = mean absolute error; RMSE = root mean squared error"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  )


perf_long %>% 
  filter(statistic == "R2") %>% 
  ggplot(aes(x = health_outcome, y = value, fill = model)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(
  name = "Model",
  labels = c(
    "full_model_test" = "Full Model, Test Data",
    "full_model_train" = "Full Model, Train data",
    "selected_model_test" = "Selected Model, Test Data",
    "selected_model_train" = "Selected Model, Train Data"
  )) + 
  labs(
    title = "R2 for Random Forest Models by Health Outcome",
    x = "Health Outcome",
    y = "Value",
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
  )



  
# all variable importance
mean_importance %>%
  slice_max(mean_importance, n = 10) %>%
  ggplot(
    aes(
      x = reorder(variable, mean_importance),
      y = mean_importance
    )
  ) +
  geom_col(fill = "maroon") +
  coord_flip() +
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 30)) +
  labs(
    title = "Top 10 Mean Variable Importance",
    x = "Predictor Variable",
    y = "Mean Variable Importance")
