# random forest model plots


library(dplyr)
library(ggplot2)

#models
load("data/analysis/random_forest_models.RData")

var_lookup <- read_csv("reference/all_codebook.csv") %>%
  select(Variable.Name, Variable.Label)

plot_importance <- function(importance_df, title, start_yr, end_yr) {
  
  importance_df <- importance_df %>%
    left_join(
      var_lookup,
      by = c("variable" = "Variable.Name")
    ) %>%
    mutate(
      variable = coalesce(Variable.label, variable)
    )
  
  p <- importance_df %>%
    slice_max(importance, n = 20) %>%
    ggplot(
      aes(
        x = reorder(variable, importance),
        y = importance
      )
    ) +
    geom_col(fill = "maroon") +
    coord_flip() +
    labs(
      title = paste("Top 20 Variable Importance for", title),
      x = "Predictor Variable",
      y = "Variable Importance",
      caption = paste(start_yr, "-", end_yr)
    )
  
  ggsave(
    paste0("figures/random_forest/", gsub(" ", "_", title), ".png"),
    plot = p,width = 10,height = 6,dpi = 300,bg = "white"
  )
}

# plots
plot_importance(
  mortality$importance,
  title = "All-Cause Mortality Rate",
  start_yr = 2010,
  end_yr = 2023
)

plot_importance(
  stroke_dth$importance,
  title = "Stroke Mortality Rate (Age 35+)",
  start_yr = 2010,
  end_yr = 2021
)

plot_importance(
  hiv_rate$importance,
  title = "HIV Diagnosis Rate (Age 13+)",
  start_yr = 2010,
  end_yr = 2023
)

plot_importance(
  self_harm_dth$importance,
  title = "Self-Harm Mortality Rate",
  start_yr = 2010,
  end_yr = 2023
)

plot_importance(
  injury_dth$importance,
  title = "Injury Mortality Rate",
  start_yr = 2010,
  end_yr = 2023
)

plot_importance(
  heart_dth$importance,
  title = "Heart Disease Mortality Rate (Age 35+)",
  start_yr = 2010,
  end_yr = 2021
)

plot_importance(
  obesity$importance,
  title = "Adult Obesity Prevalence",
  start_yr = 2010,
  end_yr = 2017
)

plot_importance(
  diabetes$importance,
  title = "Adult Diabetes Prevalence",
  start_yr = 2010,
  end_yr = 2017
)

plot_importance(
  low_birth$importance,
  title = "Low Birth Weight Prevalence",
  start_yr = 2010,
  end_yr = 2014
)

plot_importance(
  mental$importance,
  title = "Frequent Mental Distress Prevalence",
  start_yr = 2014,
  end_yr = 2022
)

plot_importance(
  alc_drv_death$importance,
  title = "Alcohol-Impaired Driving Deaths",
  start_yr = 2012,
  end_yr = 2022
)