# random forest model plots

library(dplyr)
library(ggplot2)

# for prettier names
var_lookup <- read.csv("reference/all_codebook.csv") %>%
  select(Variable.Name, Variable.Label)


# load model and plot importance
plot_importance <- function(outcome, title, start_yr, end_yr) {
  
  load(paste0("data/output/", outcome, "_grf_results.RData"))
  
  importance_df <- importance_df %>%
    left_join(
      var_lookup,
      by = c("variable" = "Variable.Name")
    ) %>%
    mutate(
      variable = coalesce(Variable.Label, variable)
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
      caption = paste("Data years:", start_yr, "-", end_yr)
    )
  
  ggsave(
    paste0("figures/random_forest/", gsub(" ", "_", title), ".png"),
    plot = p,width = 10,height = 6,dpi = 300,bg = "white"
  )
}

# plots
plot_importance("CDCA_STROKE_DTH_RATE_ABOVE35",
  title = "Stroke Mortality Rate (Age 35+)",
  start_yr = 2010, end_yr = 2021)


plot_importance("CDCW_SELFHARM_DTH_RATE",
  title = "Self-Harm Mortality Rate",
  start_yr = 2010,
  end_yr = 2023
)

plot_importance("CDCW_INJURY_DTH_RATE",
  title = "Injury Mortality Rate",
  start_yr = 2010,
  end_yr = 2023
)


plot_importance("CHR_PCT_ADULT_OBESITY",
  title = "Adult Obesity Prevalence",
  start_yr = 2010,
  end_yr = 2017
)


plot_importance("CHR_PCT_LOW_BIRTH_WT",
  title = "Low Birth Weight Prevalence",
  start_yr = 2010,
  end_yr = 2014
)

plot_importance("CHR_PCT_MENTAL_DISTRESS",
  title = "Frequent Mental Distress Prevalence",
  start_yr = 2014,
  end_yr = 2022
)
