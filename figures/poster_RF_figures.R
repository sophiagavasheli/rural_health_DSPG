# figures for poster

library(dplyr)
library(ggplot2)
library(purrr)

outdir = "figures/poster_figs"

pal <- c(
  "#861F41",
  "maroon",  
  "#D96C1F", 
  "goldenrod2",
  "#CA7594",
  "#EDA369"
)


save_plot <- function(plot, filename, width = 10, height = 7){
  ggsave(
    filename = file.path(outdir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

imp = readRDS("shiny_dashboard/many_year_grf_importance.rds") %>% 
  filter(demographics == "yes")

perf = readRDS("shiny_dashboard/many_year_grf_performance.rds") %>% 
  filter(demographics == "yes")


p_rmse_mae <- perf %>% 
  filter(statistic %in% c("RMSE","MAE")) %>%
  ggplot(aes(x = statistic, y = value, fill = model)) +
  geom_col(position="dodge") +
  facet_wrap(~health_outcome, scales = "free_y") +
  scale_fill_manual(
    name = "",
    values = c(
      full_model_test = pal[1],
      selected_model_test = pal[3]
    ),
    breaks = c("full_model_test", "selected_model_test"),
    labels = c("Full Model", "Selected Model")
  ) +
  labs(
    title="Mean Absolute Errors and Root Mean Squared Errors for Random Forest Models by Health Outcome",
    x="Statistic",
    y="Value") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )

save_plot(p_rmse_mae, "rmse_mae.png")

p_r2 <- perf %>% 
  filter(statistic == "R2") %>%
    ggplot(aes(x = statistic, y = value, fill = model)) +
    geom_col(position="dodge") +
    facet_wrap(~health_outcome, scales = "free_y") +
    scale_fill_manual(
      name = "",
      values = c(
        full_model_test = pal[1],
        selected_model_test = pal[3]
      ),
      breaks = c("full_model_test", "selected_model_test"),
      labels = c("Full Model", "Selected Model")
    ) +
    labs(
      title="R2 Values for Random Forest Models by Health Outcome",
      x="Statistic",
      y="Value") +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    )

save_plot(p_r2, "r2.png")

plot_importance <- function(outcome_name, label){
  
  p_dem <- imp %>%
    filter(outcome == outcome_name) %>%
    arrange(desc(importance)) %>%
    slice_max(importance, n = 10) %>%
    ggplot(
      aes(
        x = reorder(Variable.Label, importance),
        y = importance,
        fill = is_infrastructure
      )
    ) +
    geom_col() +
    scale_fill_manual(
      name = "",
      values = c(
        yes = pal[5],
        no = pal[6]
      ),
      breaks = c("yes", "no"),
      labels = c("Infrastructure Variable", "Non-infrastructure Variable")
    ) +
    coord_flip() +
    scale_x_discrete(
      labels = \(x) stringr::str_wrap(x, width = 30)
    ) +
    labs(
      title = paste0("Variable Importance in Predicting ", label),
      x = "Predictor Variable",
      y = "Variable Importance"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    )
  
  
  save_plot(
    p_dem,
    paste0(gsub(" ", "_", label), "_dem.png")
  )
  
}

health_labels <- tribble(
  ~variable,                          ~label,
  "CDCA_STROKE_DTH_RATE_ABOVE35",     "Stroke Mortality Rate (Age 35+)",
  "CDCW_SELFHARM_DTH_RATE",           "Self-Harm Mortality Rate",
  "CDCW_INJURY_DTH_RATE",             "Injury Mortality Rate",
  "CHR_PCT_ADULT_OBESITY",            "Adult Obesity Prevalence",
  "CHR_PCT_LOW_BIRTH_WT",             "Low Birth Weight Prevalence",
  "CHR_PCT_MENTAL_DISTRESS",          "Frequent Mental Distress Prevalence"
)
pwalk(
  list(
    outcome_name = health_labels$variable,
    label = health_labels$label
  ),
  plot_importance
)