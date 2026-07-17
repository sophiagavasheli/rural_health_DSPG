# plots to analyze fit of rf models

library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)

var_lookup <- read.csv("reference/all_codebook.csv") %>%
  select(Variable.Name, Variable.Label) %>%
  bind_rows(
    tribble(
      ~Variable.Name, ~Variable.Label,
      "acute_care_hospital_avg_drive_time_minutes", "Average Drive Time to Acute Care Hospital (min)",
      "clinic_urgent_care_avg_drive_time_minutes", "Average Drive Time to Clinic/Urgent Care (min)",
      "dentist_avg_drive_time_minutes", "Average Drive Time to Dentist (min)",
      "doctors_medical_specialists_avg_drive_time_minutes", "Average Drive Time to Doctors & Medical Specialists (min)",
      "mental_health_avg_drive_time_minutes", "Average Drive Time to Mental Health Facility (min)",
      "pharmacy_avg_drive_time_minutes", "Average Drive Time to Pharmacy (min)",
      "acute_care_hospital_max_drive_time_minutes", "Maximum Drive Time to Acute Care Hospital (min)",
      "clinic_urgent_care_max_drive_time_minutes", "Maximum Drive Time to Clinic/Urgent Care (min)",
      "dentist_max_drive_time_minutes", "Maximum Drive Time to Dentist (min)",
      "doctors_medical_specialists_max_drive_time_minutes", "Maximum Drive Time to Doctors & Medical Specialists (min)",
      "mental_health_max_drive_time_minutes", "Maximum Drive Time to Mental Health Facility (min)",
      "pharmacy_max_drive_time_minutes", "Maximum Drive Time to Pharmacy (min)",
      "acute_care_hospital_min_drive_time_minutes", "Minimum Drive Time to Acute Care Hospital (min)",
      "clinic_urgent_care_min_drive_time_minutes", "Minimum Drive Time to Clinic/Urgent Care (min)",
      "dentist_min_drive_time_minutes", "Minimum Drive Time to Dentist (min)",
      "doctors_medical_specialists_min_drive_time_minutes", "Minimum Drive Time to Doctors & Medical Specialists (min)",
      "mental_health_min_drive_time_minutes", "Minimum Drive Time to Mental Health Facility (min)",
      "pharmacy_min_drive_time_minutes", "Minimum Drive Time to Pharmacy (min)"
    )
  )

# health outcome labels
many_yrs <- tribble(
  ~variable,                          ~label,
  "CDCA_STROKE_DTH_RATE_ABOVE35",     "Stroke Mortality Rate (Age 35+)",
  "CDCW_SELFHARM_DTH_RATE",           "Self-Harm Mortality Rate",
  "CDCW_INJURY_DTH_RATE",             "Injury Mortality Rate",
  "CHR_PCT_ADULT_OBESITY",            "Adult Obesity Prevalence",
  "CHR_PCT_LOW_BIRTH_WT",             "Low Birth Weight Prevalence",
  "CHR_PCT_MENTAL_DISTRESS",          "Frequent Mental Distress Prevalence"
)

w_drv_time <- tribble(
  ~variable,                          ~label,
  "CDCW_SELFHARM_DTH_RATE",           "Self-Harm Mortality Rate",
  "CDCW_INJURY_DTH_RATE",             "Injury Mortality Rate",
  "CHR_PCT_LOW_BIRTH_WT",             "Low Birth Weight Prevalence",
  "CDCW_DRUG_DTH_RATE",               "Drug Mortality Rate",
  "CDCW_crude_death_rate",            "Overall Mortality Rate"
)

analyze_rf <- function(dir, health_labels, save = FALSE){
  
  # directories
  input_dir <- paste0("data/output/", dir)
  
  figure_dir <- paste0("figures/random_forest/", dir)
  
  if(!dir.exists(figure_dir)){
    dir.create(figure_dir, recursive = TRUE)
  }
  
  
  save_plot <- function(plot, filename, width = 10, height = 7){
    ggsave(
      filename = file.path(figure_dir, filename),
      plot = plot,
      width = width,
      height = height,
      dpi = 300
    )
  }
  
  
  files <- list.files(
    input_dir,
    pattern = ".RData",
    full.names = TRUE
  )
  

  # Performance
  
  performance <- map_dfr(files, function(f){
    load(f)
    performance_summary
  })
  
  
  perf_long <- performance %>% 
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
    ) %>% 
    filter(model == "full_model_test" | model == "selected_model_test")
  
  
  p_rmse_mae <- perf_long %>% 
    filter(statistic %in% c("RMSE","MAE")) %>%
    ggplot(aes(x = statistic, y = value, fill = model)) +
    geom_col(position="dodge") +
    facet_wrap(~health_outcome) +
    scale_fill_manual(
      name = "Model",
      values = c(
        full_model_test = "maroon4",
        selected_model_test = "orange2"
      ),
      breaks = c("full_model_test", "selected_model_test"),
      labels = c("Full Model", "Selected Model")
    ) +
    labs(
      title="Test Data Performance Summaries for Random Forest Models",
      x="Statistic",
      y="Value",
      caption="MAE = mean absolute error; RMSE = root mean squared error"
    ) +
    theme_bw()
  
  
  save_plot(
    p_rmse_mae,
    "performance_RMSE_MAE.png"
  )
  
  
  
  p_r2 <- perf_long %>% 
    filter(statistic == "R2") %>%
    ggplot(aes(x = health_outcome,
               y = value,
               fill = model)) +
    geom_col(position="dodge") +
    scale_fill_manual(
      name = "Model",
      values = c(
        full_model_test = "maroon4",
        selected_model_test = "orange2"
      ),
      breaks = c("full_model_test", "selected_model_test"),
      labels = c("Full Model", "Selected Model")
    ) +
    labs(
      title="R2 for Random Forest Models by Health Outcome",
      x="Health Outcome",
      y="R2"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(hjust=0.5)
    ) + 
    scale_x_discrete(
      labels = \(x) stringr::str_wrap(x, width = 20)
    )
  
  
  save_plot(
    p_r2,
    "performance_R2.png", 12, 8
  )
  
  
  

  # Variable importance

  importance_all <- map_dfr(files, function(f){
    load(f)
    # vsurf outputs are differently named
    if (exists("imp")) {
      importance_df <- imp
      rm(imp)
    }
    
    importance_df %>%
      mutate(outcome = unique(performance_summary$health_outcome))
  })
  
  importance_all = importance_all %>% 
    left_join(
      var_lookup,
      by=c("variable"="Variable.Name")
    ) %>%
    mutate(
      variable = coalesce(Variable.Label, variable)
    )
  
  
  mean_importance <- importance_all %>%
    group_by(variable) %>%
    summarize(
      mean_importance = mean(importance),
      appearances = sum(importance > 0)
    ) %>%
    arrange(desc(mean_importance))
  
  
  p_importance <- mean_importance %>%
    slice_max(mean_importance,n=10) %>%
    ggplot(
      aes(
        x=reorder(variable,mean_importance),
        y=mean_importance
      )
    ) +
    geom_col(fill = "maroon4") +
    coord_flip() +
    scale_x_discrete(
      labels=function(x) str_wrap(x,30)
    ) +
    labs(
      title="Top 10 Mean Variable Importance",
      x=NULL,
      y="Mean Importance"
    ) +
    theme_bw()
  
  
  save_plot(
    p_importance,
    "variable_importance_top10.png"
  )
  
  
  

  # Predictions

  predictions <- map_dfr(files,function(f){
    
    load(f)
    
    predictions_output %>%
      mutate(
        health_outcome =
          unique(performance_summary$health_outcome)
      )
  })
  
  
  predictions <- predictions %>%
    left_join(
      health_labels,
      by=c("health_outcome"="variable")
    ) %>%
    mutate(
      health_outcome = coalesce(label,health_outcome),
      residual = observed - predicted_full_model
    )  %>% 
    pivot_longer(
      cols = c(predicted_full_model, predicted_selected_model),
      names_to = "model",
      values_to = "value"
    )
  
  
  # p_obs_full <- ggplot(
  #   predictions,
  #   aes(observed,predicted_full_model)
  # ) +
  #   geom_point(alpha=.3) +
  #   geom_abline(
  #     intercept=0,
  #     slope=1, color = "maroon4"
  #   ) +
  #   facet_wrap(~health_outcome,scales="free") +
  #   labs(
  #     title="Observed vs Predicted: Full Model",
  #     x="Observed",
  #     y="Predicted"
  #   ) +
  #   theme_bw()
  # 
  # 
  # save_plot(
  #   p_obs_full,
  #   "observed_vs_predicted_full.png"
  # )
  # 
  # 
  # 
  # p_obs_selected <- ggplot(
  #   predictions,
  #   aes(observed,predicted_selected_model)
  # ) +
  #   geom_point(alpha=.3) +
  #   geom_abline(
  #     intercept=0,
  #     slope=1, color = "maroon4"
  #   ) +
  #   facet_wrap(~health_outcome,scales="free") +
  #   labs(
  #     title="Observed vs Predicted: Selected Model",
  #     x="Observed",
  #     y="Predicted"
  #   ) +
  #   theme_bw()
  # 
  # 
  # save_plot(
  #   p_obs_selected,
  #   "observed_vs_predicted_selected.png"
  # )
    
    
    p_obs <- ggplot(
      predictions,
      aes(x= observed, y=value, color=model)
    ) +
      geom_point(alpha=.3) +
      geom_abline(
        intercept=0,
        slope=1, color = "black"
      ) +
      facet_wrap(~health_outcome,scales="free") +
      labs(
        title="Observed vs Predicted Values",
        x="Observed",
        y="Predicted"
      ) +
    scale_color_manual(
      name = "Model",
      values = c(
        predicted_full_model = "maroon4",
        predicted_selected_model = "orange2"
      ),
      breaks = c("predicted_full_model", "predicted_selected_model"),
      labels = c("Full Model", "Selected Model")
      ) +
      theme_bw()


    save_plot(
      p_obs,
      "observed_vs_predicted.png", 12, 8
    )
  
  
  
  # p_res_full <- ggplot(
  #   predictions,
  #   aes(predicted_full_model,residual)
  # ) +
  #   geom_point(alpha=.3) +
  #   geom_hline(yintercept=0, color = "maroon4") +
  #   facet_wrap(~health_outcome,scales="free") +
  #   labs(
  #     title="Residuals vs Predicted: Full Model",
  #     x="Predicted",
  #     y="Residual"
  #   ) +
  #   theme_bw()
  # 
  # 
  # save_plot(
  #   p_res_full,
  #   "residuals_vs_predicted_full.png"
  # )
  # 
  # 
  # 
  # p_res_selected <- ggplot(
  #   predictions,
  #   aes(predicted_selected_model,residual)
  # ) +
  #   geom_point(alpha=.3) +
  #   geom_hline(yintercept=0, color = "maroon4") +
  #   facet_wrap(~health_outcome,scales="free") +
  #   labs(
  #     title="Residuals vs Predicted: Selected Model",
  #     x="Predicted",
  #     y="Residual"
  #   ) +
  #   theme_bw()
  # 
  # 
  # save_plot(
  #   p_res_selected,
  #   "residuals_vs_predicted_selected.png"
  # )
  
    p_res <- ggplot(
      predictions,
      aes(x= value, y = residual, color = model)
    ) +
      geom_point(alpha=.3) +
      geom_hline(yintercept=0, color = "black") +
      facet_wrap(~health_outcome,scales="free") +
      labs(
        title="Residuals vs Predicted Values",
        x="Predicted",
        y="Residual"
      ) +
      scale_color_manual(
        name = "Model",
        values = c(
          predicted_full_model = "maroon4",
          predicted_selected_model = "orange2"
        ),
        breaks = c("predicted_full_model", "predicted_selected_model"),
        labels = c("Full Model", "Selected Model")
        ) +
      theme_bw()


    save_plot(
      p_res,
      "residuals_vs_predicted.png",12, 8
    )
  

  # Variable importance by health outcome
  importance_dir <- file.path(
    figure_dir,
    "variable_importance"
  )
  
  if(!dir.exists(importance_dir)){
    dir.create(importance_dir, recursive = TRUE)
  }
  
  
  plot_importance <- function(outcome_name, label){
    
    p <- importance_all %>%
      filter(outcome == outcome_name) %>%
      slice_max(importance, n = 10) %>%
      ggplot(
        aes(
          x = reorder(variable, importance),
          y = importance
        )
      ) +
      geom_col(fill = "maroon4") +
      coord_flip() +
      scale_x_discrete(
        labels = \(x) stringr::str_wrap(x, width = 30)
      ) +
      labs(
        title = paste("Top 10 Variable Importance for", label),
        x = "Predictor Variable",
        y = "Variable Importance"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
    
    
    save_plot(
      p,
      paste0(
        "variable_importance/",
        gsub(" ", "_", label),
        ".png"
      ),
      width = 10,
      height = 6
    )
    
  }
  
  pwalk(
    list(
      outcome_name = health_labels$variable,
      label = health_labels$label
    ),
    plot_importance
  )
  
  
  if(save) {
    saveRDS(performance, paste0("shiny_dashboard/", dir, "_performance.rds"))
    saveRDS(predictions, paste0("shiny_dashboard/", dir, "_predictions.rds"))
    saveRDS(importance_all, paste0("shiny_dashboard/", dir, "_importance.rds"))
  }
} 

# analyze_rf("with_demographics_year_dummies", many_yrs)
# analyze_rf("without_demographics_year_dummies", many_yrs)

analyze_rf("drive_grf_w_dem", w_drv_time)
analyze_rf("drive_grf_wo_dem", w_drv_time)

# analyze_rf("drive_vsurf_w_dem", w_drv_time)
# analyze_rf("drive_vsurf_wo_dem", w_drv_time)

