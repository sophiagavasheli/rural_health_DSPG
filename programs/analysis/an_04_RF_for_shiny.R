# export RF data for Shiny plots

library(dplyr)
library(purrr)
library(tidyr)
library(tibble)

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

export <- function(dir, health_labels, save = FALSE){
  
  # directories
  input_dir <- paste0("data/output/", dir)
  
  
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
  
  
  
  list(
    performance = perf_long,
    importance = importance_all,
    predictions = predictions
  )
} 

combine <- function(dir1, dir2, labs, name) {
  res1 = export(dir1, labs)
  res2 = export(dir2, labs)
  
  perf1 = res1$performance %>% 
    mutate(demographics = "yes")
  
  imp1 = res1$importance %>% 
    mutate(demographics = "yes")
  
  pred1 = res1$predictions %>% 
    mutate(demographics = "yes")
  
  perf2 = res2$performance %>% 
    mutate(demographics = "no")
  
  imp2 = res2$importance %>% 
    mutate(demographics = "no")
  
  pred2 = res2$predictions %>% 
    mutate(demographics = "no")
  
  performance = bind_rows(perf1, perf2)
  importance = bind_rows(imp1, imp2)
  predictions = bind_rows(pred1, pred2)
  
  saveRDS(performance, paste0("shiny_dashboard/", name, "_performance.rds"))
  saveRDS(importance, paste0("shiny_dashboard/", name, "_importance.rds"))
  saveRDS(predictions, paste0("shiny_dashboard/", name, "_predictions.rds"))
  
}

combine("with_demographics_year_dummies", "without_demographics_year_dummies", 
        many_yrs, "many_year_grf")

combine("one_yr_grf_w_dem_drive","one_yr_grf_wo_dem_drive", 
        w_drv_time, "one_year_grf")

combine("one_yr_vsurf_w_dem_drive", "one_yr_vsurf_wo_dem_drive", 
        w_drv_time, "one_year_vsurf")
