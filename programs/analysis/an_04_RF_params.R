# create table of model parameters

library(dplyr)
library(purrr)
library(tidyr)

param_table <- function(dir) {
  
  input_dir <- paste0("data/output/", dir)
  
  files <- list.files(
    input_dir,
    pattern = ".RData",
    full.names = TRUE
  )
  
  
  params <- map_dfr(files, function(f){
    load(f)
    
    full = as.data.frame(tuned_params_full) %>% 
      mutate(model = "full") 
    
    selected = as.data.frame(tuned_params_selected) %>% 
      mutate(model = "selected")
    
    bind_rows(full, selected) %>% 
      mutate(outcome = unique(performance_summary$health_outcome))
    
  })
  
  desc = tibble::tibble(
    parameter = c(
      "sample.fraction",
      "mtry",
      "min.node.size",
      "honesty.fraction",
      "honesty.prune.leaves",
      "alpha",
      "imbalance.penalty"
    ),
    description = c(
      "Proportion of observations sampled for each tree. Controls the amount of data used per tree and affects bias-variance tradeoff.",
      "Number of variables randomly considered at each split. Controls feature randomness and tree diversity.",
      "Minimum number of observations required in a terminal node. Controls tree depth and model complexity.",
      "Fraction of the training sample used to determine tree splits under honesty. Controls the split between training and estimation samples.",
      "Whether to remove empty leaves created during honest splitting. Improves tree validity by pruning unused leaves.",
      "Quantile parameter controlling allowed imbalance in splits. Smaller values encourage more balanced splits.",
      "Penalty applied to highly imbalanced splits. Discourages splits that create very uneven child nodes."
    )
  )
  
  params_long = params %>% 
    pivot_longer(
      cols = c("sample.fraction", "mtry", "min.node.size", "honesty.fraction", 
               "honesty.prune.leaves", "alpha", "imbalance.penalty")
    ) %>% 
    left_join(
      desc,
      by = c("name" = "parameter")
    ) %>% 
    rename(parameter = name)
    
  out_dir <- paste0("figures/random_forest/", dir)
  if(!dir.exists(out_dir)){
    dir.create(out_dir, recursive = TRUE)
  }
  
  write.csv(params_long, paste0(out_dir, "/model_params.csv"), row.names = F)
}

# param_table("with_demographics_year_dummies")
# param_table("without_demographics_year_dummies")

param_table("one_yr_grf_w_dem_drive")
param_table("one_yr_grf_wo_dem_drive")

param_table("one_yr_vsurf_w_dem_drive")
param_table("one_yr_vsurf_wo_dem_drive")
