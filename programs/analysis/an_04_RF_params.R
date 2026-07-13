# look at parameters of each rf model

library(dplyr)
library(purrr)

files <- list.files(
  "data/output/all_params_tuned_plus_year_effects",
  pattern = "_grf_results.RData",
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

write.csv(params, "figures/random_forest/all_params_tuned_plus_year_effects/model_params.csv", row.names = F)

