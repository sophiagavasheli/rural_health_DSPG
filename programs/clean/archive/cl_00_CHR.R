# loading/cleaning CHR data with API
# Sophia

library(countyhealthR)
library(dplyr)
library(purrr)
library(tidyr)
library(here)
library(stringr)

#all the measures available
measures = list_chrr_measures() 
states = read.csv("states.csv")

want = c("Poor or Fair Health", "Poor Physical Health Days", "Poor Mental Health Days",
        "HIV Prevalence", "Frequent Mental Distress", "Premature Death", "Uninsured")

#get measures
chr <- map_dfr(
  want,
  ~ get_chrr_measure_data(
    geography = "county",
    measure = .x,
    release_year = 2025,
    verbose = TRUE
  ) %>%
    mutate(requested_measure = .x)
)

#see years of the data
meta = map_df(
  want,
  ~get_chrr_measure_metadata(
    measure = .x,
    release_year = 2025
  )
)
#all are from 2022

#get 2022 Mental Health Providers
mental = get_chrr_measure_data(geography = "county", measure = "Mental Health Providers",
                            release_year = 2023)


mental_fix = mental %>% 
  select(state_fips, county_fips, raw_value) %>% 
  rename(mental_health_providers = raw_value)


chr_fix = chr %>% 
  select(state_fips, county_fips, raw_value, requested_measure) %>% 
  mutate(requested_measure = tolower(requested_measure)) %>% 
  mutate(requested_measure = gsub(" ", "_", requested_measure))

chr_pivot = pivot_wider(chr_fix,
  id_cols = c(state_fips, county_fips),
  names_from = requested_measure,
  values_from = raw_value
)   

final = chr_pivot %>% 
  left_join(mental_fix, by= c("state_fips" = "state_fips", 
                              "county_fips" = "county_fips")) %>% 
  mutate(
    state_fips = str_pad(as.character(state_fips), width = 2, pad = "0"),
    county_fips = str_pad(as.character(county_fips), width = 3, pad = "0"),
    GEOID = paste0(state_fips, county_fips)
  ) %>% 
  select(-c(state_fips, county_fips))

write.csv(final, here("data", "outcome", "CHR", "chr2022.csv"), row.names = FALSE)
