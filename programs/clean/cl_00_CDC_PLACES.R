# loading/cleaning CDC PlACES data with api
# Sophia

library(CDCPLACES)
library(dplyr)
library(here)
library(purrr)
library(tidyr)

#list of all vars
#vars = get_dictionary()

#variables to keep
want <- c(
  "CASTHMA", "ARTHRITIS", "STROKE", "OBESITY", "VISION",
  "DISABILITY", "MOBILITY", "BINGE", "DEPRESSION",
  "COPD", "CHD", "CHECKUP", "CANCER", "CSMOKING", "ACCESS2"
)

# get county data (2025 release, 2023 data)
states = read.csv("states.csv")
state_abbrevs <- states$state_abbrev

dat25 <- map_dfr(
  state_abbrevs,
  ~ get_places(
    geography = "county",
    state = .x,
    release = "2025",
    geometry = FALSE
  )
)

measures  = dat25 %>% 
  distinct(measureid, measure) %>% 
  filter(measureid %in% want) %>% 
  mutate(measureid = tolower(measureid))

filtered25 = dat25 %>% 
  select(year, stateabbr, locationname, locationid, measureid, 
         data_value_type, data_value, totalpopulation, totalpop18plus) %>% 
  rename(state_abbrev = stateabbr, county = locationname, GEOID = locationid) %>% 
  filter(data_value_type == "Crude prevalence") %>% 
  select(-data_value_type) %>% 
  filter(measureid %in% want) %>% 
  mutate(measureid = tolower(measureid))

pivot23 <- filtered25 %>% 
  pivot_wider(
    id_cols = c(year, GEOID, county, state_abbrev,
                totalpopulation, totalpop18plus),
    names_from = measureid,
    values_from = data_value
  ) %>% 
  filter(year == 2023)


#get 2022
dat24 <- map_dfr(
  state_abbrevs,
  ~ get_places(
    geography = "county",
    state = .x,
    release = "2024",
    geometry = FALSE
  )
)

filtered24 = dat24 %>%
  select(year, stateabbr, locationname, locationid, measureid, 
         data_value_type, data_value, totalpopulation, totalpop18plus) %>% 
  rename(state_abbrev = stateabbr, county = locationname, GEOID = locationid) %>% 
  filter(data_value_type == "Crude prevalence") %>% 
  select(-data_value_type) %>% 
  filter(measureid %in% want) %>% 
  mutate(measureid = tolower(measureid))

pivot22 <- filtered24 %>% 
  pivot_wider(
    id_cols = c(year, GEOID, county, state_abbrev,
                totalpopulation, totalpop18plus),
    names_from = measureid,
    values_from = data_value
  ) %>% 
  filter(year == 2022)


write.csv(pivot22, here("data", "outcome", "CDC_PLACES", "clean_places_2022.csv"), row.names = FALSE)
write.csv(pivot23, here("data", "outcome", "CDC_PLACES", "clean_places_2023.csv"), row.names = FALSE)

#this code was used to compare amount of NAs and select variables
# na23 = pivot23 %>%
#   summarise(across(everything(), ~ sum(is.na(.))))
# na22 = pivot22 %>%
#   summarise(across(everything(), ~ sum(is.na(.))))
# 
# na22 <- pivot22 %>%
#   summarise(across(everything(), ~ sum(is.na(.)))) %>%
#   pivot_longer(
#     everything(),
#     names_to = "variable",
#     values_to = "na_2022"
#   )
# 
# na23 <- pivot23 %>%
#   summarise(across(everything(), ~ sum(is.na(.)))) %>%
#   pivot_longer(
#     everything(),
#     names_to = "variable",
#     values_to = "na_2023"
#   )
# 
# na_compare <- na22 %>%
#   full_join(na23, by = "variable") %>%
#   arrange(desc(na_2023), desc(na_2022))