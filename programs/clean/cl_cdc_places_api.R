# loading/cleaning CDC PlACES data with api
# Sophia

library(CDCPLACES)
library(dplyr)
library(here)
library(purrr)
library(tidyr)

#list of all vars
vars = get_dictionary()

# get county data
states = read.csv("states.csv")
state_abbrevs <- states$state_abbrev

dat <- map_dfr(
  state_abbrevs,
  ~ get_places(
    geography = "county",
    state = .x,
    release = "2025",
    geometry = FALSE
  )
)

measures  = dat %>% 
  distinct(measure, measureid)

filtered = dat %>% 
  select(year, stateabbr, locationname, locationid, measureid, 
         data_value_type, data_value, totalpopulation, totalpop18plus) %>% 
  rename(state_abbrev = stateabbr, county = locationname, GEOID = locationid) %>% 
  filter(data_value_type == "Crude prevalence") %>% 
  select(-data_value_type)

pivot <- filtered %>% 
  pivot_wider(
    id_cols = c(year, GEOID, county, state_abbrev,
                totalpopulation, totalpop18plus),
    names_from = measureid,
    values_from = data_value
  )

pivot23 <- pivot %>% 
  filter(year == 2023)

pivot22 <- pivot %>% 
  filter(year == 2022)

write.csv(pivot22, here("data", "outcome", "CDC_PLACES", "places2022.csv"))
write.csv(pivot23, here("data", "outcome", "CDC_PLACES", "places2023.csv"))

  