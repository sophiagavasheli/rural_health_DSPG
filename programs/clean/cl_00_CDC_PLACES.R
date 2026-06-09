# loading/cleaning CDC PlACES data
# Sophia

library(CDCPLACES)
library(dplyr)
library(here)

#list of all vars
vars = get_dictionary()

# get county data
dat = get_places(geography = "county", release="2025", geometry = FALSE)

#clean
# data sourced from BRFSS and all data values are percentages
filtered = dat %>% 
  select(c(stateabbr, locationname, locationid, measureid, measure, data_value_type, 
           data_value, low_confidence_limit, high_confidence_limit, 
           totalpopulation, totalpop18plus)) %>% 
  rename(state = stateabbr) %>% 
  rename(county = locationname) %>% 
  rename(GEOID = locationid)

write.csv(filtered, here("data", "outcome", "CDC_PLACES", "places2025.csv"))
  