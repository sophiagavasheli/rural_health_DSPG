# cleaning 2025 FCC broadband data
#Sophia

library(dplyr)
library(tidyr)

fixed = read.csv(here("data", "source", "FCC", "FCC_fixed_broadband_25.csv"))
mobile = read.csv(here("data", "source", "FCC", "FCC_mobile_broadband_25.csv"))

clean_fixed <- fixed %>% 
  #keep counties
  filter(geography_type =="County") %>% 
  #extract state abbrevs
  mutate(geography_desc_full = trimws(sub(".*,", "", geography_desc_full))) %>%
  #rename
  rename(state = geography_desc_full) %>% 
  #remove the word "County" from counties
  mutate(geography_desc = gsub(" County", "", geography_desc)) %>% 
  rename(county = geography_desc) %>% 
  # keep nontribal divisions
  filter(area_data_type %in% c("Total", "Urban", "Rural")) %>% 
  # use all technology
  filter(technology == "Any Technology") %>% 
  #drop unnecessary cols
  select(-c(geography_type, technology)) %>% 
  rename(GEOID = geography_id) #useful for joins later
  

clean_mobile <- mobile %>% 
  #keep counties
  filter(geography_type =="County") %>% 
  #separate county and state
  separate(geography_desc, into = c("county", "state"), sep = ", ") %>% 
  #remove the word "County" from counties
  mutate(county = gsub(" County", "", county)) %>% 
  filter(area_data_type %in% c("Total", "Urban", "Rural")) %>% 
  select(-c(geography_type)) %>% 
  rename(GEOID = geography_id)


write.csv(clean_fixed, here("data", "outcome","FCC", "cleanFCC_fixed_broadband25.csv"))
write.csv(clean_mobile, here("data", "outcome","FCC", "cleanFCC_mobile_broadband25.csv"))
