# cleaning 2025 FCC broadband data
#Sophia

library(dplyr)
library(tidyr)
library(here)
library(stringr)

# from FCC BDC
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


write.csv(clean_fixed, here("data", "outcome","FCC", "cleanFCC_BDC_fixed_broadband25.csv"), row.names = FALSE)
write.csv(clean_mobile, here("data", "outcome","FCC", "cleanFCC_BDC_mobile_broadband25.csv"), row.names = FALSE)


#from FCC form 477
form = read.csv(here("data", "source", "FCC", "FCC_form_477_county_tiers2014_2025.csv"))

# state name to state abbrev lookup table
states = read.csv("states.csv")

clean_form = form %>%
  #convert to UTF characters
  mutate(County_Name = iconv(County_Name, from = "",to = "UTF-8",sub = "")) %>% 
  select(-c(State, County)) %>% 
  #remove the word "County" from counties
  mutate(County_Name = gsub(" County", "", County_Name)) %>% 
  #convert FIPS to GEOID (add leading zeros)
  mutate(FIPS = str_pad(FIPS, width = 5, side = "left", pad = "0")) %>% 
  rename(GEOID = FIPS) %>% 
  #convert state name to state abbreviation
  left_join(states, by = c("State_Name" = "state_name")) %>%
  rename(state = state_abbrev) %>% 
  #all col names to lower case
  rename_with(tolower) %>% 
  rename(GEOID = geoid) %>% 
  select(-c(state_name))
  
write.csv(clean_form, here("data", "outcome","FCC", "cleanFCC_form_477_2014_2025.csv"), row.names = FALSE)
