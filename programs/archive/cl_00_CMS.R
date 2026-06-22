# cleaning/geocoding cms data 2022

library(here)
library(dplyr)
library(tidygeocoder)

cms = read.csv(here("data", "source", "CMS", "hospitals_10_22.csv"))


cms_fix = cms %>% 
  select(Facility.ID, Facility.Name, Address, City, State, County.Name, ZIP.Code, Hospital.Type, Hospital.Ownership, Emergency.Services) %>% 
  rename_with(tolower) %>% 
  mutate(location = paste0(address,', ', city,', ', state,', ', zip.code))

# geocode first with census then missing adresses with OSM
cms_geo <- cms_fix %>%
  geocode_combine(
    queries = list(
      list(method = "census"),
      list(method = "osm"),
      list(method = "arcgis")
    ),
    global_params = list(address = "location"),
    cascade = TRUE
  )

#make sure nothing missing
cms_geo %>% filter(is.na(lat) | is.na(long)) 

# filter out us territories
cms_geo = cms_geo %>% 
  filter(!state %in% c("GU", "AS", "PR", "VI", "MP"))
  
write.csv(cms_geo, here("data", "outcome", "CMS", "cms2022_clean.csv"), row.names = FALSE)