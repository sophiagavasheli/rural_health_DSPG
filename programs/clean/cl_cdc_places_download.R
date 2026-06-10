# cleaning downloaded CDC data
# Sophia

library(dplyr)
library(stringr)
library(tidyr)
library(here)

data = read.csv(here("data", "source", "CDC_PLACES", "places2025.csv"))

#change to character
data$LocationID = as.character(data$LocationID)

measures  = data %>% 
  distinct(Measure, MeasureId)

# data sourced from BRFSS and all data values are percentages
filtered = data %>% 
  select(Year, StateAbbr, LocationName, LocationID, MeasureId, 
         Data_Value_Type, Data_Value, TotalPopulation, TotalPop18plus) %>% 
  rename(county = LocationName) %>% 
  mutate(LocationID = str_pad(LocationID, width = 5, side = "left", pad = "0")) %>% 
  rename(GEOID = LocationID) %>% 
  filter(county != "") %>% 
  #only using crude prevalence since we'll adjust for age in the final model
  filter(Data_Value_Type == "Crude prevalence") %>% 
  select(-Data_Value_Type) %>% 
  rename(state_abbrev = StateAbbr, measure_id = MeasureId) %>% 
  rename_with(tolower) %>% 
  rename(GEOID = geoid)

pivot <- filtered %>% 
  pivot_wider(
    id_cols = c(year, GEOID, county, state_abbrev,
                totalpopulation, totalpop18plus),
    names_from = measure_id,
    values_from = data_value
  )

pivot23 <- pivot %>% 
  filter(year == 2023)

pivot22 <- pivot %>% 
  filter(year == 2022)
