# loading/cleaning CDC PlACES data with api
# Sophia

library(CDCPLACES)
library(dplyr)
library(here)
library(purrr)

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


  