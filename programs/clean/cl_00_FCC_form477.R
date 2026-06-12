#cleaning FCC form 477
# Sophia 

library(here)
library(dplyr)
library(stringr)

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

years = 2014:2025

for (yr in years) {
  year_dat = clean_form %>% 
    filter(year == yr) %>% 
    select(-year)
  
  write.csv(year_dat, here("data", "outcome", "FCC_form477", 
                           paste0("form477_", yr, ".csv")), row.names = FALSE)
}
