# cleaning RUCA codes 2023/13

# codes 1-3 = urban (0 in cleaned dataset)
# codes 4-9 = rural (1 in cleaned dataset)

library(readxl)
library(here)
library(dplyr)
library(stringr)

ruca13 = read_excel(here("data", "source", "RUCA", "ruca2013.xls"))
ruca23 = read_excel(here("data", "source", "RUCA", "ruca2023.xlsx"))

clean23 = ruca23 %>% 
  select(FIPS, State, County_Name, RUCC_2023) %>% 
  mutate(FIPS = str_pad(FIPS, width = 5, side = "left", pad = "0")) %>% 
  mutate(County_Name = gsub(" County", "", County_Name)) %>% 
  mutate(rural23 = if_else(RUCC_2023 <= 3, 0, 1)) %>% 
  rename_with(tolower) %>% 
  rename(GEOID = fips)
  
clean13 = ruca13 %>% 
  select(FIPS, State, County_Name, RUCC_2013) %>% 
  mutate(FIPS = str_pad(FIPS, width = 5, side = "left", pad = "0")) %>% 
  mutate(County_Name = gsub(" County", "", County_Name)) %>% 
  mutate(rural13 = if_else(RUCC_2013 <= 3, 0, 1)) %>% 
  rename_with(tolower) %>% 
  rename(GEOID = fips)

write.csv(clean13, here("data", "outcome", "RUCA", "clean_ruca_2013.csv"), row.names = FALSE)

write.csv(clean23, here("data", "outcome", "RUCA", "clean_ruca_2023.csv"), row.names = FALSE)