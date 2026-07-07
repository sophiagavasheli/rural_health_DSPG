# cleaning CDC wonder data

library(dplyr)

mort1 = read.csv("data/source/CDC_WONDER/mortality_2009_2017.csv", 
                 na.strings = c("Suppressed", "Unreliable", "Not Available", "Missing"))
mort2 = read.csv("data/source/CDC_WONDER/mortality_2018_2023.csv", 
                 na.strings = c("Suppressed", "Unreliable", "Not Available", "Missing"))

clmort1 = mort1 %>% 
  select(County.Code, Year, Crude.Rate) %>% 
  rename(
    fips = County.Code,
    year = Year,
    CDCW_crude_death_rate = Crude.Rate
  ) %>% 
  filter(!is.na(fips)) #get rid of empty rows

clmort2 = mort2 %>% 
  select(County.Code, Year, Crude.Rate) %>% 
  rename(
    fips = County.Code,
    year = Year,
    CDCW_crude_death_rate = Crude.Rate
  ) %>% 
  filter(!is.na(fips))


mortfinal = bind_rows(clmort1, clmort2)

write.csv(mortfinal, "data/outcome/CDC_WONDER/clean_mortality_2009_2023.csv", row.names = FALSE)