# cleaning/joining to create final dataset

library(dplyr)
library(readxl)

clh = readRDS("data/outcome/CLH/clean_clh_2009_2023.rds")
app = read_excel("data/source/ARC/appalachian_counties.xlsx", skip = 4)
fcc = read.csv("data/outcome/FCC_form477/clean_FCC_form477_2009_2023.csv")
mort = read.csv("data/outcome/CDC_WONDER/clean_mortality_2009_2023.csv")

app = app %>% 
  filter(!is.na(STATE)) %>%  #remove empty rows
  mutate(FIPS = sprintf("%05s", FIPS))

fcc = fcc %>% 
  select(-county_name, -state) %>% 
  mutate(fips = sprintf("%05s", fips))

mort = mort %>% 
  mutate(fips =  as.character(fips)) %>% 
  mutate(fips = sprintf("%05s", fips)) %>% 
  mutate(CDCW_crude_death_rate = as.numeric(CDCW_crude_death_rate))

#anti_join(fcc, clh, by = c("year" = "YEAR", "fips" = "COUNTYFIPS")) %>% View()
#anti_join(clh, fcc, by = c("YEAR" = "year", "COUNTYFIPS" = "fips")) %>% View()

final = clh %>% 
  left_join(fcc, by = c("YEAR" = "year", "COUNTYFIPS" = "fips")) %>% 
  left_join(mort, by = c("YEAR" = "year", "COUNTYFIPS" = "fips")) %>% 
  # codes 1-3 are urban, 4-9 are rural. Assign 1 if rural, 0 if urban
  mutate(USDA_rural_indicator_2013 = if_else(AHRF_USDA_RUCC_2013 <= 3, 0L, 1L)) %>% 
  mutate(USDA_rural_indicator_2023 = if_else(AHRF_USDA_RUCC_2023 <= 3, 0L, 1L)) %>% 
  # assign 1 if appalachia, 0 if not
  mutate(ARC_appalachia_indicator = if_else(COUNTYFIPS %in% app$FIPS, 1L, 0L))

write.csv(final, "shiny_dashboard/clean_ALL_data.csv", row.names = FALSE)
saveRDS(final, "shiny_dashboard/clean_ALL_data.rds")

