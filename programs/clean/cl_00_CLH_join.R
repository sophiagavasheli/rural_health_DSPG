# cleaning CLH data and joining it to FCC and CDCW data

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

fcc = read.csv("data/outcome/FCC_form477/clean_FCC_form477_2009_2023.csv")
mort = read.csv("data/outcome/CDC_WONDER/clean_mortality_2009_2023.csv")

# get all CLH xlsx files in folder
files <- list.files(
  path = "data/source/CLH",
  pattern = "\\.xlsx$",
  full.names = TRUE
)


# join all
joined <- lapply(files, function(f) {
  read_excel(f, sheet = "Data", guess_max = 10000) 
  
}) |>
  bind_rows()

joined$COUNTYFIPS = as.numeric(joined$COUNTYFIPS)
clh = joined %>% 
  filter(COUNTYFIPS < 57000) #exclude US territories

#convert back to character
clh$COUNTYFIPS = as.character(clh$COUNTYFIPS)

fcc = fcc %>% 
  select(-county_name, -state) %>% 
  filter(fips < 57000) 

fcc$fips = as.character(fcc$fips)

mort = mort %>% 
  filter(fips < 57000) 

mort$fips = as.character(mort$fips)
mort$CDCW_crude_death_rate = as.numeric(mort$CDCW_crude_death_rate)

#anti_join(fcc, clh, by = c("year" = "YEAR", "fips" = "COUNTYFIPS")) %>% View()
#anti_join(clh, fcc, by = c("YEAR" = "year", "COUNTYFIPS" = "fips")) %>% View()

final = clh %>% 
  left_join(fcc, by = c("YEAR" = "year", "COUNTYFIPS" = "fips")) %>% 
  left_join(mort, by = c("YEAR" = "year", "COUNTYFIPS" = "fips"))



write.csv(final, "shiny_dashboard/clean_ALL_data.csv", row.names = FALSE)
