# cleaning CLH data and joining FCC data

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

fcc = read.csv("data/outcome/FCC_form477/clean_FCC_form477_2009_2023.csv")

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
  filter(COUNTYFIPS < 57000)

#convert back to character
clh$COUNTYFIPS = as.character(clh$COUNTYFIPS)

fcc = fcc %>% 
  select(-county_name, -state) %>% 
  filter(fips < 57000) #exclude US territories

fcc$fips = as.character(fcc$fips)

#anti_join(fcc, clh, by = c("year" = "YEAR", "fips" = "COUNTYFIPS")) %>% View()
#anti_join(clh, fcc, by = c("YEAR" = "year", "COUNTYFIPS" = "fips")) %>% View()

final = clh %>% 
  left_join(fcc, by = c("YEAR" = "year", "COUNTYFIPS" = "fips"))


write.csv(final, "shiny_dashboard/clean_FCC_CLH_data.csv", row.names = FALSE)
