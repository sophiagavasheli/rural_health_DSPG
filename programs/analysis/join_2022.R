# joining 2022 data

library(dplyr)
library(here)

acs = read.csv(here("data", "outcome", "ACS", "clean_ACS_2022.csv"))

places = read.csv(here("data", "outcome", "CDC_PLACES", "clean_places_2022.csv"))

all_mort = read.csv(here("data", "outcome", "CDC_WONDER", "clean_all_mortality_2022.csv"))

drug_alc_mort = read.csv(here("data", "outcome", "CDC_WONDER", "clean_mortality_drug_alc_2022.csv"))

