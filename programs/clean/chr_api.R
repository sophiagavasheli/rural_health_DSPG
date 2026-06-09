# loading/cleaning CHR data with API
# Sophia

library(countyhealthR)
library(dplyr)
library(purrr)

#all the measures available
measures = list_chrr_measures() 
states = read.csv("states.csv")

dat <- map_df(
  states$state_abbrev,
  get_chrr_county_data,
  release_year = 2025
)
