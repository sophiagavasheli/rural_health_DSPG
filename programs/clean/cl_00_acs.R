# downloading and cleaning acs data 

library(tidycensus)
library(dplyr)

acs_vars <- load_variables(2024, 
               "acs5")
View(acs_vars)

acs_vars_filter  = acs_vars |> 
  filter(geography == "county") |> 
  filter(concepts %in% c("Median Income in the Past 12 Months (in 2024 Inflation-Adjusted Dollars) by Geographical Mobility in the Past Year for Residence 1 Year Ago in the United States"))

data <- get_acs(table = "B27011", geography = "county", output = "wide")
