# cleaning CLH data

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)



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

# data to big to store as csv
saveRDS(joined, "data/outcome/CLH/clean_clh_2009_2023.rds")
