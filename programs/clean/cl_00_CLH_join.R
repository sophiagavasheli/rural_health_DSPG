# cleaning CLH data and joining FCC data

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# get all xlsx files in  folder
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


write.csv(joined, "data/outcome/CLH/clean_CLH2009_2023.csv", row.names = FALSE)
