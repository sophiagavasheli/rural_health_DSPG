library(readxl)
library(tidyverse)
chr <- read_excel("rawdata/chr2025.xlsx", 
           sheet = "Select Measure Data", #sheet with the data
           skip=1) #skip the first row

colnames(chr) <- colnames(chr) |>
  gsub("\\s+", "_", x = _) |>
  gsub("%", "Pct", x = _) |>
  gsub("#", "Num", x=_)

chr <- chr |> 
  select("FIPS", "County", "Num_Dentists")
