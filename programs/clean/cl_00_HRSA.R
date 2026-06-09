# cleaning HRSA Area Health Resources Data
# Sophia

library(here)

#this file has everything, over 4k vars
# other csvs are more manageable subsets

ahrf = read.csv(here("data", "source", "HRSA", "AHRF2025.csv"))
colnames(ahrf)

