# cleaning CDC wonder

library(here)
library(dplyr)
library(stringr)
library(tidyr)

mort = read.csv(here("data", "source", "CDC_WONDER", "mortality2024.csv"))
mort$County.Code = as.character(mort$County.Code)

filtered = mort %>% 
  filter(Notes == "") %>% 
  select(-c(Notes, Crude.Rate.Upper.95..Confidence.Interval, 
            Crude.Rate.Lower.95..Confidence.Interval)) %>% 
  separate(County, into = c("county", "state"), sep = ", ", fill = "right") %>% 
  mutate(county = gsub(" County", "", county)) %>% 
  mutate(cause = recode(Drug.Alcohol.Induced.Code,
    "O" = "non_drug_alcohol",
    "D" = "drug_induced",
    "A" = "alcohol_induced")) %>% 
  select(-c(Drug.Alcohol.Induced.Code, Drug.Alcohol.Induced)) %>% 
  mutate(County.Code = str_pad(County.Code, width = 5, side = "left", pad = "0")) %>% 
  rename_with(tolower) %>% 
  rename(GEOID = county.code, crude_rate = crude.rate)


pivot = pivot_wider(filtered, 
  id_cols = c(county, state, GEOID, population), 
  names_from = cause,
  values_from = c(deaths, crude_rate), 
  names_sep = "_"
)

write.csv(pivot, here("data", "outcome", "CDC_WONDER", "mortality_clean_2024.csv"), row.names = FALSE)
