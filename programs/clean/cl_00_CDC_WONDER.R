# cleaning CDC wonder

library(here)
library(dplyr)
library(stringr)
library(tidyr)

# cleaning the drug/alcohol grouped data
mort = read.csv(here("data", "source", "CDC_WONDER", "mortality_drug_alc_2023.csv"), 
na.strings = "Not Available")

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

# look at NA/supressed values
na = pivot %>% 
  summarise(
    across(
      everything(),
      list(
        n_na = ~sum(is.na(.)),
        n_suppressed = ~sum(. == "Suppressed", na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

write.csv(pivot, here("data", "outcome", "CDC_WONDER", "clean_mortality_drug_alc_2023.csv"), row.names = FALSE)

# cleaning all mortality
all = read.csv(here("data", "source", "CDC_WONDER", "all_mortality_2023.csv"), 
                na.strings = "Not Available")

mort$County.Code = as.character(mort$County.Code)

filtered_all = all %>% 
  filter(Notes == "") %>% 
  select(-c(Notes, Crude.Rate.Upper.95..Confidence.Interval, 
            Crude.Rate.Lower.95..Confidence.Interval)) %>% 
  separate(County, into = c("county", "state"), sep = ", ", fill = "right") %>% 
  mutate(county = gsub(" County", "", county)) %>% 
  mutate(County.Code = str_pad(County.Code, width = 5, side = "left", pad = "0")) %>% 
  rename_with(tolower) %>% 
  rename(GEOID = county.code, crude_rate = crude.rate)

write.csv(filtered_all, here("data", "outcome", "CDC_WONDER", "clean_all_mortality_2023.csv"), row.names = FALSE)