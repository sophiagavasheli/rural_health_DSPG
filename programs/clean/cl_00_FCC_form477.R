#cleaning FCC form 477
# Sophia 

library(here)
library(dplyr)

#cleaning 2014-2025 data
form2014 = read.csv(here("data", "source", "FCC", "FCC_form_477_county_tiers2014_2025.csv"))

# state name to state abbrev lookup table
states = read.csv("states.csv")

clean_2014 = form2014 %>%
  #convert to UTF characters
  mutate(County_Name = iconv(County_Name, from = "",to = "UTF-8",sub = "")) %>% 
  select(-c(State, County)) %>% 
  #convert state name to state abbreviation
  left_join(states, by = c("State_Name" = "state_name")) %>%
  rename(state = state_abbrev) %>% 
  #all col names to lower case
  rename_with(tolower) %>% 
  select(-c(state_name, housing_units)) %>% 
  #keep more recent month
  filter(month == 12) %>% 
  select(-month) %>% 
  rename(
    FCC_res_connections_200_kbps = tier_1,
    FCC_res_connections_10_mbps = tier_2,
    FCC_res_connections_25_mbps = tier_3,
    FCC_res_connections_100_mbps = tier_4
  ) %>% 
  filter(year <= 2023) #health data ends 2023


#cleaning 2008-2013 data
form2008 = read.csv(here("data", "source", "FCC", "FCC_form_477_county_tiers2008_2013.csv"))

clean_2008 = form2008 %>%
  #convert to UTF characters
  mutate(County_Name = iconv(County_Name, from = "",to = "UTF-8",sub = "")) %>% 
  select(-c(State, County)) %>% 
  #convert state name to state abbreviation
  left_join(states, by = c("State_Name" = "state_name")) %>%
  rename(state = state_abbrev) %>% 
  #all col names to lower case
  rename_with(tolower) %>% 
  select(-c(state_name, housing_units)) %>% 
  #keep more recent month
  group_by(fips, year) %>%
  slice_max(order_by = month, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(-month) %>% 
  rename(
    FCC_res_connections_200_kbps = tier_1,
    FCC_res_connections_768_kbps = tier_2,
    FCC_res_connections_3_mbps = tier_3,
    FCC_res_connections_10_mbps = tier_4
  ) %>% 
  filter(!year == 2008) #only have health data starting 2009

# clean fcc county connections
conn = read.csv(here("data", "source", "FCC", "FCC_form477_county_connections2009_2025.csv"))

clean_conn  = conn %>% 
  # all col names to lower case
  rename_with(tolower) %>% 
  select(-c(statename, countyname)) %>% 
  filter(year <= 2023) %>%
  # keep most recent month for each county-year
  group_by(countycode, year) %>%
  slice_max(order_by = month, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-month) %>% 
  mutate(
    across(where(is.numeric), ~ na_if(., -9999)),
    countycode = sprintf("%05d", as.integer(countycode))
  ) %>% 
  rename(
    FCC_county_consumer_connections = consumer,
    FCC_county_non_consumer_connections = non_consumer,
    FCC_county_all_connections = all,
    fips = countycode
  )


all <- bind_rows(
  clean_2008,
  clean_2014
)

all <- all %>%
  mutate(across(where(is.numeric), ~ na_if(., -999))) %>% 
  mutate(fips = sprintf("%05d", as.integer(fips)))

joined = left_join(all, clean_conn, 
                   by = c("fips" = "fips", "year" = "year"))


write.csv(joined, "data/outcome/FCC/clean_FCC_form477_2009_2023.csv", row.names = FALSE)
