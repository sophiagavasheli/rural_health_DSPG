# joining 2022 data

library(dplyr)
library(here)

acs = read.csv(here("data", "outcome", "ACS", "clean_ACS_2022.csv"))
places = read.csv(here("data", "outcome", "CDC_PLACES", "clean_places_2022.csv"))
all_mort = read.csv(here("data", "outcome", "CDC_WONDER", "clean_all_mortality_2022.csv"))
drug_alc_mort = read.csv(here("data", "outcome", "CDC_WONDER", "clean_mortality_drug_alc_2022.csv"))
chr = read.csv(here("data", "outcome", "CHR", "chr2022.csv"))
form477 = read.csv(here("data", "outcome", "FCC_form477", "form477_2022.csv"))
hrsa = read.csv(here("data", "outcome", "HRSA", "clean_HRSA_2022.csv"))

states = read.csv("states.csv")
states_keep <- states$state_abbr

acs <- acs %>% 
  filter(GEOID < 57000) %>% 
  rename(state = state_abbrev)

places <- places %>% 
  filter(GEOID < 57000) %>% 
  rename(state = state_abbrev) %>% 
  select(-year, -totalpopulation, -totalpop18plus, -county, -state)

all_mort <- all_mort %>% 
  filter(GEOID < 57000) %>% 
  select(GEOID, crude_rate)

drug_alc_mort <- drug_alc_mort %>% 
  filter(GEOID < 57000) %>% 
  select(GEOID, starts_with("crude"))

chr <- chr %>% 
  filter(GEOID < 57000) %>% 
  select(GEOID, mental_health_providers)

form477 <- form477 %>% 
  filter(GEOID < 57000) %>% 
  filter(month == 12) %>% 
  rename(county = county_name) %>% 
  select(-housing_units) %>% 
  #keep county level GEOID for DC
  filter(GEOID != 11000) %>% 
  select(-county, -state, -month)

hrsa <- hrsa %>% 
  filter(GEOID < 57000) %>% 
  select(-county, -state, -year)

#figure out some discrepancies in row count mismatch
anti_join(chr, acs, by = "GEOID")
anti_join(hrsa, acs, by = "GEOID")
# connecticut switching from counties to county planning regions, hrsa and chr include those planning regions


joined = acs %>%
  left_join(places, by = c("GEOID" = "GEOID")) %>%
  left_join(all_mort, by = c("GEOID" = "GEOID")) %>%
  left_join(drug_alc_mort, by = c("GEOID" = "GEOID")) %>%
  left_join(chr, by = c("GEOID" = "GEOID")) %>%
  left_join(form477, by = c("GEOID" = "GEOID")) %>%
  left_join(hrsa, by = c("GEOID" = "GEOID"))


library(tidyr)

summary_table <- joined %>%
  select(-GEOID, -county, -state) %>% 
  summarise(across(
    where(is.numeric),
    list(
      mean   = ~mean(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      min    = ~min(.x, na.rm = TRUE),
      max    = ~max(.x, na.rm = TRUE),
      num_NAs   = ~sum(is.na(.x))
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_sep = "__",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

write.csv(joined, here("shiny_dashboard", "join2022.csv"), row.names = FALSE)
#write.csv(summary_table, here("shiny_dashboard", "summary2022.csv"), row.names = FALSE)