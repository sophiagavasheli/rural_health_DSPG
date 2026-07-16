# making filtered dataset for dashboard map

library(dplyr)
library(tigris)
library(sf)
library(tidyr)
library(purrr)
library(stringr)

dat <- readRDS("data/analysis/clean_ALL_data.rds")
dash <- readRDS("shiny_dashboard/dashboard_data.rds")

topics <- c(
  "Characteristics of health care providers",
  "Characteristics of health care facilities",
  "Transportation",
  "Broadband Adoption",
  "Health outcomes"
)

available_vars <- dash %>%
  filter(
    Topic %in% topics,
    Available == 1,
    Yearly.County.Coverage.Level == "Mostly Full Coverage",
    Data.Type == "num"
  ) %>%
  select(Year, Variable.Name, Variable.Label, Topic)

vars <- unique(available_vars$Variable.Name)

years <- sort(unique(dat$YEAR))

# create lookup between data year and TIGER year
year_lookup <- tibble(
  YEAR = years,
  TIGER_YEAR = ifelse(
    years %in% c(2009, 2010, 2011, 2012),
    2013,
    years
  )
)

map_dat <- map_dfr(years, function(yr) {
  
  tiger_yr <- year_lookup %>%
    filter(YEAR == yr) %>%
    pull(TIGER_YEAR)
  
  # use 2013 counties for missing TIGER years
  counties_sf <- counties(year = tiger_yr, cb = TRUE) %>%
    select(GEOID, geometry)
  
  county_grid <- tibble(
    COUNTYFIPS = counties_sf$GEOID,
    YEAR = yr
  )
  
  county_grid %>%
    left_join(
      dat %>%
        filter(YEAR == yr) %>%
        transmute(
          YEAR,
          COUNTYFIPS = sprintf("%05d", as.numeric(COUNTYFIPS)),
          COUNTY,
          across(all_of(vars))
        ),
      by = c("YEAR", "COUNTYFIPS")
    ) %>%
    left_join(
      counties_sf,
      by = c("COUNTYFIPS" = "GEOID")
    )
}) %>%
  st_as_sf() %>%
  st_transform(4326)

# split into health and infrastructure data
health_vars = available_vars %>% 
  filter(Topic == "Health outcomes") %>% 
  filter(!str_detect(Variable.Name, "TOT_")) %>% 
  select(-Topic)

inf_vars = available_vars %>% 
  filter(Topic %in% c("Characteristics of health care providers",
                      "Characteristics of health care facilities",
                      "Transportation","Broadband Adoption")) %>% 
  filter(!str_detect(Variable.Name, "TOT_")) %>% 
  select(-Topic)

health_dat = map_dat %>% 
  select(YEAR, COUNTY, all_of(unique(health_vars$Variable.Name)))


inf_dat = map_dat %>% 
  select(YEAR, COUNTY, all_of(unique(inf_vars$Variable.Name)))


saveRDS(health_dat, "shiny_dashboard/health_map_data.rds")
saveRDS(inf_dat, "shiny_dashboard/infrastructure_map_data.rds")

saveRDS(health_vars, "shiny_dashboard/health_vars.rds")
saveRDS(inf_vars, "shiny_dashboard/infrastructure_vars.rds")