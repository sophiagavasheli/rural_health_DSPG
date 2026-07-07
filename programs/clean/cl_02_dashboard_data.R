# creating data for data availability dashboard

library(dplyr)
library(tidyr)
library(stringr)

actual_data <- readRDS("data/analysis/clean_ALL_data.rds")
codebook <- read.csv("reference/all_codebook.csv")

geo_cols <- c(
  "YEAR", "COUNTYFIPS", "STATEFIPS",
  "STATE", "COUNTY", "REGION", "TERRITORY"
)

actual_data <- actual_data %>%
  mutate(across(-all_of(geo_cols), as.numeric))

remove_vars <- c(
  "COUNTY", "COUNTYFIPS", "REGION",
  "STATE", "STATEFIPS", "TERRITORY", "YEAR"
)

total_years <- n_distinct(actual_data$YEAR)


# Variable-level yearly coverage

raw_coverage <- actual_data %>%
  pivot_longer(
    cols = -all_of(geo_cols),
    names_to = "Variable Name",
    values_to = "value"
  ) %>%
  group_by(`Variable Name`, YEAR) %>%
  summarise(
    na_pct = mean(is.na(value)) * 100,
    active_counties = n_distinct(COUNTYFIPS[!is.na(value)]),
    is_present = as.integer(any(!is.na(value))),
    .groups = "drop"
  ) %>%
  mutate(
    coverage_pct = 100 - na_pct,
    
    coverage_level_year = case_when(
      coverage_pct >= 70 ~ "Mostly Full Coverage",
      coverage_pct >= 50 ~ "Partial Coverage",
      TRUE ~ "Little Coverage"
    )
  )


# Global summaries across all years

global_summary <- raw_coverage %>%
  group_by(`Variable Name`) %>%
  summarise(
    years_available = sum(is_present),
    global_county_coverage = mean(coverage_pct),
    .groups = "drop"
  ) %>%
  mutate(
    yearly_availability = case_when(
      years_available == total_years ~ "Full Availability",
      years_available >= 5 ~ "Partial Availability",
      TRUE ~ "Very Little Availability"
    ),
    
    county_coverage_level = case_when(
      global_county_coverage >= 70 ~ "Mostly Full Coverage",
      global_county_coverage >= 50 ~ "Partial Coverage",
      global_county_coverage > 0 ~ "Little Coverage",
      TRUE ~ "Unavailable"
    )
  )


# Create complete variable-year grid
# Ensures every variable has every year represented

all_var_years <- expand_grid(
  `Variable Name` = unique(codebook$Variable.Name),
  YEAR = sort(unique(actual_data$YEAR))
)


# Final dashboard dataset, One row = one variable in one year

dashboard_long <- all_var_years %>%
  
  left_join(
    raw_coverage,
    by = c("Variable Name", "YEAR")
  ) %>%
  
  mutate(
    is_present = coalesce(is_present, 0L),
    active_counties = coalesce(active_counties, 0),
    na_pct = coalesce(na_pct, 100),
    coverage_pct = coalesce(coverage_pct, 0),
    coverage_level_year = coalesce(
      coverage_level_year,
      "Little Coverage"
    )
  ) %>%
  
  left_join(
    codebook,
    by = c("Variable Name" = "Variable.Name")
  ) %>%
  
  left_join(
    global_summary,
    by = "Variable Name"
  ) %>%
  
  mutate(
    #clean domain names
    Domain = str_remove(Domain, "^\\d+\\.\\s*"),
    
    Domain = str_replace(
      Domain,
      "Physical infrastructure",
      "Physical Infrastructure"
    )
  ) %>%
  #remove vars not wanted in dashboard
  filter(
    !`Variable Name` %in% remove_vars,
    Domain != "Identifier"
  ) %>%
  
  rename(
    Year = YEAR,
    Variable.Name = `Variable Name`,
    Available = is_present,
    Active.Counties = active_counties,
    Yearly.County.Coverage.Pct = coverage_pct,
    Yearly.County.Coverage.Level = coverage_level_year,
    Years.Available = years_available,
    Yearly.Availability.Level = yearly_availability,
    Global.County.Coverage.Pct = global_county_coverage,
    Global.County.Coverage.Level = county_coverage_level
  ) %>%
  select(-starts_with("X"), -na_pct) %>% 
  
  arrange(
    Domain,
    Topic,
    Variable.Name,
    Year
  )

saveRDS(dashboard_long, "shiny_dashboard/dashboard_data.rds")

