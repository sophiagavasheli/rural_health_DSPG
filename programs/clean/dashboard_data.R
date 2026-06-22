library(dplyr)
library(tidyr)
library(stringr)

actual_data = read.csv("data/outcome/CLH/clean_CLH2009_2023.csv")
codebook = read.csv("reference/CLH_master_codebook.csv")


geo_cols <- c("YEAR", "COUNTYFIPS", "STATEFIPS", "STATE", "COUNTY", "REGION", "TERRITORY")
total_years <- n_distinct(actual_data$YEAR)


# 1. Compute granular metrics per variable per year
raw_coverage <- actual_data |>
  select(all_of(geo_cols), everything()) |>
  pivot_longer(
    cols = -all_of(geo_cols), 
    names_to = "Variable Name", 
    values_to = "value"
  ) |>
  group_by(`Variable Name`, YEAR) |>
  summarise(
    na_pct = mean(is.na(value)),
    active_counties = n_distinct(COUNTYFIPS[!is.na(value)]),
    is_present = ifelse(any(!is.na(value)), 1, 0),
    .groups = "drop"
  )

# 2. Compute the new global summary rules across ALL years
global_summary <- raw_coverage |>
  group_by(`Variable Name`) |>
  summarise(
    years_available = sum(is_present),
    # County coverage represented as overall non-NA percentage across time
    global_county_coverage = 1 - mean(na_pct),
    .groups = "drop"
  ) |>
  mutate(
    availability_cat = case_when(
      years_available == total_years ~ "Full Availability",
      years_available >= 5           ~ "Partial",
      TRUE                           ~ "Very Little"
    )
  )

# 3. Shape year-by-year metrics for the popup layout
wide_metrics <- raw_coverage |>
  pivot_wider(
    id_cols = "Variable Name",
    names_from = YEAR,
    values_from = c(na_pct, active_counties),
    names_glue = "{YEAR}_{.value}"
  )

# 4. Consolidate into the final dashboard dataset
dashboard_data <- codebook |>
  left_join(global_summary, by = c("Variable.Name" = "Variable Name")) |>
  left_join(wide_metrics, by = c("Variable.Name" = "Variable Name")) %>% 
  mutate(Domain  = gsub("^\\d+\\.\\s*", "", Domain)) %>% 
  mutate(Domain  = gsub("Physical infrastructure", "Physical Infrastructure", Domain)) %>% 
  filter(!Domain == "Identifier")

#don't care about the availability of these vars
remove_vars = c("COUNTY", "COUNTYFIPS", "REGION", "STATE", "STATEFIPS",
                "TERRITORY", "YEAR")

# pivoting table longer
id_cols <- c(
  "Variable.Name", "Variable.Label", "Domain",
  "Topic", "Data.Source", "Data.Type", "availability_cat"
)

base <- dashboard_data %>%
  select(all_of(id_cols))

avail <- dashboard_data %>%
  select(all_of(id_cols), starts_with("X")) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    names_prefix = "X",
    values_to = "available"
  ) %>%
  mutate(year = as.integer(year))

na <- dashboard_data %>%
  select(all_of(id_cols), ends_with("_na_pct")) %>%
  pivot_longer(
    cols = ends_with("_na_pct"),
    names_to = "year",
    names_pattern = "(\\d{4})_na_pct",
    values_to = "na_pct"
  ) %>%
  mutate(year = as.integer(year))


active <- dashboard_data %>%
  select(all_of(id_cols), ends_with("_active_counties")) %>%
  pivot_longer(
    cols = ends_with("_active_counties"),
    names_to = "year",
    names_pattern = "(\\d{4})_active_counties",
    values_to = "active_counties"
  ) %>%
  mutate(year = as.integer(year))

long_data <- avail %>%
  left_join(na, by = c(id_cols, "year")) %>%
  left_join(active, by = c(id_cols, "year")) %>% 
  filter(!Variable.Name %in% remove_vars) %>% 
  mutate(year = as.integer(year)) %>% 
  rename(Year = year)

write.csv(long_data, "reference/dashboard_data.csv", row.names = FALSE)
