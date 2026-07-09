# make filtered dataset for dashboard maps and random forest models

library(dplyr)
library(tigris)
library(sf)

dat <- readRDS("data/analysis/clean_ALL_data.rds")
dash = readRDS("shiny_dashboard/dashboard_data.rds")

# random forest data prep
outcomes <- c(
  "CHR_PCT_MENTAL_DISTRESS",
  "CHR_PCT_LOW_BIRTH_WT",
  "CHR_PCT_ADULT_OBESITY",
  "CDCW_INJURY_DTH_RATE",
  "CDCW_SELFHARM_DTH_RATE",
  "CDCA_STROKE_DTH_RATE_ABOVE35"
)

predictor_topics = c(#"People", "Income", "Attainment", "Health insurance status",
  "Characteristics of health care providers", "Characteristics of health care facilities", "Transportation")

additional_vars <- c(
  "USDA_rural_indicator_2013",
  "FCC_res_connections_10_mbps"
)

rf_predictors <- dash %>% 
  filter(
    Topic %in% predictor_topics |
      Variable.Name %in% additional_vars) %>%
  filter(
    Global.County.Coverage.Level == "Mostly Full Coverage") %>%
  pull(Variable.Name) %>%
  unique()

# Combine outcomes + predictors
rf_vars <- unique(c(outcomes, additional_vars, rf_predictors))

rf_dat <- dat %>% 
  select(YEAR, COUNTYFIPS, all_of(rf_vars)) %>%
  select(YEAR, COUNTYFIPS, USDA_rural_indicator_2013,
         FCC_res_connections_10_mbps, contains("RATE"), contains("PCT")) %>% 
  filter(YEAR > 2009)

saveRDS(
  rf_dat,
  "data/analysis/random_forest_dat_2010_2023.rds"
)


# future dashboard map
# cons = counties(year = 2023, cb = TRUE) %>%
#   select(GEOID, geometry)
# 
# keep_vars = dash %>%
#   filter(Year == 2023, Available == 1,
#          Yearly.County.Coverage.Level == "Mostly Full Coverage")
# 
# map_vars = unique(keep_vars$Variable.Name)
# 
# dat_filt = dat %>%
#   select(YEAR, COUNTYFIPS, COUNTY, all_of(map_vars)) %>%
#   filter(YEAR == 2023) %>%
#   mutate(COUNTYFIPS = sprintf("%05d", as.numeric(COUNTYFIPS))) %>%
#   left_join(
#     cons,
#     by = c("COUNTYFIPS" = "GEOID")
#   ) %>%
#   st_as_sf() %>% 
#   st_transform(4326)
# 
# 
# saveRDS(dat_filt, "shiny_dashboard/clean_2023_filtered_dat.rds")
