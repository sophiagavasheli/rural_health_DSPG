# making filtered dataset for dashboard map

library(dplyr)
library(tigris)
library(sf)
library(tidyr)
library(purrr)
library(rmapshaper)

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

tiger_years <- sort(unique(year_lookup$TIGER_YEAR))

# store county geometry separately to cut down on file size
county_geom <- map_dfr(tiger_years, function(ty) {
  counties(year = ty, cb = TRUE) %>%
    select(GEOID, geometry) %>%
    mutate(TIGER_YEAR = ty)
}) %>%
  st_as_sf() %>%
  st_transform(4326)

# simplify polygons to cut size further
county_geom <- ms_simplify(county_geom, keep = 0.05, keep_shapes = TRUE)

saveRDS(county_geom, "shiny_dashboard/county_geometry.rds")
saveRDS(year_lookup, "shiny_dashboard/year_lookup.rds")

county_grid <- map_dfr(years, function(yr) {
  ty <- year_lookup %>% filter(YEAR == yr) %>% pull(TIGER_YEAR)
  county_geom %>%
    st_drop_geometry() %>%
    filter(TIGER_YEAR == ty) %>%
    transmute(COUNTYFIPS = GEOID, YEAR = yr)
})

map_dat <- county_grid %>%
  left_join(
    dat %>%
      filter(YEAR %in% years) %>%
      transmute(
        YEAR,
        COUNTYFIPS = sprintf("%05d", as.numeric(COUNTYFIPS)),
        COUNTY,
        across(all_of(vars))
      ),
    by = c("YEAR", "COUNTYFIPS")
  )

# split into health and infrastructure data
health_var_names = c(
  "CDCA_HEART_DTH_RATE_ABOVE35",
  "CDCA_STROKE_DTH_RATE_ABOVE35",
  "CDCA_BLOOD_HOSP_RATE_ABOVE65",
  "CDCA_HEART_HOSP_RATE_ABOVE65",
  "CDCW_DRUG_DTH_RATE",
  "CDCW_INJURY_DTH_RATE",
  "CDCW_SELFHARM_DTH_RATE",
  "CDCW_TRANSPORT_DTH_RATE",
  "CHR_PCT_ADULT_OBESITY",
  "CHR_PCT_DIABETES",
  "CHR_PCT_MENTAL_DISTRESS",
  "CHR_PCT_PHYSICAL_DISTRESS",
  "CHR_AVG_LIFE_EXPEC",
  "CHR_PREMAT_DEATH_RATE",
  "CHR_YRS_LIFE_LOST",
  "CHR_PCT_LOW_BIRTH_WT",
  "CDCAP_HIVDIAG_RATE_ABOVE13",
  "CDCP_DEPRESSION_ADULT_A",
  "CDCP_CHOLES_ADULT_A",
  "CDCP_CANCER_ADULT_A",
  "CDCP_ASTHMA_ADULT_A",
  "CDCP_ARTHRITIS_ADULT_A",
  "CHR_PCT_ALCOHOL_DRIV_DEATH",
  "CDCW_crude_death_rate",
  "CHR_TEEN_BIRTH_RATE_15_19"
)

health_vars = available_vars %>% 
  filter(Variable.Name %in% health_var_names) %>% 
  select(-Topic)

inf_var_names = c(
  "AHRF_HOSP_BED_RATE",
  "AHRF_GENRL_SURG_RATE",
  "AHRF_HOSPS_RATE",
  "AHRF_ST_G_HOSP_RATE",
  "AHRF_HOSP_TELE_STROKE_RATE",
  "AHRF_PHYS_PRIMARY_RATE",
  "AHRF_NURSE_PRACT_RATE",
  "AHRF_PHYSICIAN_ASSIST_RATE",
  "AHRF_DENTISTS_RATE",
  "AHRF_PEDIATRICS_RATE",
  "CHR_MENTAL_PROV_RATE",
  "POS_FQHC_RATE",
  "POS_RHC_RATE",
  "POS_HOSPICE_RATE",
  "POS_HHA_RATE",
  "POS_MEAN_DIST_ED",
  "POS_MEAN_DIST_OBSTETRICS",
  "POS_MEAN_DIST_TRAUMA",
  "FCC_res_connections_10_mbps",
  "FCC_res_connections_100_mbps",
  "FCC_county_consumer_connections",
  "ACS_PCT_HU_NO_VEH",
  "CDCP_LACKTRPT_12_MTH_A",
  "AHRF_OB_GYN_RATE",
  "ACS_PCT_DRIVE_2WORK",
  "ACS_PCT_PUBL_TRANSIT",
  "HIFLD_UC_RATE"
)

inf_vars = available_vars %>% 
  filter(Variable.Name %in% inf_var_names) %>% 
  select(-Topic)

health_dat = map_dat %>% 
  select(YEAR, COUNTYFIPS, COUNTY, all_of(health_var_names))


inf_dat = map_dat %>% 
  select(YEAR, COUNTYFIPS, COUNTY, all_of(inf_var_names))

saveRDS(health_dat, "shiny_dashboard/health_map_data.rds")
saveRDS(inf_dat, "shiny_dashboard/infrastructure_map_data.rds")

saveRDS(health_vars, "shiny_dashboard/health_vars.rds")
saveRDS(inf_vars, "shiny_dashboard/infrastructure_vars.rds")

states <- states(year = 2023, cb = TRUE, class = "sf") %>%
  st_transform(4326) %>%
  ms_simplify(keep = 0.05, keep_shapes = TRUE)

saveRDS(states, "shiny_dashboard/states_2023.rds")