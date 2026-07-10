# joining OSM health sites and UNC hospitals into one dataset

library(dplyr)
library(sf)

unc = readRDS("data/outcome/UNC_shep/clean_UNC_hosps_all_2023.rds")  %>% st_transform(4326)
osm = readRDS("data/outcome/OSM/clean_health_sites_2023.rds")  %>% st_transform(4326)
counties <- readRDS("data/outcome/census/us_counties_2020.rds") %>% st_transform(4326)


unc_clean = unc %>% 
  select(name, fips, type, location, pos_total_beds) %>% 
  # editing types to match OSM
  mutate(type = case_when(
    type == "long term care" ~ "long_term_care_hospital",
    type == "rehabilitation" ~ "rehab",
    type == "children's" ~ "childrens_hospital",
    type == "psychiatric" ~ "mental_health",
    type == "acute" ~ "acute_care_hospital",
    type == "religious non med"  ~ "religious_non_medical_hospital",
    TRUE ~ type
  )) %>% 
  st_join(
    counties %>% select(GEOID, NAME, STATEFP),
    join = st_within
  ) %>% 
  rename(
    county_fips = GEOID,
    county = NAME,
    health_site_name = name,
    health_site_type = type,
    state_fips = STATEFP,
    address = location,
    hospital_beds = pos_total_beds
  ) %>% 
  select(-fips) %>% 
  mutate(health_site_name = str_to_title(health_site_name),
         address = str_to_title(address))

osm_clean = osm %>% 
  filter(!health_site_type == "hospital")

all = bind_rows(
  unc_clean,
  osm_clean
)

# sites wanted for drive time calculations
sites = c("acute_care_hospital", "doctors_medical_specialists", "mental_health", 
          "dentist", "clinic_urgent_care", "pharmacy")

drive_time_dat = all %>% 
  filter(health_site_type %in% sites) %>% 
  mutate(site_id = row_number()) %>% 
  rename(
    STATEFP = state_fips,
    COUNTYFP = county_fips
  )

health_site_labels <- c(
  long_term_care_hospital = "Long-Term Care Hospital",
  childrens_hospital = "Children's Hospital",
  acute_care_hospital = "Acute Care Hospital",
  religious_non_medical_hospital = "Religious Non-Medical Hospital",
  clinic_urgent_care = "Clinics & Urgent Care",
  doctors_medical_specialists = "Doctors & Medical Specialists",
  specialists_musculoskeletal_pain = "Musculoskeletal & Pain Specialists",
  dentist = "Dentist",
  vision = "Vision Care",
  pharmacy = "Pharmacy",
  mental_health = "Mental Health & Psychiatric Hospitals",
  nursing_home = "Nursing Home",
  rehab = "Rehabilitation Facilities & Hospitals",
  diagnostics = "Diagnostics",
  kidney_care = "Kidney Care",
  specialists_aesthetic = "Aesthetic Specialists",
  other_healthcare = "Other Healthcare"
)

#add labels for map
all = all %>% 
  mutate(
    health_site_label = health_site_labels[health_site_type])

saveRDS(all, "shiny_dashboard/us_health_sites_2023.rds")
saveRDS(drive_time_dat, "data/analysis/drive_time_health_sites_2023.rds")
