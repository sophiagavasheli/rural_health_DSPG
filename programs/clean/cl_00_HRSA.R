# cleaning HRSA Area Health Resources Data
# Sophia

library(here)
library(dplyr)
library(tidyr)
library(stringr)

#most of the data in this release is 2022-2023
dat25 = read.csv(here("data", "source", "HRSA", "AHRF2025.csv"))

#get some 2022-23 data that was not in 2025 release
dat24 = read.csv(here("data", "source", "HRSA", "AHRF2024.csv"))

#select vars from 2025
select25 = dat25 %>% 
  select(fips_st_cnty,st_name_abbrev,cnty_name, hosp_23, hosp_22, lth_psych_23, lth_psych_22, hosp_adm_23, hosp_adm_22, hosp_beds_23, hosp_beds_22, md_nf_fed_23, md_nf_fed_22, do_nf_fed_activ_23, do_nf_fed_activ_22, stgh_ed_vists_23, stgh_ed_vists_22, stnglth_ed_vists_23, stnglth_ed_vists_22, popn_23, popn_22, lo_birth_wgt_3yr_avg_23, lo_birth_wgt_3yr_avg_22, suicide_deth_3yr_23, suicide_deth_3yr_22)

#select from 2024
select24 = dat24 %>% 
  select(fips_st_cnty, comn_mentl_hlth_ctr_23, comn_mentl_hlth_ctr_22, fedly_qualfd_hlth_ctr_23, fedly_qualfd_hlth_ctr_22)

#join
joined = left_join(select25, select24, by = c("fips_st_cnty" = "fips_st_cnty"))

#pivot longer to separate years
long <- joined %>%
  pivot_longer(
    cols = matches("_[0-9]{2}$"),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_([0-9]{2})$"
  ) %>%
  mutate(year = paste0("20", year))

long$fips_st_cnty = as.character(long$fips_st_cnty)

#rename, convert to GEOID
filtered = long %>% 
  mutate(fips_st_cnty = str_pad(fips_st_cnty, width = 5, 
                                side = "left", pad = "0")) %>% 
  rename(GEOID = fips_st_cnty, state = st_name_abbrev, county = cnty_name)

#filter by year
final23 = filtered %>% 
  filter(year == 2023) %>% 
  select(-year)

final22 = filtered %>% 
  filter(year == 2022) %>% 
  select(-year)

write.csv(final23, here("data", "outcome", "HRSA", "hrsa23.csv"), row.names = FALSE)
write.csv(final22, here("data", "outcome", "HRSA", "hrsa22.csv"), row.names = FALSE)