# downloading and cleaning acs data 
# Sophia

library(tidycensus)
library(dplyr)
library(here)

#loading in total population data for each county
population <- get_acs(geography = "county", 
                      variables = c(pop = "B01003_001"), 
                      output = "wide")

#loading in population by race data for each county
race <- get_acs(geography = "county", 
                variables = c(
                  white = "B02001_002", 
                  black = "B02001_003",
                  native = "B02001_004", 
                  asian = "B02001_005", 
                  other = "B02001_007",
                  hisp_latino = "B02001_005"
                  ), 
                output = "wide")

#loading in median household income in the past 12 months for each county
income <- get_acs(geography = "county", 
                      variables = c(med_hhincome = "B19013_001"), 
                      output = "wide")

#loading in number of people by sex for each county
sex <- get_acs(geography = "county", 
               variables = c(male = "B01001_002", 
                             female = "B01001_026"),
               output = "wide")

#loading in age data and making new columns to reflect preferred groupings
age <- get_acs(table = "B01001",
               geography = "county", 
               output = "wide"
               ) |> 
  mutate(male_under_18E = B01001_003E + B01001_004E + B01001_005E + B01001_006E, 
         male_18to64E = B01001_007E + B01001_008E + B01001_009E + B01001_010E + B01001_011E + B01001_012E + B01001_013E,
         male_over65E = B01001_014E + B01001_015E + B01001_016E, 
         female_under_18E = B01001_018E + B01001_019E + B01001_020E + B01001_021E,
         female_18to64E = B01001_022E + B01001_023E + B01001_024E + B01001_025E + B01001_026E + B01001_027E + B01001_028E,
         female_over65E = B01001_029E + B01001_030E + B01001_031E) |> 
  select(GEOID, NAME, male_under_18E, male_18to64E, male_over65E, female_under_18E, female_18to64E, female_over65E)

#loading in median age data and renaming variables
med_age <- get_acs(table = "B01002",
                geography = "county", 
                output = "wide")|>
  rename(
    median_ageE = B01002_001E,
    median_ageM = B01002_001M,
    median_age_maleE = B01002_002E,
    median_age_male_M = B01002_002M,
    median_age_femaleE = B01002_003E,
    median_age_female_M = B01002_003M
  )

#loading in education data and selecting only the variables of interest 
education <- get_acs(table = "B15003", 
                     geography = "county", 
                     output = "wide") |> 
  select("GEOID", "NAME", "B15003_017E","B15003_017M", "B15003_018E", "B15003_018M",  "B15003_022E", "B15003_022M") |> 
  rename(hs_diplomaE = "B15003_017E",
         hs_diplomaM = "B15003_017M", 
         GED_or_equivE = "B15003_018E",
         GED_or_equivM = "B15003_018M",  
         bachelors_degE = "B15003_022E", 
         bachelors_degM = "B15003_022M"
         )
  
#combining datasets to one demographics set 
demographics <- population |> 
  left_join(insurance, by = c("GEOID" = "GEOID", "NAME" = "NAME")) |> 
  left_join(income, by = c("GEOID" = "GEOID", "NAME" = "NAME")) |>
  left_join(race, by = c("GEOID" = "GEOID", "NAME" = "NAME")) |> 
  left_join(sex, by = c("GEOID" = "GEOID", "NAME" = "NAME")) |>
  left_join(age, by = c("GEOID" = "GEOID", "NAME" = "NAME")) |> 
  left_join(med_age, by = c("GEOID" = "GEOID", "NAME" = "NAME")) |> 
  left_join(education, by = c("GEOID" = "GEOID", "NAME" = "NAME"))
  
write.csv(demographics,"~/Desktop/DSPG/project_work/acs_demographics.csv", row.names = FALSE)
  
=======
dat22 = get_acs(geography = "county", variables = c(
  pop = "B01003_001",
  med_house_income = "B19013_001",
>>>>>>> a090e9bb39f435bf1783b6a6dc8bdd6faac1009d
  
  #race
  white = "B03002_003",
  black = "B03002_004",
  native = "B03002_005",
  asian = "B03002_006",
  island = "B03002_007",
  other = "B03002_008",
  hisp_latino = "B03002_012",
  
  #sex and age
  male_total = "B01001_002",
  male_u5 = "B01001_003",
  male_5_9 = "B01001_004",
  male_10_14 = "B01001_005",
  male_15_17 = "B01001_006",
  male_18_19 = "B01001_007",
  male_20 = "B01001_008",
  male_21 = "B01001_009",
  male_22_24 = "B01001_010",
  male_25_29 = "B01001_011",
  male_30_34 = "B01001_012",
  male_35_39 = "B01001_013",
  male_40_44 = "B01001_014",
  male_45_49 = "B01001_015",
  male_50_54 = "B01001_016",
  male_55_59 = "B01001_017",
  male_60_61 = "B01001_018",
  male_62_64 = "B01001_019",
  male_65_66 = "B01001_020",
  male_67_69 = "B01001_021",
  male_70_74 = "B01001_022",
  male_75_79 = "B01001_023",
  male_80_84 = "B01001_024",
  male_85_plus = "B01001_025",
  female_total = "B01001_026",
  female_u5 = "B01001_027",
  female_5_9 = "B01001_028",
  female_10_14 = "B01001_029",
  female_15_17 = "B01001_030",
  female_18_19 = "B01001_031",
  female_20 = "B01001_032",
  female_21 = "B01001_033",
  female_22_24 = "B01001_034",
  female_25_29 = "B01001_035",
  female_30_34 = "B01001_036",
  female_35_39 = "B01001_037",
  female_40_44 = "B01001_038",
  female_45_49 = "B01001_039",
  female_50_54 = "B01001_040",
  female_55_59 = "B01001_041",
  female_60_61 = "B01001_042",
  female_62_64 = "B01001_043",
  female_65_66 = "B01001_044",
  female_67_69 = "B01001_045",
  female_70_74 = "B01001_046",
  female_75_79 = "B01001_047",
  female_80_84 = "B01001_048",
  female_85_plus = "B01001_049",
  
  #educational attainment
  no_school = "B15003_002",
  mid_school = "B15003_012",
  high_school  = "B15003_017",
  ged = "B15003_018",
  associates = "B15003_021",
  bachelors = "B15003_022",
  masters = "B15003_023",
  doctorate = "B15003_025",
  
  #means of transportation to work
  drove_alone = "B08301_003",
  public_transp = "B08301_010"
  ), 
  output = "wide", year = yr, survey = "acs5")

#manipulation
states = read.csv("states.csv")

filtered <- dat22 %>%
  select(-ends_with("M")) %>%
  rename_with(~ sub("E$", "", .x), ends_with("E")) %>% 
  # create larger age groups
  mutate(
    age_under_18 =
      male_u5 + male_5_9 + male_10_14 + male_15_17 +
      female_u5 + female_5_9 + female_10_14 + female_15_17,
    
    age_18_64 =
      male_18_19 + male_20 + male_21 + male_22_24 +
      male_25_29 + male_30_34 + male_35_39 + male_40_44 +
      male_45_49 + male_50_54 + male_55_59 + male_60_61 +
      male_62_64 +
      female_18_19 + female_20 + female_21 + female_22_24 +
      female_25_29 + female_30_34 + female_35_39 + female_40_44 +
      female_45_49 + female_50_54 + female_55_59 + female_60_61 +
      female_62_64,
    
    age_65_plus =
      male_65_66 + male_67_69 + male_70_74 +
      male_75_79 + male_80_84 + male_85_plus +
      female_65_66 + female_67_69 + female_70_74 +
      female_75_79 + female_80_84 + female_85_plus
  ) %>% 
  # remove smaller age categories (keep total female and male)
  select(
    -matches("^male_(?!total)", perl = TRUE),
    -matches("^female_(?!total)", perl = TRUE)
  ) %>% 
  separate(NAM, c("county", "state"), sep = ", ") %>% 
  mutate(county = gsub(" County", "", county)) %>% 
  left_join(states, by = c("state" = "state_name")) %>% 
  select(-state)

write.csv(filtered, here("data", "outcome", "ACS", "acs2022.csv"), row.names = FALSE)
