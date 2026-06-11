# downloading and cleaning acs data 

library(tidycensus)
library(dplyr)


#loading in all of the variables from the 2024 ACS5
acs_vars24 <- load_variables(2024, 
               "acs5")
View(acs_vars24)

#loading in variable B27001_001 for total health insurance for each county
insurance <- get_acs(geography = "county", 
                     variables = c(health_insur = "B27001_001"), 
                     output = "wide")

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
  
  
  
  
  


