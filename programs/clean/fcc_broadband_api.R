# use fcc api to download 2025 broadband data
# Sophia

library(httr2)
library(jsonlite)
library(dplyr)
library(here)

#setup
user = "sopo.gavasheli11@gmail.com"
api_key = "z46IAqvZpVcY9ehkR3HSkaJgjjpCZ3tCbsdyyri1CHE="

#get list of available data dates
resp <- request("https://bdc.fcc.gov/api/public/map/listAsOfDates") |>
  req_headers(
    username = user,
    hash_value = api_key
  ) |>
  req_perform()

result <- resp_body_json(resp)
#convert to dataframe
dates <- as.data.frame(bind_rows(result$data))


#list of available data as of 2025-12-31 (most recent availability data)
resp <- request("https://bdc.fcc.gov/api/public/map/downloads/listAvailabilityData/2025-12-31") |>
  req_headers(
    username = user,
    hash_value = api_key
  ) |>
  req_perform()

avail_result = resp_body_json(resp)
avail_data <- as.data.frame(bind_rows(avail_result$data))

#we want the summary datasets for all states (state == NA)
avail_filter <-  avail_data %>% 
  filter(category == "Summary", is.na(state_name))

#we want the following datasets:
#"bdc_us_fixed_broadband_summary_by_geography_D25_27may2026" 
#"bdc_us_mobile_broadband_summary_by_geography_D25_27may2026"

#pull out the file ids to get download links
fixed_link = "https://bdc.fcc.gov/api/public/map/downloads/downloadFile/availability/1625951/1"
mobile_link = "https://bdc.fcc.gov/api/public/map/downloads/downloadFile/availability/1625952/1"


resp <- request(fixed_link) |>
  req_headers(
    username = user,
    hash_value = api_key
  ) |>
  req_perform(path = here("data", "source", "CHR", "fixed_broadband.zip"))

resp <- request(mobile_link) |>
  req_headers(
    username = user,
    hash_value = api_key
  ) |>
  req_perform(path = here("data", "source", "CHR", "mobile_broadband.zip"))

#unzip
unzip(
  here("data", "source", "CHR","fixed_broadband.zip"),
  exdir = here("data", "source", "CHR",)
)

unzip(
  here("data", "source", "CHR", "mobile_broadband.zip"),
  exdir = here("data", "source", "CHR",)
)
