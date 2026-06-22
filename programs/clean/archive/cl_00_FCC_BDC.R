# use fcc api to download 2025 broadband data
# Sophia

library(httr2)
library(dplyr)
library(purrr)
library(here)
library(jsonlite)

#setup
user = ""
api_key = ""

#get list of available data dates
resp <- request("https://bdc.fcc.gov/api/public/map/listAsOfDates") |>
  req_headers(
    username = user,
    hash_value = api_key
  ) |>
  req_perform()

result <- resp_body_json(resp)
#convert to dataframe
dates_avail <- as.data.frame(bind_rows(result$data))


dates <- c("2022-12-31",
           "2023-12-31",
           "2024-12-31",
           "2025-12-31")

#function to loop
download_bdc <- function(date, user, api_key){
  
  message("Processing ", date)
  
  # get list of files
  resp <- request(
    paste0(
      "https://bdc.fcc.gov/api/public/map/downloads/listAvailabilityData/",
      date
    )
  ) |>
    req_headers(
      username = user,
      hash_value = api_key
    ) |>
    req_perform()
  
  avail_result <- resp_body_json(resp)
  
  avail_data <- bind_rows(avail_result$data)
  
  # keep only national summary datasets
  avail_filter <- avail_data %>%
    filter(
      category == "Summary",
      subcategory == "Summary by Geography Type - Other Geographies",
      is.na(state_name)
    )
  
  # fixed and mobile file ids
  fixed_id <- avail_filter %>%
    filter(grepl("fixed", technology_type, ignore.case = TRUE)) %>%
    pull(file_id)
  
  mobile_id <- avail_filter %>%
    filter(grepl("mobile", technology_type, ignore.case = TRUE)) %>%
    pull(file_id)
  
  ids <- tibble(
    type = c("fixed", "mobile"),
    file_id = c(fixed_id, mobile_id)
  )
  
  # download and unzip
  walk2(ids$file_id, ids$type, ~{
    
    zip_path <- here(
      "data", "source", "FCC",
      paste0(.y, "_", date, ".zip")
    )
    
    download_link <- paste0(
      "https://bdc.fcc.gov/api/public/map/downloads/downloadFile/availability/",
      .x,
      "/1"
    )
    
    request(download_link) |>
      req_headers(
        username = user,
        hash_value = api_key
      ) |>
      req_perform(path = zip_path)
    
    unzip(
      zip_path,
      exdir = here(
        "data", "source", "FCC",
        paste0("FCC_BDC_", date)
      )
    )
    
    message("Downloaded ", .y, " for ", date)
  })
}

#run function
walk(
  dates,
  download_bdc,
  user = user,
  api_key = api_key
)