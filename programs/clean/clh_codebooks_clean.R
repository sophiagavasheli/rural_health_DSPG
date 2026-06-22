# cleaning and joining clh codebooks

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# get all xlsx files in  folder
files <- list.files(
  path = "reference/CLH_codebooks",
  pattern = "\\.xlsx$",
  full.names = TRUE
)


# join all
master_codebook <- lapply(files, function(f) {
  
  yr <- str_extract(basename(f), "\\d{4}")
  
  read_excel(f, sheet = "County") |>
    mutate(year = yr)
  
}) |>
  bind_rows() %>% 
  select(`Variable Name`, `Variable Label`, Domain, Topic, `Data Source`, 
         `Type of Data (Numeric or Character)`, year) %>% 
  rename(`Data Type` = `Type of Data (Numeric or Character)`)



# pivot wider
wide_codebook <- master_codebook |>
  # 1. Sort by year descending so the most recent metadata comes first
  arrange(`Variable Name`, desc(year)) |>
  
  # 2. Group by Variable Name to clean up metadata differences
  group_by(`Variable Name`) |>
  mutate(
    # Keep the most recent valid label/metadata for this variable
    `Variable Label` = first(`Variable Label`),
    Domain           = first(Domain),
    Topic            = first(Topic),
    `Data Source`    = first(`Data Source`),
    `Data Type`      = first(`Data Type`)
  ) |>
  ungroup() |>
  
  # 3. Mark presence and pivot
  mutate(present = "X") |>
  pivot_wider(
    names_from = year,
    values_from = present,
    values_fn = list(present = ~ first(.)) # Handles any remaining duplicates within the same year
  )

# because of slight label changes across years, pivoting was creating duplicates 
# diagnosed with this code 
# dups = wide_codebook |> 
#   count(`Variable Name`) |> filter(n > 1) 
# 
# vars = wide_codebook %>% 
#   filter(`Variable Name` %in% dups$`Variable Name`)
  
write.csv(wide_codebook, "reference/CLH_master_codebook.csv", row.names = FALSE)
