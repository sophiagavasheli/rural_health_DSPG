# cleaning and joining CLH and extra codebooks

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
    values_fn = list(present = ~ first(.)), # Handles any remaining duplicates within the same year
    names_prefix = "X"
    ) %>% 
  mutate(
    `Data Source` = recode(
      `Data Source`,
      ACS = "American Community Survey (ACS)",
      AHRF = "Area Health Resource Files (AHRF)",
      CAF = "County Adjacency File (CAF)",
      CCBP = "Census County Business Patterns (CCBP)",
      CDCAP = "National Center for HIV, Viral Hepatitis, STD, and TB Prevention AtlasPlus (CDCAP)",
      CDCW = "CDC WONDER (Wide-ranging Online Data for Epidemiologic Research) (CDCW)",
      Census = "U.S. Census Bureau, TIGERweb and COVID-19 Demographic and Economic Resources (Census)",
      CHR = "County Health Rankings (CHR)",
      CRE = "Community Resilience Estimates (CRE)",
      EPAA = "Environmental Protection Agency (EPAA)",
      HHC = "Home Health Compare (HHC)",
      MUA = "HRSA Medically Underserved Areas (MUA)",
      IHS = "Indian Health Service (IHS)",
      MMD = "Mapping Medicare Disparities Tool (MMD)",
      MP = "Medicare Advantage State/County Penetration Files (MP)",
      NCHS = "National Center for Health Statistics Urban-Rural Classification Scheme (NCHS)",
      NEPHTN = "National Environmental Public Health Tracking Network (NEPHTN)",
      NHC = "Nursing Home Compare (NHC)",
      NOAAC = "National Oceanic and Atmospheric Administration Climate (NOAAC)",
      NOAAS = "National Oceanic and Atmospheric Administration Storm (NOAAS)",
      PC = "Physician Compare (PC)",
      POS = "Centers for Medicare and Medicaid (CMS) Provider of Services (POS) File",
      SAIPE = "Census Bureau Small Area Income and Poverty Estimates (SAIPE)"
    )
  )

# because of slight label changes across years, pivoting was creating duplicates 
# diagnosed with this code 
# dups = wide_codebook |> 
#   count(`Variable Name`) |> filter(n > 1) 
# 
# vars = wide_codebook %>% 
#   filter(`Variable Name` %in% dups$`Variable Name`)
  
#write.csv(wide_codebook, "reference/CLH_master_codebook.csv", row.names = FALSE)

extra = read.csv("reference/extra_codebook.csv")

extra = extra %>% 
  rename(
    `Variable Name` = Variable.Name,
    `Variable Label` = Variable.Label,
    `Data Type` = Data.Type,
    `Data Source` = Data.Source
  )

final = bind_rows(
  extra,
  wide_codebook
)
  
write.csv(final, "reference/all_codebook.csv", row.names = FALSE)
