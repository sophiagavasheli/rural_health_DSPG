#cleaning County Health rankings 2025 downloaded data
# Sophia

library(readxl)
library(here)

chr = read_excel(here("data", "source", "chr", "chr2025.xlsx"), 
                 sheet = "Select Measure Data", #sheet with the data
                 skip=1) #skip the first row

#replace whitespace with underscore
#replace % with Pct and # with Num
colnames(chr) <- colnames(chr) |>
  gsub("\\s+", "_", x = _) |>
  gsub("%", "Pct", x = _) |>
  gsub("#", "Num", x=_)

#select useful columns and remove rows with state summaries
library(dplyr)
select_chr <- chr %>%
  select("FIPS", "State","County", "Years_of_Potential_Life_Lost_Rate",
         "Average_Number_of_Mentally_Unhealthy_Days", "Primary_Care_Physicians_Rate",
         "Mental_Health_Provider_Rate","Dentist_Rate", "Preventable_Hospitalization_Rate",
         "Pct_with_Annual_Mammogram", "Pct_Uninsured", "Pct_Households_with_Broadband_Access",
         "Num_Completed_High_School") %>% 
  filter(!is.na(County))


#write to csv
#write.csv(select_chr, "cleaned_data/cleanCHR25.csv", row.names = FALSE)
