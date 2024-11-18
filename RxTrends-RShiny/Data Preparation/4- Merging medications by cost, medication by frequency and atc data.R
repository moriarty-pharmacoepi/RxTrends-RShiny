# Load necessary libraries
library(dplyr)
library(readxl)
library(writexl)

# ReadING the Excel files

PCRSByFreq<- read_excel("path/to/your/medsbyfreq_code.xlsx") # change to the path of the file of medications by frequency 
PCRSByCost <- read_excel("path/to/your/medsbycost_code.xlsx") # change to the path of the file of medications by cost



# Converting and formating  month columns
PCRSByFreq <- PCRSByFreq %>%
  mutate(mth_yr1 = as.Date(paste0(mth_yr, "01"), format = "%Y%m%d"))  

PCRSByCost <- PCRSByCost %>%
  mutate(mth_yr1 = as.Date(paste0(mth_yr, "01"), format = "%Y%m%d")) 

# Merge the datasets
combined_medications <- bind_rows(PCRSByFreq, PCRSByCost)

# Resolving duplicates
final_medications <- combined_medications %>%
  arrange(desc(Filter_scrabe)) %>%  
  group_by(name, mth_yr1) %>%       
  filter(Filter_scrabe == "Frequency" | row_number() == 1) %>%
  ungroup()


final_medications <- final_medications %>%
  select(-name) %>%
  rename(name = `Name with ATC Code`)


medication_merged_file <- "C:/the //desired //path /medsFreq&cost_merged.xlsx" # change to the desired path 
write_xlsx(final_medications, medication_merged_file)


final_medications <- read_excel(medication_merged_file)
ATCsCoded <- read_excel("C:/the // path/ to/ /ATCsCoded_merged.xlsx") # change to the desired path 


final_combined <- bind_rows(final_medications, ATCsCoded)


final_combined_file <- "C:/the path / to /PCRS_Meds_Atc.xlsx" # change to the desired path 
write_xlsx(final_combined, final_combined_file)
