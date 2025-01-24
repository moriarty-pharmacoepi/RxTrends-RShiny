library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(writexl)
library(lubridate)

# Extracting the value from the last row and last column
extract_total <- function(file_path) {
  sheets <- readxl::excel_sheets(file_path)
  last_sheet <- tail(sheets, n = 1)
  data <- read_excel(file_path, sheet = last_sheet)
  
  
  last_row_index <- max(which(rowSums(!is.na(data)) > 0))
  last_column_index <- max(which(colSums(!is.na(data)) > 0))
  
  
  total <- data[last_row_index, last_column_index]
  

  month <- str_extract(basename(file_path), "\\d+")
  
  
  if(!is.numeric(total)) {
    total <- as.numeric(as.character(total))
  }
  
  return(data.frame(Month = month, Total = total))
}


file_dir <- "C:/ / Raw data"  # Directory containing the Excel files


file_paths <- list.files(path = file_dir, pattern = "\\.xlsx$", full.names = TRUE)


totals_data <- do.call(rbind, lapply(file_paths, extract_total))


print(totals_data)
write.xlsx(totals_data, file = "/path/to/ /GMS eleigibity.xlsx", rowNames = FALSE)



PCRS_prescribing <- read_excel("/path/to/your/final_dataset.xlsx")
eligibility <- read_excel("/path/to/your/GrandTotals.xlsx")



# Prepare the data (convert and format month columns)
PCRS_prescribing <- PCRS_prescribing %>%
  mutate(mth_yr = as.Date(paste0(mth_yr, "01"), format = "%Y%m%d"))  # Append "01" for day

eligibility <- eligibility %>%
  mutate(month = as.Date(paste0(Month, "01"), format = "%Y%m%d"))  # Append "01" for day

# Merge the dataframes to match each medication record with the eligible patient count
merged_data <- left_join(PCRS_prescribing, eligibility, by = c("mth_yr" = "month"))

# Check and conditionally drop the 'month' column if it exists
if("month" %in% names(merged_data)) {
  merged_data <- select(merged_data, -Month)
}

# Optionally, rename the 'Total' column for clarity
merged_data <- rename(merged_data, Eligible_Patients = Total)


write_xlsx(merged_data, "/path/to/PCRS.xlsx")

