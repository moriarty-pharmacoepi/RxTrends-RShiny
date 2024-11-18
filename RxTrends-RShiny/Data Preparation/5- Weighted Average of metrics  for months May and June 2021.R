
options(digits = 15)
library(readxl)
library(dplyr)
library(openxlsx)

# Loading data
file_path <- "C:the // path /to /PCRS_Meds_Atc.xlsx" # change it to the path of the file 
data <- read_excel(file_path)

# Filtering data 
meds_data <- data %>% filter(Type == "Medication" & mth_yr %in% c("202105", "202106"))

SystemAtc_data <- data %>% filter(Type %in% c("ATC", "System") & mth_yr %in% c("202105", "202106"))

#Calculating the estimated total prescriptions and total ingredient cost for a month
calculate_total_prescriptions <- function(df) {
  total_percentage <- sum(df$`% Prescribing Frequency (of Scheme Total Frequency)`)
  total_prescriptions <- sum(df$Prescribing_Frequency) / (total_percentage / 100)
  return(total_prescriptions)
}


calculate_total_cost <- function(df) {
  total_percentage_cost <- sum(df$`% Ingredient Cost (of Scheme Total Cost)`)
  total_cost <- sum(df$`Ingredient Cost €`) / (total_percentage_cost / 100)
  return(total_cost)
}

# Processing data for meds_data and SystemAtc_data separately
process_data <- function(data, month1, month2) {
  # Filter data for the specific months
  data_jun <- data %>% filter(mth_yr == month1)
  data_may <- data %>% filter(mth_yr == month2)
  
  # Calculating total prescriptions and costs for each month
  total_prescriptions_jun <- calculate_total_prescriptions(data_jun)
  total_prescriptions_may <- calculate_total_prescriptions(data_may)
  sum_total_prescriptions <- total_prescriptions_jun + total_prescriptions_may
  
  total_cost_jun <- calculate_total_cost(data_jun)
  total_cost_may <- calculate_total_cost(data_may)
  sum_total_cost <- total_cost_jun + total_cost_may
  
  # Calculating average metrics
  average_metrics <- data %>%
    group_by(name) %>%
    summarise(
      mth_yr = first(mth_yr),
      count = n(),
      total_prescribing_frequency = sum(Prescribing_Frequency),
      total_ingredient_cost = sum(`Ingredient Cost €`),
      .groups = 'drop'
    ) %>%
    mutate(
      Prescribing_Frequency_ave = if_else(count > 1, total_prescribing_frequency / 2, total_prescribing_frequency),
      `Ingredient Cost €_ave` = if_else(count > 1, total_ingredient_cost / 2, total_ingredient_cost),
      `ave_% Prescribing Frequency (of Scheme Total Frequency)` = if_else(
        count > 1,
        (Prescribing_Frequency_ave / sum_total_prescriptions) * 100,
        if_else(
          mth_yr == month1,
          (Prescribing_Frequency_ave / (sum_total_prescriptions / 2)) * 100,
          (Prescribing_Frequency_ave / (sum_total_prescriptions / 2)) * 100
        )
      ),
      `ave_% Ingredient Cost (of Scheme Total Cost)` = if_else(
        count > 1,
        (`Ingredient Cost €_ave` / sum_total_cost) * 100,
        if_else(
          mth_yr == month1,
          (`Ingredient Cost €_ave` / (sum_total_cost / 2)) * 100,
          (`Ingredient Cost €_ave` / (sum_total_cost / 2)) * 100
        )
      )
    )
  
  final_data <- data %>%
    left_join(average_metrics, by = "name")
  
  return(final_data)
}

# handling emerged data
reorder_and_rename_columns <- function(df) {
  df <- df %>%
    select(
      name,
      Prescribing_Frequency_ave,
      `ave_% Prescribing Frequency (of Scheme Total Frequency)`,
      `Ingredient Cost €_ave`,
      `ave_% Ingredient Cost (of Scheme Total Cost)`,
      `Prescribing Unit Cost €`,
      dispensing_rate_per_1000,
      cost_rate_per_1000,
      Scheme,
      mth_yr.x,
      Type,
      Filter_scrabe,
      `ATC code`
    ) %>%
    rename(
      `Prescribing Frequency` = Prescribing_Frequency_ave,
      `% Prescribing Frequency (of Scheme Total Frequency)` = `ave_% Prescribing Frequency (of Scheme Total Frequency)`,
      `Ingredient Cost €` = `Ingredient Cost €_ave`,
      `% Ingredient Cost (of Scheme Total Cost)` = `ave_% Ingredient Cost (of Scheme Total Cost)`,
      `Month` = mth_yr.x
    )
  return(df)
}

# Calculating totals and saving  meds_data
total_prescriptions_meds <- calculate_total_prescriptions(meds_data)
total_cost_meds <- calculate_total_cost(meds_data)
final_data_meds <- process_data(meds_data, "202106", "202105")
final_data_meds <- reorder_and_rename_columns(final_data_meds)
output_file_path_meds <- "C:/../ .// /aver_PCRS_Meds.xlsx" # change it to the path of the file 
write.xlsx(final_data_meds, file = output_file_path_meds, rowNames = FALSE)
print(paste("Data saved to", output_file_path_meds))
print(paste("Total Prescriptions for Medications:", total_prescriptions_meds))
print(paste("Total Cost for Medications:", total_cost_meds))

# Calculating totals and saving SystemAtc_data
total_prescriptions_SystemAtc <- calculate_total_prescriptions(SystemAtc_data)
total_cost_SystemAtc <- calculate_total_cost(SystemAtc_data)
final_data_SystemAtc <- process_data(SystemAtc_data, "202106", "202105")
final_data_SystemAtc <- reorder_and_rename_columns(final_data_SystemAtc)
output_file_path_SystemAtc <- "// / /  /aver_PCRS_ATC_System2.xlsx" # change it to the path of the file 
write.xlsx(final_data_SystemAtc, file = output_file_path_SystemAtc, rowNames = FALSE)
print(paste("Data saved to", output_file_path_SystemAtc))
print(paste("Total Prescriptions for ATC/System:", total_prescriptions_SystemAtc))
print(paste("Total Cost for ATC/System:", total_cost_SystemAtc))
# Merge the final dataset with ATCsCoded
final_combined <- bind_rows(final_data_meds, final_data_SystemAtc)
output_file_path_final <- "C:// /final_combined.xlsx" # change it to the path of the file 
write.xlsx(final_combined, file = output_file_path_final, rowNames = FALSE)
