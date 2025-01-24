
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(writexl)
library(lubridate)


file_path <- "/ Replace/ with / the  actual file path/cpiall.xlsx"  
  
df <- read_excel(file_path)


# Convert Month format to "Jan 2016", "Feb 2016", etc.
df <- df %>%
  mutate(Month = format(parse_date_time(Month, orders = "Y B"), "%b %Y"))



Oct_2024_value <- df %>%
  filter(Month == "Oct 2024") %>%
  pull(VALUE)

# Using Oct 2024 as the reference (i.e., Oct 2024 = 100)
df <- df %>%
  mutate(CPIallitems = (VALUE / Oct_2024_value) * 100)


df <- df %>%
  mutate(percentage_changeALL = (CPIallitems / 100 - 1) * 100)


df <- df %>%
  mutate(`Selected Base Reference Period` = "Base Oct 2024=100")


print(df)


output_file_path <- "/ Replace/ with / the  actual file path/infla_Oct24Refffallitems.xlsx" 
write_xlsx(df, output_file_path)


file_path <- "/ Replace/ with / the  actual file path/cpidrugs.xlsx"  
df <- read_excel(file_path)



df <- df %>%
  mutate(Month = format(parse_date_time(Month, orders = "Y B"), "%b %Y"))


Oct_2024_value <- df %>%
  filter(Month == "Oct 2024") %>%
  pull(VALUE)


df <- df %>%
  mutate(CPImeds = (VALUE / Oct_2024_value) * 100)


df <- df %>%
  mutate(percentage_changeMeds = (CPImeds / 100 - 1) * 100)


df <- df %>%
  mutate(`Selected Base Reference Period` = "Base Oct 2024=100")


print(df)


output_file_path <- "/ Replace/ with / the  actual file path/infla_Oct24Refffmeds.xlsx"  
write_xlsx(df, output_file_path)



PCRS_prescribing <- read_excel("/ Replace/ with / the  actual file path/PCRSunadj.xlsx")
CPIall_data <- read_excel("/ Replace/ with / the  actual file path/infla_Oct24Refffallitems.xlsx")


PCRS_prescribing <- PCRS_prescribing %>%
  mutate(mth_yr = format(as.Date(paste0(mth_yr, "01"), format = "%Y%m%d"), "%Y%m"))


CPIall_data <- CPIall_data %>%
  mutate(month = format(parse_date_time(Month, orders = "b Y"), "%Y%m"))


print(head(PCRS_prescribing$mth_yr))
print(head(CPIall_data$month))


merged_data <- left_join(PCRS_prescribing, CPIall_data, by = c("mth_yr" = "month"))

# Define the CPI reference value (e.g., for the base year)
CPI_reference <- 100  

# Create the Adjusted_Ingredient Cost € column
merged_data <- merged_data %>%
  mutate(
    `Adjusted_Ingredient Cost €` = `Ingredient Cost €` * ( CPI_reference / CPIallitems),
    `adjusted_Prescribing Unit Cost €` = `Adjusted_Ingredient Cost €` / `Prescribing_Frequency`,
    `adjusted_cost_rate_per_1000` = (`Adjusted_Ingredient Cost €` / `eligible_number`) * 1000
  )

 
  
    print(colnames(merged_data))
    
  merged_data <- merged_data %>%
    select(
      everything(),                                                      # Include all other columns afterward
      `Ingredient Cost €`, `Adjusted_Ingredient Cost €`,               # Move Adjusted_Ingredient Cost € after Ingredient Cost €
      `Prescribing Unit Cost €`, `adjusted_Prescribing Unit Cost €`,    # Move adjusted_Prescribing Unit Cost € after Prescribing Unit Cost €
      `cost_rate_per_1000`, `adjusted_cost_rate_per_1000`               # Move adjusted_cost_rate_per_1000 after cost_rate_per_1000
    )
  
  

  merged_data <- merged_data %>%
    select(-matches("STATISTIC Label"), -Month, -`Selected Base Reference Period`, -UNIT, -VALUE)

write_xlsx(merged_data, "/ Replace/ with / the  actual file path/PCRS_cpiall.xlsx")


PCRS_prescribing2 <- read_excel("/ Replace/ with / the  actual file path/PCRS_cpiall.xlsx")
CPImeds_data <- read_excel("/ Replace/ with / the  actual file path/infla_Oct24Refffmeds.xlsx")


PCRS_prescribing2 <- PCRS_prescribing2 %>%
  mutate(mth_yr = format(as.Date(paste0(mth_yr, "01"), format = "%Y%m%d"), "%Y%m"))


CPImeds_data <- CPImeds_data %>%
  mutate(month = format(parse_date_time(Month, orders = "b Y"), "%Y%m"))

print(head(PCRS_prescribing2$mth_yr))
print(head(CPImeds_data$month))


merged_data2 <- left_join(PCRS_prescribing2, CPImeds_data, by = c("mth_yr" = "month"))


CPI_reference <- 100  


merged_data2 <- merged_data2 %>%
  mutate(
    `Adjustedmeds_Ingredient Cost €` = `Ingredient Cost €` * (CPI_reference / CPImeds),  
    `adjustedmeds_Prescribing Unit Cost €` = `Adjustedmeds_Ingredient Cost €` / `Prescribing_Frequency`,
    `adjustedmeds_cost_rate_per_1000` = (`Adjustedmeds_Ingredient Cost €` / `eligible_number`) * 1000
  )


print(head(merged_data2))


merged_data2 <- merged_data2 %>%
  select(
    everything(),                                                      
    `Ingredient Cost €`, `Adjustedmeds_Ingredient Cost €`,               
    `Prescribing Unit Cost €`, `adjustedmeds_Prescribing Unit Cost €`,    
    `cost_rate_per_1000`, `adjustedmeds_cost_rate_per_1000`               
  )



merged_data2 <- merged_data2 %>%
  select(-matches("STATISTIC Label"), -Month, -`Selected Base Reference Period`, -UNIT, -VALUE)

# Save the final dataframe to an Excel file
write_xlsx(merged_data2, "/ Replace/ with / the  actual file path/PCRSallsmeds.xlsx")