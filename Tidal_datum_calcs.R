# Date: 7/15/24
# Author: Julie Gonzalez
# The purpose of this code is to streamline tidal datum calculations

# Load packages
library(dplyr)
library(tidyr)
library(lubridate)

#### OPTION 1 ####
# Define a function to calculate tidal datums
calculate_tidal_datums <- function(data, time_window = 3 * 3600, datetime_format = "%m/%d/%y %H:%M") { 
  # 3 hours in seconds
  # Convert datetime to POSIXct for easier manipulation
  data <- data %>%
    mutate(datetime = as.POSIXct(datetime, format = datetime_format))
  
  # Add new columns for high and low tides
  data <- data %>%
    mutate(high_tide = NA,
           low_tide = NA)
  
  # Extract high and low tides
  for (i in 1:nrow(data)) {
    # Define the time range for the current record
    time_current <- data$datetime[i]
    past_range <- data %>% filter(datetime >= time_current - time_window & datetime < time_current)
    future_range <- data %>% filter(datetime > time_current & datetime <= time_current + time_window)
    
    # Ensure past_range and future_range are not empty before proceeding
    if (nrow(past_range) > 0 & nrow(future_range) > 0) {
      past_max <- max(past_range$level_m_corrected, na.rm = TRUE)
      future_max <- max(future_range$level_m_corrected, na.rm = TRUE)
      past_min <- min(past_range$level_m_corrected, na.rm = TRUE)
      future_min <- min(future_range$level_m_corrected, na.rm = TRUE)
      
      # Extract high tide
      if (!is.infinite(past_max) & !is.infinite(future_max)) {
        if (data$level_m_corrected[i] > past_max & data$level_m_corrected[i] >= future_max) {
          data$high_tide[i] <- data$level_m_corrected[i]
        }
      }
      
      # Extract low tide
      if (!is.infinite(past_min) & !is.infinite(future_min)) {
        if (data$level_m_corrected[i] < past_min & data$level_m_corrected[i] <= future_min) {
          data$low_tide[i] <- data$level_m_corrected[i]
        }
      }
    }
  }
  
  # Debug print to check extracted high and low tides
  print("Extracted high and low tides:")
  print(data %>% filter(!is.na(high_tide) | !is.na(low_tide)))
  
  # Create a copy of the dataframe for filtering out high and low tide results
  data_filtered <- data
  
  # Insert blank columns for "tide_wse-H" and "tide_wse-L"
  data_filtered <- data_filtered %>%
    mutate(tide_wse_H = ifelse(!is.na(high_tide), high_tide, NA),
           tide_wse_L = ifelse(!is.na(low_tide), low_tide, NA))
  
  # Debug print to check tide_wse_H and tide_wse_L columns
  print("Columns for tide_wse_H and tide_wse_L:")
  print(data_filtered %>% filter(!is.na(tide_wse_H) | !is.na(tide_wse_L)))
  
  # Delete extraction formula columns as no longer needed
  data_filtered <- data_filtered %>%
    select(-high_tide, -low_tide)
  
  # Set "tide_wse_H/L" fields equal to the water level values in original water_level column
  data_filtered <- data_filtered %>%
    mutate(tide_wse_H = ifelse(!is.na(tide_wse_H), level_m_corrected, tide_wse_H),
           tide_wse_L = ifelse(!is.na(tide_wse_L), level_m_corrected, tide_wse_L))
  
  # Sort data on high tide extraction, low tide extraction, and datetime
  data_sorted <- data_filtered %>%
    arrange(desc(tide_wse_H), desc(tide_wse_L), datetime)
  
  # Debug print to check sorted data
  print("Sorted data:")
  print(data_sorted)
  
  # Resort data subset of high/low tides by datetime
  data_final <- data_sorted %>%
    filter(!is.na(tide_wse_H) | !is.na(tide_wse_L)) %>%
    arrange(datetime)
  
  # QA/QC results
  # Check for the number of tides per day
  # This is not working for some reason, not counting the correct amt of highs and lows per day, need to check
  data_final <- data_final %>%
    mutate(date = as.Date(datetime)) %>%
    group_by(date) %>%
    summarise(high_tides = sum(!is.na(tide_wse_H)),
              low_tides = sum(!is.na(tide_wse_L)))
  
  # Debug print to check final result
  print("Final tidal datums:")
  print(data_final)
  
  return(data_final)
}


# Example usage:
# Assuming your dataset is in a CSV file named "water_level_data.csv"
# and it has columns "date_time" and "water_level"

# Load your dataset
water_level_data1 <- read.csv("638045/Gallinas_WL_data_Jul2023-24_20240715_JAG.csv")

water_level_data <- water_level_data1 %>% 
  filter(level_m_corrected != "")

water_level_data1 <- read.csv("MWNW_WL_data_20240717_JAG.csv")

water_level_data <- water_level_data1 %>% 
  filter(water_level_NAVD88 != "") %>% 
  rename(level_m_corrected = water_level_NAVD88)

# Specify the format of your datetime strings
datetime_format <- "%m/%d/%y %H:%M"

# Calculate tidal datums
tidal_datums <- calculate_tidal_datums(water_level_data, datetime_format = datetime_format)

print(tidal_datums)

# Export the final data to a CSV file
write.csv(tidal_datums, "Output/final_tides_data_MWNW_QAQC_20240717_JAG.csv", row.names = FALSE)

##################

