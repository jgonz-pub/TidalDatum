# Date: 7/15/24
# Author: Julie Gonzalez
# The purpose of this code is to streamline tidal datum calculations

# Load packages
library(dplyr)
library(tidyr)

# Define a function to calculate tidal datums
calculate_tidal_datums <- function(data, time_window = 3 * 3600) { # 3 hours in seconds
  # Convert datetime to POSIXct for easier manipulation
  data <- data %>%
    mutate(datetime = as.POSIXct(datetime))
  
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
    
    # Extract high tide
    if (nrow(past_range) > 0 & nrow(future_range) > 0) {
      if (data$water_level[i] > max(past_range$water_level) & data$water_level[i] >= max(future_range$water_level)) {
        data$high_tide[i] <- data$water_level[i]
      } else {
        data$high_tide[i] <- FALSE
      }
      
      # Extract low tide
      if (data$water_level[i] < min(past_range$water_level) & data$water_level[i] <= min(future_range$water_level)) {
        data$low_tide[i] <- data$water_level[i]
      } else {
        data$low_tide[i] <- FALSE
      }
    }
  }
  
  # Create a copy of the dataframe for filtering out high and low tide results
  data_filtered <- data
  
  # Insert blank columns for "tide_wse-H" and "tide_wse-L"
  data_filtered <- data_filtered %>%
    mutate(tide_wse_H = ifelse(!is.na(high_tide), high_tide, NA),
           tide_wse_L = ifelse(!is.na(low_tide), low_tide, NA))
  
  # Delete extraction formula columns as no longer needed
  data_filtered <- data_filtered %>%
    select(-high_tide, -low_tide)
  
  # Set "tide_wse_H/L" fields equal to the water level values in original water_level column
  data_filtered <- data_filtered %>%
    mutate(tide_wse_H = ifelse(!is.na(tide_wse_H), water_level, tide_wse_H),
           tide_wse_L = ifelse(!is.na(tide_wse_L), water_level, tide_wse_L))
  
  # Sort data on high tide extraction, low tide extraction, and datetime
  data_sorted <- data_filtered %>%
    arrange(desc(tide_wse_H), desc(tide_wse_L), datetime)
  
  # Resort data subset of high/low tides by datetime
  data_final <- data_sorted %>%
    filter(!is.na(tide_wse_H) | !is.na(tide_wse_L)) %>%
    arrange(datetime)
  
  # QA/QC results
  # Check for the number of tides per day
  data_final <- data_final %>%
    mutate(date = as.Date(datetime)) %>%
    group_by(date) %>%
    summarise(high_tides = sum(!is.na(tide_wse_H)),
              low_tides = sum(!is.na(tide_wse_L)))
  
  return(data_final)
}

# Example usage:
# Assuming your dataset is in a CSV file named "water_level_data.csv"
# and it has columns "date_time" and "water_level"

# Load your dataset
water_level_data <- read.csv("water_level_data.csv")

# Calculate tidal datums
tidal_datums <- calculate_tidal_datums(water_level_data)
