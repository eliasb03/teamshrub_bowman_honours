#------------------------------
# teamshrub_bowman_honours
# 04_climateNA_processing_sequential
# By: Elias Bowman
# Created: 2024-11-17
# Last Updated: 2025-01-18
#
# Description: This script processes climate data for Qikiqtaruk using ClimateNA.
# It processes outputs to create a combined dataframe of climate variables for a specified year period.
# This version of the script runs the ClimateNA analysis sequentially.
#------------------------------

# Import Packages ####
library(tidyverse)
library(ClimateNAr)

# Helper Functions ####

# Function to create input dataframe
create_input_df <- function(lat, lon, elev) {
  message("Creating input dataframe with latitude, longitude, and elevation.")
  data.frame(
    ID1 = c(1, 2),
    ID2 = c(1, 2),
    lat = c(lat, 1),
    lon = c(lon, 1),
    elev = c(elev, 1)
  )
}

# Function to run ClimateNA for all periods sequentially
run_climateNA_sequential <- function(input_file, periods, variables, output_dir) {
  message("Starting sequential ClimateNA runs.")
    climateNAr(
      inputFile = input_file,
      periodList = periods,
      varList = variables,
      outDir = output_dir
    )
  }
  message("All ClimateNA runs completed sequentially.")


# Function to process ClimateNA output files
process_climateNA_files <- function(directory) {
  message("Processing ClimateNA output files in directory: ", directory)
  # List all CSV files in the directory
  file_list <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  message("Found ", length(file_list), " output files.")
  
  # Initialize an empty list to store dataframes
  data_list <- list()
  
  # Loop through each file
  for (file in file_list) {
    # Extract the year from the file name (assuming it's a 4-digit number)
    year <- str_extract(basename(file), "\\d{4}")
    message("Processing file for year: ", year)
    
    # Read the CSV file
    file_data <- read_csv(file, col_names = TRUE, show_col_types = FALSE)
    
    # Select the first row and convert to a dataframe
    value_row <- file_data[1, ] %>%
      as_tibble() %>%
      mutate(year = as.numeric(year)) # Add the year column
    
    # Append to the list
    data_list[[file]] <- value_row
  }
  
  # Combine all rows into a single dataframe
  final_data <- bind_rows(data_list) %>%
    select(-ID1, -ID2) # Remove the ID columns
#  %>%    select(year, everything()) # Move the Year column to the front
  message("ClimateNA output files successfully processed.")
  return(final_data)
}

# Main Script ####

message("Starting ClimateNA processing script.")

# Specify parameters
latitude <- 69.5723 
longitude <- -138.9064
elevation <- 2 # elevation in metres of site

# Create input dataframe
input_df <- create_input_df(latitude, longitude, elevation)

# Define file paths
input_file_path <- "data/raw/climateNA/climateNA_input.csv"
output_dir <- "data/raw/climateNA/output/"

##############################
# ClimateNA Processing
##############################
# Write input file
write_csv(input_df, input_file_path)
message("Input file written to: ", input_file_path)

# Define year range and climate variables
year_range <- 1985:2023
year_inputs <- paste0("Year_", year_range, ".ann")
clim_var_list <- c("Tave_sp", "Tmin_sp", "Tmax_sp", "Tave_sm", "Tmin_sm", "Tmax_sm", 
                   "Tave04", "Tave05", "Tave06", "Tave07", "Tave08")

message("Year range: ", min(year_range), " to ", max(year_range))
message("Climate variables to process: ", paste(clim_var_list, collapse = ", "))
message("Output directory: ", output_dir)

# Run ClimateNA sequentially
run_climateNA_sequential(input_file_path, year_inputs, clim_var_list, output_dir)

# Process ClimateNA output files
climate_dataNA <- process_climateNA_files(output_dir)

# Remove repeated rows
climate_dataNA <- climate_dataNA[!duplicated(climate_dataNA), ]

message("ClimateNA processing script completed successfully.")

#########################
# Komakuk Climate Station
#########################
# Bash code to download komakuk daily weather data
# KOMAKUK BEACH
# for year in $(seq 1995 2025); do
# wget --content-disposition "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=10822&Year=${year}&Month=5&Day=14&timeframe=2&submit= Download+Data";
# done;
# KOMAKUK BEACH A
# for year in $(seq 1985 1993); do
# wget --content-disposition "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=1568&Year=${year}&Month=5&Day=14&timeframe=2&submit= Download+Data";
# done;

station_data_dir <- "data/raw/komakuk_climate_data/"

# Function to read, mutate, and combine climate data
join_climate_data <- function(data_dir) {
  # Get list of climate files
  climate_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize an empty list to store mutated dataframes
  data_list <- list()
  
  # Loop through each file and process the data
  for (file in climate_files) {
    df <- read_csv(file)
    
    # Rename columns to remove spaces
    colnames(df) <- gsub("[[:space:]]", "_", colnames(df))  # Replace spaces with underscores
    colnames(df) <- gsub("[()°/]", "", colnames(df))          # Remove parentheses and degree symbols
    
    
    
    # Mutate to create new columns based on existing ones
    df_new <- df %>%
      mutate(
        longitude = as.numeric(Longitude_x),  
        latitude = as.numeric(Latitude_y),
        stn_name = Station_Name,
        date_time = ymd(DateTime),
        year = as.numeric(Year),
        month = as.numeric(Month),
        day = as.numeric(Day),
        mean_temp = as.numeric(Mean_Temp_C),
        max_temp = as.numeric(Max_Temp_C),
        min_temp = as.numeric(Min_Temp_C)
      ) %>%
      select(longitude, latitude, stn_name, date_time, year, month, day, mean_temp, max_temp, min_temp)
    
    
    data_list[[file]] <- df_new
  }
  
  # Combine all mutated dataframes into one
  combined_data <- bind_rows(data_list)
  
  return(combined_data)
}

komakuk_data <- join_climate_data(station_data_dir)

# averaging to the monthly level
komakuk_monthly <- komakuk_data %>%
  group_by(year, month) %>%
  summarise(
    mean_temp = mean(mean_temp, na.rm = TRUE),
    max_temp = mean(max_temp, na.rm = TRUE),
    min_temp = mean(min_temp, na.rm = TRUE)
  )

komakuk_breed_season <- komakuk_data %>%
  # Filter for the date range (April 15 to June 30)
  filter(date_time >= as.Date(paste(year, 4, 15, sep = "-")) & date_time <= as.Date(paste(year, 5, 30, sep = "-"))) %>%
  group_by(year) %>%
  summarise(
    count_mean_temp = sum(!is.na(mean_temp)),  # Count non-NA values for mean_temp
    mean_temp_breeding_season = mean(mean_temp, na.rm = TRUE),
    max_temp_breeding_season = mean(max_temp, na.rm = TRUE),
    min_temp_breeding_season = mean(min_temp, na.rm = TRUE)
  ) %>% 
  filter(count_mean_temp > 40)  # Filter out years with little or no data


#########################
# Saving climate data
#########################

climate_data <- climate_dataNA %>%
  left_join(komakuk_breed_season, by = join_by(year)) %>%
  select(-lat, -lon, -elev, -count_mean_temp) %>%
  select(year, everything())


output_path <- "data/clean/sem/" # Output path
write_csv(climate_data, paste0(output_path, "climate_data.csv"))



