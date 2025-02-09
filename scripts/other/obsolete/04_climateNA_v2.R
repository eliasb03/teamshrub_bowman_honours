#------------------------------
# teamshrub_bowman_honours
# 04_climateNA_processing_v2
# By: Elias Bowman
# Created: 2024-11-17
# Last Updated: 2025-01-18
#
# Description: This script processes creates climate data for Qikiqtaruk using ClimateNA. It process  outputs to create a combined dataframe of climate variables for a specified year period.
# This version of the script runs the climateNAr in parallel. I DONT THINK THIS PARALLEL APPROACH IS CORRECT RN BECAUSE OF THE "periods" argument which messes it up I believe.
#------------------------------
# Import Packages ####
library(tidyverse)
library(ClimateNAr)
library(future)
library(furrr)

# Helper Functions ####

# Function to create input dataframe
create_input_df <- function(lat, lon, elev) {
  data.frame(
    ID1 = c(1, 2),
    ID2 = c(1, 2),
    lat = c(lat, 1),
    lon = c(lon, 1),
    elev = c(elev, 1)
  )
}

# Function to run ClimateNA for a single set of parameters
run_climateNA_single <- function(input_file, periods, variables, output_dir) {
  climateNAr(
    inputFile = input_file,
    periodList = periods,
    varList = variables,
    outDir = output_dir
  )
  message("ClimateNA processing complete for: ", input_file)
}

# Function to run ClimateNA in parallel
run_climateNA_parallel <- function(input_files, periods, variables, output_dir) {
  plan(multisession) # Set up parallel processing
  future_map(input_files, ~run_climateNA_single(.x, periods, variables, output_dir))
  plan(sequential) # Reset to sequential processing
  message("All ClimateNA runs completed.")
}

# Function to process ClimateNA output files
process_climateNA_files <- function(directory) {
  # List all CSV files in the directory
  file_list <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize an empty list to store dataframes
  data_list <- list()
  
  # Loop through each file
  for (file in file_list) {
    # Extract the year from the file name (assuming it's a 4-digit number)
    year <- str_extract(basename(file), "\\d{4}")
    
    # Read the CSV file
    file_data <- read_csv(file, col_names = TRUE, show_col_types = FALSE)
    
    # Select the first row and convert to a dataframe
    value_row <- file_data[1, ] %>%
      as_tibble() %>%
      mutate(Year = as.numeric(year)) # Add the year column
    
    # Append to the list
    data_list[[file]] <- value_row
  }
  
  # Combine all rows into a single dataframe
  final_data <- bind_rows(data_list) %>%
    select(-ID1, -ID2) %>%  # Remove the ID columns
    select(Year, everything()) # Move the Year column to the front
  
  return(final_data)
}

# Main Script ####

# Specify parameters
latitude <- 69.5723 
longitude <- -138.9064
elevation <- 2 # elevation in metres of site

# Create input dataframe
input_df <- create_input_df(latitude, longitude, elevation)

# Define file paths
input_file_path <- "data/raw/climateNA/climateNA_input_test.csv"
output_dir <- "data/raw/climateNA/output/"

# Write input file
write_csv(input_df, input_file_path)
message("Input file written to: ", input_file_path)

# Define year range and climate variables
year_range <- 1985:2023
year_inputs <- paste0("Year_", year_range, ".ann")
clim_var_list <- c("Tave_sp", "Tmin_sp", "Tmax_sp", "Tave_sm", "Tmin_sm", "Tmax_sm", 
                   "Tave04", "Tave05", "Tave06", "Tave07", "Tave08")

# Run ClimateNA in parallel
input_files <- list(input_file_path) # Add additional input files as needed
run_climateNA_parallel(input_files, year_inputs, clim_var_list, output_dir)

# Process ClimateNA output files
climate_data <- process_climateNA_files(output_dir)

# View resulting dataframe
print(climate_data)

# Saving resulting dataframe
write_csv(climate_data, "data/clean/climateNA_output.csv")