#------------------------------
# teamshrub_bowman_honours
# 04_climateNA_processing_v4
# By: Elias Bowman
# Created: 2024-11-17
# Last Updated: 2025-01-18
#
# Description: This script processes climate data for Qikiqtaruk using ClimateNA. 
# It processes outputs to create a combined dataframe of climate variables for a specified year period.
# This version of the script runs the climateNAr in parallel by splitting the periods argument into chunks.
#------------------------------
# Import Packages ####
library(tidyverse)
library(ClimateNAr)
library(future)
library(furrr)

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

# Function to split periods into chunks
split_periods <- function(periods, n_chunks) {
  message("Splitting periods into ", n_chunks, " chunks.")
  split(periods, cut(seq_along(periods), breaks = n_chunks, labels = FALSE))
}

# Function to run ClimateNA for a chunk of periods
run_climateNA_chunk <- function(input_file, period_chunk, variables, output_dir) {
  message("Processing period chunk: ", paste(period_chunk, collapse = ", "))
  climateNAr(
    inputFile = input_file,
    periodList = period_chunk,
    varList = variables,
    outDir = output_dir
  )
  message("ClimateNA processing complete for period chunk: ", paste(period_chunk, collapse = ", "))
}

# Function to run ClimateNA in parallel for period chunks
run_climateNA_parallel_chunks <- function(input_file, periods, variables, output_dir, n_chunks) {
  message("Starting parallel ClimateNA runs.")
  # Split periods into chunks
  period_chunks <- split_periods(periods, n_chunks)
  
  # Set up parallel processing
  plan(multisession) 
  
  # Process each chunk in parallel
  future_map(period_chunks, ~run_climateNA_chunk(input_file, .x, variables, output_dir))
  
  # Reset to sequential processing
  plan(sequential)
  message("All ClimateNA runs for period chunks completed.")
}

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
      mutate(Year = as.numeric(year)) # Add the year column
    
    # Append to the list
    data_list[[file]] <- value_row
  }
  
  # Combine all rows into a single dataframe
  final_data <- bind_rows(data_list) %>%
    select(-ID1, -ID2) %>%  # Remove the ID columns
    select(Year, everything()) # Move the Year column to the front
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

# Define number of chunks for parallel processing
n_chunks <- 4  # Adjust this based on your system's capabilities
message("Year range: ", min(year_range), " to ", max(year_range))
message("Climate variables to process: ", paste(clim_var_list, collapse = ", "))
message("Output directory: ", output_dir)

# Run ClimateNA in parallel for chunks
run_climateNA_parallel_chunks(input_file_path, year_inputs, clim_var_list, output_dir, n_chunks)

# Process ClimateNA output files
climate_data <- process_climateNA_files(output_dir)

# View resulting dataframe
print(climate_data)

# Save resulting dataframe
output_file_path <- "data/clean/climateNA_output.csv"
write_csv(climate_data, output_file_path)
message("Final climate data saved to: ", output_file_path)

message("ClimateNA processing script completed successfully.")
