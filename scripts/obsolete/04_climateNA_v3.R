#------------------------------
# teamshrub_bowman_honours
# 03_climateNA_processing_v1
# By: Elias Bowman
# Created: 2024-11-17
# Last Updated: 2025-01-18
#
# Description: This script processes ClimateNA outputs to create a combined dataframe of climate variables 
# for specified years and locations. Outputs include annual climate averages for use in ecological analyses.
#------------------------------
# Analysis approach:
# - Create input files with specified lat/lon/elevation
# - Run ClimateNA in parallel
# - Process ClimateNA output files
# - Combine rows from all files into a single dataframe
# - Add year information extracted from filenames

# Import Packages ####
library(tidyverse)
library(ClimateNAr)
library(future)
library(foreach)
library(doFuture)

# Set up the parallel backend
plan(multisession, workers = parallel::detectCores() - 2)  # Use total cores - 2
registerDoFuture()  # Register doFuture as the backend for foreach

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

# Function to run ClimateNA for a single chunk of years
run_climateNA_single <- function(input_file, year_chunk, variables, output_dir) {
  start_time <- Sys.time()
  message("Starting ClimateNA for years: ", paste(year_chunk, collapse = ", "))
  
  # Run ClimateNA
  climateNAr(
    inputFile = input_file,
    periodList = year_chunk,
    varList = variables,
    outDir = output_dir
  )
  
  end_time <- Sys.time()
  message("ClimateNA processing complete for years: ", paste(year_chunk, collapse = ", "), 
          " - Duration: ", round(end_time - start_time, 2), " seconds")
}

# Function to run ClimateNA in parallel for chunks of years
run_climateNA_parallel <- function(year_range, clim_var_list, output_dir, input_file_path) {
  # Split the year range into smaller chunks
  cores <- parallel::detectCores() - 2
  year_chunks <- split(year_range, ceiling(seq_along(year_range) / cores))
  
  message("Running ClimateNA in parallel...")
  
  # Run ClimateNA for each chunk in parallel using foreach
  foreach(year_chunk = year_chunks) %dofuture% {
            run_climateNA_single(input_file_path, year_chunk, clim_var_list, output_dir)
          }
  
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
elevation <- 2

# Define file paths
output_dir <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/climateNA/output/"
input_file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/climateNA/climateNA_input_parallel_test.csv"

# Create input dataframe
input_df <- create_input_df(latitude, longitude, elevation)

# Write the input dataframe to a file once, before the parallel processing
write_csv(input_df, input_file_path)
message("Input file written to: ", input_file_path)

# Define year range and climate variables
year_range <- 1985:2023
clim_var_list <- c("Tave_sp", "Tmin_sp", "Tmax_sp", "Tave_sm", "Tmin_sm", "Tmax_sm", 
                   "Tave04", "Tave05", "Tave06", "Tave07", "Tave08")

# Run ClimateNA in parallel for chunks of years
run_climateNA_parallel(year_range, clim_var_list, output_dir, input_file_path)

# Process ClimateNA output files
climate_data <- process_climateNA_files(output_dir)

# View resulting dataframe
print(climate_data)