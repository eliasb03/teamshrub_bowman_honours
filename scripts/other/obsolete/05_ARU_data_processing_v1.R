#------------------------------
# teamshrub_bowman_honours
# 0X_ARU_postprocess_v0
# By: Elias Bowman
# Created: 2024-11-18
#
# Description: This script will process the csv outputs from BirdNET runs on ARU recordings
# Should handle the full csv output from 02_ARU_pipeline
#------------------------------

# Load necessary libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(patchwork)
library(zoo)

library(ggplot2)
#library(terra)
#library(sf)
#library(stringi)


# Define the path to the ARU data folder

base_dir <- "D:/ARU_QHI_2024"
aru_all_output_folder <- "D:/ARU_QHI_2024/output_total"


# Columns to remove (replace with actual column names you want to exclude)
columns_to_remove <- c(
  "lat",
  "lon",
  "week",
  "overlap",
  "sensitivity",
  "species_list",
  "dateTimeRecorder",
  "dateTimeUTC",
  "timezone",
  "min_conf"
)

# Confidence threshold
confidence_threshold <- 0.5

# Remove 1.5 overlap
# Define the function to remove 1.5 overlap
remove_overlap <- function(dt) {
  dt[!(start %% 1 == 0.5)] # Keep rows where 'start' does not have a fractional part of 0.5
}

# Define a function to extract locationID from recordingID
extract_locationID <- function(dt) {
  dt[, locationID := sub("^(ARUQ\\d+)B?\\D.*", "\\1", recordingID)]
  return(dt)
}

# Remove faulty files
# toignore <- process_and_compile_toignore(base_dir)
#write.csv((toignore), paste0(base_dir, "/full_toignore.csv"))
toignore <- read_csv(paste0(base_dir, "/full_toignore.csv"))

# Define a function to process each file
process_aru_folder <- function(folder_path,
                               cols_to_remove,
                               confidence_threshold,
                               toignore) {
  # Get a list of CSV files in the folder
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Process each CSV file
  lapply(csv_files, function(file_path) {
    # Read the CSV using fread
    dt <- fread(file_path)
    
    # Remove unwanted columns
    dt <- dt[, !cols_to_remove, with = FALSE]
    
    # Remove rows with 1.5 overlap
    dt <- remove_overlap(dt)
    
    # Filter rows based on the confidence value
    dt <- dt[confidence > confidence_threshold]
    
    # Remove rows with file paths in the `toignore` dataset
    dt <- dt[!file_path %in% toignore$File]
    
    # Extract locationID from recordingID
    dt <- extract_locationID(dt)
    
    return(dt)
  })
}

# Process all folders
raw_aru_data <- process_aru_folder(aru_all_output_folder,
                                   columns_to_remove,
                                   confidence_threshold,
                                   toignore)

# Define the summarization function
summarize_species_by_time <- function(dt) {
  # Convert to 30-minute intervals
  dt[, time_interval := floor_date(detectionTimeLocal, unit = "30 minutes")]
  
  # Group by species, ARU, and time_interval, and count occurrences
  summarized_data <- dt[, .(species_count = .N), by = .(common_name, locationID, time_interval)]
  
  return(summarized_data)
}

summarized_aru_data <- lapply(raw_aru_data, function(dt) {
  # Call the summarize function on each data.table in the list
  summarize_species_by_time(dt)
})

# Add wind speed data
summarized_aru_data <- lapply(summarized_aru_data, function(dt)
  merge(
    dt,
    windspeed.filled,
    by.x = "time_interval",
    by.y = "datetime_whitehorse",
    all.x = TRUE
  ))

# Add temp data
summarized_aru_data <- lapply(summarized_aru_data, function(dt)
  merge(
    dt,
    tomst_temp,
    by.x = c("time_interval", "locationID"),
    by.y = c("datetime", "aru_name"),
    all.x = TRUE
  ))

# Add snowmelt data
summarized_aru_data <- lapply(summarized_aru_data, function(dt)
  merge(
    dt,
    select(tomst_snow, !tomst_num), # select out tomst_num to avoid duplicate columns 
    by.x = c("time_interval", "locationID"),
    by.y = c("datetime", "aru_name"),
    all.x = TRUE
  ))

#########
# Plotting aru date coverage

