#------------------------------
# teamshrub_bowman_honours
# 05_ARU_postprocess_v1
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

library(ggplot2)

# Define the path to the ARU data folder

base_dir <- "D:/ARU_QHI_2024"
aru_all_output_folder <- "D:/ARU_QHI_2024/output_total"

# Confidence threshold 
confidence_threshold <- 0.5 

# Columns to remove 
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
    dt <- dt[!recordingID %in% toignore$File]
    
    # Filter out rows where recordingID doesn't start with "ARU" or doesn't end with "0000.wav" or "3000.wav"
    dt <- dt[str_detect(recordingID, "^ARU") & str_detect(recordingID, "(0000|3000)\\.wav$")]
    
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

# This line of code is only here to handle an issue with ARUQ14 where the datetime isnt formatting correctly, all others are
# Note that ARUQ14 still isnt filling in seconds correctly, but i dont think that should matter
raw_aru_data[[7]][, detectionTimeLocal := as.POSIXct(detectionTimeLocal, format = "%Y-%m-%d %H:%M")]

# aru_activity[[7]][, dateTimeLocal := as.POSIXct(str_extract(filepath, "\\d{8}_\\d{6}") %>%
#                                                   strptime(format = "%Y%m%d_%H%M%S"), 
#                                                 tz = "America/Dawson")]
raw_aru_data[[7]][, dateTimeLocal := ifelse(
  grepl("^\\d{4}-\\d{2}-\\d{2}$", dateTimeLocal),
  paste0(dateTimeLocal, " 00:00:00"),
  dateTimeLocal
)]



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


# Reordering the list based on ARU numbers
aru_numbers <- sapply(summarized_aru_data, function(dt) {
  # Extract the number after "ARU" 
  as.integer(sub("ARUQ", "", dt$locationID[1]))  # Removes 'ARU' and converts the result to integer
})
summarized_aru_data <- # Reorder the list based on the extracted numbers
  summarized_aru_data[order(aru_numbers)]


# Creating a single data.table with all of the ARU data
aru_analysis_datatable <- rbindlist(summarized_aru_data)
aru_analysis_datatable <- aru_analysis_datatable[!is.na(time_interval)]
aru_analysis_dataframe <- as.data.frame(aru_analysis_datatable)


# Write as individual csv files
lapply(summarized_aru_data, function(dt) {
  # Get the unique value from column 'x' (assuming the column exists in all data.tables)
  unique_value <- unique(dt$locationID)[1]  # Get the first value of the location ID column
  
  # Write the data.table to a CSV with the unique value as part of the file name
  write_csv(dt, paste0("data/clean/aru/individual/", unique_value, "_data.csv"))
})

# Save final dataset
write_csv(aru_analysis_dataframe, paste0("data/clean/aru/aru_analysis_data", "_conf", confidence_threshold,".csv"))

aru_analysis_dataframe <- read_csv(paste0("data/clean/aru/aru_analysis_data", "_conf", confidence_threshold,".csv"))

# Saving and writing raw ARU data
raw_aru_data

# Define the filtering threshold and the three target species.
# Adjust these values as needed.
confidence_threshold <- 0.5
target_species <- c("Lapland Longspur", "Red-throated Loon", "Semipalmated Plover")

# Filter each data.table in the large list based on the threshold and species criteria.
# We assume that "scientific_name" holds the species information.
filtered_list <- lapply(raw_aru_data, function(dt) {
  dt[confidence > confidence_threshold & common_name %in% target_species]
})

# Combine (stack) all filtered data.tables into one final data.table.
final_dt <- rbindlist(filtered_list)

# Replace all "\" with "/" in the filepath column of aru_data
final_dt[, filepath := gsub("\\\\", "/", filepath)]

# Save the final data to a CSV file.
fwrite(final_dt, "data/clean/aru/filtered_aru_data.csv")

final_dt <- fread("data/clean/aru/filtered_aru_data.csv")

