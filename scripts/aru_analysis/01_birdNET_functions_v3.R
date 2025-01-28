#--------------------------------------------------------------
# teamshrub_bowman_honours
# Script: 01_birdNET_functions_v3
# Author: Elias Bowman 
# Created: 2024-07-05
# Last Updated: 2025-01-20
#
# Description:
# This script defines functions to execute BirdNET Analyzer commands 
# via the command line and to format and gather results using the 
# NSNSDAcoustics package. The workflow integrates BirdNET for 
# bird sound analysis with custom adjustments for specific datasets.
#
# References:
# BirdNET Analyzer: https://github.com/kahst/BirdNET-Analyzer
# NSNSDAcoustics: https://github.com/nationalparkservice/NSNSDAcoustics
#--------------------------------------------------------------

#-------------------------------
# Load Required Packages ####
#-------------------------------
library(devtools)
library(data.table)
library(NSNSDAcoustics)
library(lubridate)
library(dplyr)
library(purrr)

#-------------------------------
# Global Variables ####
#-------------------------------
birdNET_path <- "C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe"  # Path to BirdNET Analyzer
  # Current path specific to Elias' computer
main_dir <- "data/temp/birdNET_input"  # ARU data directory

# Field site-specific parameters
qhi_latitude <- 69.5
qhi_longitude <- -138.9
qhi_timezone <- "America/Dawson"

#-------------------------------
# BirdNET Analysis Function ####
#-------------------------------

# Function: birdNET_analyze
# Handles both single audio file and directory input for BirdNET analysis
birdNET_analyze <- function(input, output, week = -1, sensi = 1, conf = 0.1, overlap = 1.5, 
                            rtype = "r", lat = qhi_latitude, lon = qhi_longitude, 
                            species_list = NULL, thread_count = NULL, show_warnings = TRUE) {
  
  # Check if the input is a single file or a directory
  is_single_file <- file.exists(input) && !dir.exists(input)
  
  # Conditionally format the output file name for single file input
  output <- if (is_single_file && rtype == "r") {
    sub("\\.csv$", ".BirdNET.results.csv", output)
  } else {
    output
  }
  
  # Prepare the base command with mandatory parameters
  to_execute <- paste0(
    " --i ", input,
    " --o ", output,
    " --lat ", lat,
    " --lon ", lon,
    " --week ", week, 
    " --sensitivity ", sensi, 
    " --min_conf ", conf, 
    " --overlap ", overlap, 
    " --rtype ", rtype
  )
  
  # Conditionally add species list and thread count if provided
  if (!is.null(species_list)) to_execute <- paste0(to_execute, " --slist ", species_list)
  if (!is.null(thread_count)) to_execute <- paste0(to_execute, " --threads ", thread_count)
  
  # Display the command to be executed
  cat("Executing command line prompt:\n", birdNET_path, to_execute, "\n")
  
  # Execute the command with or without warnings based on show_warnings
  output <- system2(birdNET_path, args = to_execute, stdout = TRUE, stderr = if (show_warnings) "" else NULL)
  
  # Check if there was an error
  if (!is.null(attr(output, "status")) && attr(output, "status") != 0) {
    cat("Error occurred during execution:\n")
    cat(output, sep = "\n")
  } else {
    cat("Execution completed successfully.\n")
  }
  
  return(output)
}

#-------------------------------
# BirdNET Results Processing ####
#-------------------------------

# Function: gather_birdNET_results
# Reformats, gathers, and processes BirdNET results into a single dataset.
gather_birdNET_results <- function(results_dir, timezone = qhi_timezone) {
  
  # Step 1: Reformat raw BirdNET results
  birdnet_format(
    results.directory = results_dir, 
    timezone = timezone
  )
  
  # Step 2: Gather all formatted results into one data frame
  gathered_results <- birdnet_gather(
    results.directory = results_dir, 
    formatted = TRUE
  )
  
  # Step 3: Add time columns for recorder and local time
  gathered_results <- add_time_cols(
    dt = gathered_results,
    tz.recorder = timezone,
    tz.local = timezone
  )
  
  # Add a location ID for field site identification
  gathered_results$locationID <- "qhi"
  
  # Ensure dateTimeLocal is in POSIXct format
  gathered_results$dateTimeLocal <- as.POSIXct(gathered_results$dateTimeLocal)
  
  return(gathered_results)
}

#-------------------------------
# BirdNET Faulty File Logging ####
#-------------------------------

# Check if a file has a valid .wav extension (case-insensitive)
is_valid_wav <- function(file) { 
  # This function checks if a file has a valid .wav extension (case-insensitive)
  
  str_ends(file, regex("\\.wav$", ignore_case = TRUE))
  # Returns TRUE if the file ends with ".wav" or ".WAV"
}

# Check if a .wav file has exactly 10 minutes of audio (600 seconds)
is_ten_minutes <- function(file) { 
  # This function checks if a .wav file has exactly 10 minutes of audio (600 seconds)
  
  tryCatch({
    wave_info <- readWave(file, from = 0, to = 1, header = TRUE)
    # Read only the header information of the .wav file to avoid loading the entire file
    
    duration <- wave_info$samples / wave_info$sample.rate
    # Calculate the audio duration by dividing the total samples by the sample rate
    
    return(duration == 600)  # Returns TRUE if the duration is exactly 600 seconds (10 minutes)
  }, error = function(e) {
    # Handle cases where the file can't be read (e.g., if it's corrupted or not a proper .wav file)
    warning("Error reading file: ", file)  # Output a warning message
    return(FALSE)  # Return FALSE if there's an error (file is invalid)
  })
}

# Create a directory if it doesn't exist
check_and_create_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    cat("Directory does not exist. Creating directory:", dir_path, "\n")
    dir.create(dir_path, recursive = TRUE)
  } else {
    cat("Directory already exists:", dir_path, "\n")
  }
}

# Check ARU Files
# checks and logs invalid files into a txt file (wrong duration, wrong filename, wrong type)
check_aru_files <- function(aru_dir) {
  dir_list <- list.dirs(aru_dir, recursive = FALSE, full.names = TRUE) %>%
    keep(~ str_starts(basename(.), "ARUQ")) # List subdirectories representing ARU deployments
  
  for (dir in dir_list) {
    cat("Processing directory:", dir, "\n")
    data_dir <- file.path(dir, "data")  # Path to "data" folder within the ARU directory
    aru_id <- str_extract(data_dir, "ARUQ[^_]+")  # Extract ARU ID for log naming
    log_file <- file.path(data_dir, paste0("toignore_", aru_id, ".csv"))
    
    # Load or initialize log of invalid files
    if (file.exists(log_file)) {
      existing_logs <- read.csv(log_file, stringsAsFactors = FALSE)
      cat("Log file already exists with", nrow(existing_logs), "entries.\n")
    } else {
      existing_logs <- tibble(File = character(), Duration = numeric(), Reason = character())
    }
    
    # List all .wav files in the directory
    files <- list.files(data_dir, pattern = "\\.wav$", full.names = TRUE)
    
    for (file in files) {
      filename <- basename(file)  # Extract just the filename for easier logging
      
      # Skip already logged files
      if (filename %in% existing_logs$File) {
        cat("File already logged, skipping:", filename, "\n")
        next
      }
      
      # Check if the file is valid and log issues
      if (!is_valid_wav(file)) {
        cat("Invalid .wav file:", filename, "\n")
        existing_logs <- add_row(existing_logs, File = filename, Duration = NA, Reason = "Not a .wav file")
        next
      }
      
      # Check filename suffix (assuming valid filenames end with "0000.wav" or "3000.wav")
      if (!str_detect(filename, "(0000|3000)\\.wav$")) {
        cat("Incorrect filename suffix:", filename, "\n")
        existing_logs <- add_row(existing_logs, File = filename, Duration = NA, Reason = "Incorrect filename suffix")
        next
      }
      
      # Check if the file is exactly 10 minutes long
      if (!is_ten_minutes(file)) {
        cat("Incorrect file duration:", filename, "\n")
        existing_logs <- add_row(existing_logs, File = filename, Duration = NA, Reason = "Incorrect duration")
      }
    }
    
    # Update the log file with new entries
    write.csv(existing_logs, log_file, row.names = FALSE)
    cat("Log file updated with new entries.\n")
  }
}

# Compile "toignore" CSVs from the "data" subdirectory into a single dataframe
compile_toignore_files <- function(base_dir) {
  # List all directories starting with "ARUQ"
  aru_dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE) %>%
    keep(~ str_starts(basename(.), "ARUQ"))
  
  # Initialize an empty list to store dataframes
  toignore_list <- list()
  
  # Iterate through ARU directories
  for (aru_dir in aru_dirs) {
    # Define the "data" subdirectory path
    data_dir <- file.path(aru_dir, "data")
    
    if (dir.exists(data_dir)) {
      cat("Searching in 'data' subdirectory:", data_dir, "\n")
      
      # Find "toignore" CSVs in the "data" subdirectory
      toignore_files <- list.files(data_dir, pattern = "^toignore_.*\\.csv$", full.names = TRUE)
      
      if (length(toignore_files) > 0) {
        for (file in toignore_files) {
          cat("Found 'toignore' file:", file, "\n")
          
          # Read the "toignore" CSV and add a column for the source directory
          toignore_data <- read.csv(file, stringsAsFactors = FALSE) %>%
            mutate(SourceDirectory = data_dir)
          
          # Append the dataframe to the list
          toignore_list[[length(toignore_list) + 1]] <- toignore_data
        }
      } else {
        cat("No 'toignore' files found in:", data_dir, "\n")
      }
    } else {
      cat("'data' subdirectory does not exist in:", aru_dir, "\n")
    }
  }
  
  # Combine all dataframes into one
  if (length(toignore_list) > 0) {
    combined_toignore <- bind_rows(toignore_list)
    cat("Compiled", nrow(combined_toignore), "entries from 'toignore' files.\n")
    return(combined_toignore)
  } else {
    cat("No 'toignore' files found across all 'data' subdirectories.\n")
    return(NULL)
  }
}

# Function to create "toignore" files and then compile them
process_and_compile_toignore <- function(base_dir) {
  # Step 1: Run the check_aru_files function to create "toignore" files
  cat("Running file checks to create 'toignore' files...\n")
  check_aru_files(base_dir)
  
  # Step 2: Compile all "toignore" files from the "data" subdirectories
  cat("Compiling 'toignore' files into a single dataframe...\n")
  combined_toignore <- compile_toignore_files(base_dir)
  
  # Return the combined dataframe
  return(combined_toignore)
}
