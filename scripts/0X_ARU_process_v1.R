#------------------------------
# teamshrub_bowman_honours
# 0X_ARU_process_v1
# By: Elias Bowman
# Created: 2024-10-16
# Last update: 2024-10-16
# 
# Description: This script imports, tidies, and processes ARU data from Qikiqtaruk - Herschel Island.
# It creates toignore_ARUX.csv files that list files with incorrect properties (e.g., duration, name, type),
# and processes valid audio files using BirdNET analysis.
#------------------------------

# Global Variables ####
# main_dir <- "D:/ARU_QHI_2024"   # Main directory where ARU data is stored

# Importing Required Packages ####
library(seewave)   # Audio processing, trimming, and analysis
library(tuneR)     # Reading and handling .wav files
library(stringr)   # String manipulation for handling filenames and paths
library(dplyr)     # Data frame manipulation and logging

#------------------------------
# Minor Functions ####
#------------------------------

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

# Function to filter out invalid audio files based on "toignore.csv"
filter_valid_files <- function(data_dir, all_aru_files) {
  
  # Identify and read the "toignore.csv" file
  toignore_csv <- all_aru_files[grepl("^toignore", all_aru_files) & grepl("\\.csv$", all_aru_files)]
  
  # Extract the list of files to ignore from the "toignore.csv" file
  junk_audio <- read.csv(file.path(data_dir, toignore_csv))$File
  
  # Combine junk audio and the "toignore.csv" file itself to be excluded
  files_to_exclude <- c(junk_audio, toignore_csv)
  
  # Return the list of valid audio files (excluding the junk ones)
  valid_files <- all_aru_files[!all_aru_files %in% files_to_exclude]
  
  return(valid_files)
}

#------------------------------
# Primary Functions ####
#------------------------------

# Check ARU Files
  # checks and logs invalid files into a txt file (wrong duration, wrong filename, wrong type)
check_aru_files <- function(aru_dir) {
  dir_list <- list.dirs(aru_dir, recursive = FALSE)  # List subdirectories representing ARU deployments
  
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

# Analyze valid ARU files using BirdNET
run_birdnet_analysis <- function(aru_id) {
  
  data_dir <- paste0(aru_id, "/Data")  # Define directory for ARU data
  all_aru_files <- list.files(data_dir)  # List all files in the data directory
  
  output_dir <- paste0(aru_id, "/Output")  # Define output directory
  check_and_create_dir(output_dir)  # Ensure the output directory exists
  
  # Call the filtering function to get the valid audio files
  aru_files <- filter_valid_files(data_dir, all_aru_files)
  
  # Loop through each valid audio file, run BirdNET analysis, and save results
  for (audio_file in aru_files) {
    
    # Construct full file paths for input and output
    input_file <- paste0(data_dir, "/", audio_file)
    output_file <- paste0(output_dir, "/", sub("\\.wav$", "", audio_file), "_output.csv")
    
    # Run BirdNET analysis on each valid file
    birdNET_analyze(input_file, output_file)
  }
}

#------------------------------
# Main Function ####
#------------------------------

# Run the entire ARU processing pipeline
run_aru_pipeline <- function(main_dir) {
  check_aru_files(main_dir)  # Tidy and validate ARU data
  aru <- "D:/ARU_QHI_2024/ARUQ1_17Aug2024"  # Specify ARU folder for testing
  run_birdnet_analysis(aru)  # Process valid audio files
}

#################################
#run_aru_pipeline(main_dir)