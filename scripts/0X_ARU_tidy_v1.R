#------------------------------
# teamshrub_bowman_honours
# 0X_ARU_tidy_v1
# By: Elias Bowman
# Created: 2024-07-05
# Last update: 2024-09-18
# 
# Description: This script imports and tidys ARU data from Qikiqtaruk - Herschel Island.
# Creates a toginore_ARUX.csv files that lists files with the wrong file name, the wrong length, the wrong file type, the wrong timing
#------------------------------

# Importing Packages ####
library(seewave)   # for audio processing (used for trimming and analysis)
library(tuneR)     # for reading and handling .wav files
library(stringr)   # for string manipulation (used to handle filenames and paths)
library(dplyr)     # for working with data frames (used to log invalid files)


# Designing Functions ####
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

is_valid_wav <- function(file) { 
  # This function checks if a file has a valid .wav extension (case-insensitive)
  
  str_ends(file, regex("\\.wav$", ignore_case = TRUE))
  # Returns TRUE if the file ends with ".wav" or ".WAV"
}

# Main Processing Loop ####
process_aru_data <- function(main_dir) {
  # This function processes all directories under 'main_dir', checking .wav files for validity and duration
  
  dir_list <- list.dirs(main_dir, recursive = FALSE)
  # List all subdirectories within 'main_dir' (these correspond to individual ARU deployments)
  
  for (dir in dir_list) {  # Loop over each subdirectory (each represents a different ARU unit)
    cat("Processing directory:", dir, "\n")  # Debugging output to indicate current directory being processed
    
    data_dir <- file.path(dir, "data")
    # Construct the path to the "data" folder within the current ARU directory
    
    aru_id <- str_extract(data_dir, "ARUQ[^_]+")
    # Extract the ARU ID from the directory name for use in log file naming (assuming ARU ID starts with "ARUQ")
    
    log_file <- file.path(data_dir, paste0("toignore_", aru_id, ".csv"))
    # Define the path for the log file that will record any files that are skipped or invalid
    
    # Check for existing log file ####
    if (file.exists(log_file)) {
      # If a log file already exists, load it to avoid reprocessing files that have already been logged
      
      existing_logs <- read.csv(log_file, stringsAsFactors = FALSE)
      # Read the existing log file into a data frame (no string factors to prevent issues with character data)
      
      cat("Log file already exists with", nrow(existing_logs), "entries.\n")
      # Output the number of already logged entries to the console for tracking
    } else {
      # If no log file exists, create an empty tibble (modern data frame) to store log entries
      
      existing_logs <- tibble(File = character(), Duration = numeric(), Reason = character())
      # Create columns: 'File' (filename), 'Duration' (for potential future use), 'Reason' (why the file is invalid)
    }
    
    # List all .wav files in the data directory ####
    files <- list.files(data_dir, pattern = "\\.wav$", full.names = TRUE)
    # List all files in the "data" folder that have a .wav extension, using full file paths
    
    # Loop over each file ####
    for (file in files) {
      filename <- basename(file)
      # Extract just the filename (without the directory path) for easier handling and logging
      
      # Check if the file is already logged ####
      if (filename %in% existing_logs$File) {
        # If the file has already been processed and logged, skip it to avoid duplicate entries
        
        cat("File already logged, skipping:", filename, "\n")
        # Output a message indicating the file is being skipped
        next  # Move to the next file in the loop
      }
      
      # Check if it's a valid .wav file ####
      if (!is_valid_wav(file)) {
        # If the file isn't a valid .wav file (based on its extension), log it and skip processing
        
        cat("Invalid .wav file:", filename, "\n")
        # Output a message indicating the file is invalid
        
        existing_logs <- add_row(existing_logs, File = filename, Duration = NA, Reason = "Not a .wav file")
        # Log the file as invalid with the reason "Not a .wav file"
        next  # Move to the next file in the loop
      }
      
      # Check if the filename has the correct suffix ####
      if (!str_detect(filename, "(0000|3000)\\.wav$")) {
        # Check if the filename ends with "0000.wav" or "3000.wav" (assuming valid filenames follow this pattern)
        
        cat("Incorrect filename suffix:", filename, "\n")
        # Output a message indicating the filename has an incorrect suffix
        
        existing_logs <- add_row(existing_logs, File = filename, Duration = NA, Reason = "Incorrect filename suffix")
        # Log the file as invalid with the reason "Incorrect filename suffix"
        next  # Move to the next file in the loop
      }
      
      # Check if the file is 10 minutes long ####
      if (!is_ten_minutes(file)) {
        # If the file isn't exactly 10 minutes long, log it and skip further processing
        
        cat("Incorrect file duration:", filename, "\n")
        # Output a message indicating the file has an incorrect duration
        
        existing_logs <- add_row(existing_logs, File = filename, Duration = NA, Reason = "Incorrect duration")
        # Log the file as invalid with the reason "Incorrect duration"
      }
    }
    
    # Write the updated log file ####
    write.csv(existing_logs, log_file, row.names = FALSE)
    # Write the updated log to CSV, overwriting the old log (no row names to keep the file clean)
    
    cat("Log file updated with new entries.\n")
    # Output a message indicating the log file has been updated
  }
}

# Running the Main Function ####
main_dir <- "D:/ARU_QHI_2024"
# Main directory where ARU data is stored

process_aru_data(main_dir)
# Call the main function to start tidy the data
