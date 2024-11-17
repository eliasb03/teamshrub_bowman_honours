# teamshrub_bowman_honours
# ARU_pipeline_single_folder
# By: Elias Bowman
# Created: 2024-10-19
# Last update: 2024-10-19
#
# Description: This script imports, tidies, and processes ARU data into observance records from Qikiqtaruk - Herschel Island. It checks ARU audio files, logs invalid files, and processes valid ones using BirdNET analysis functions developed in another script. It is designed to work on a single ARU folder containing a "Data" folder.

#------------------------------
# Script Setup ####
#------------------------------
# Importing Required Packages ####
library(seewave)
library(tuneR)
library(stringr)
library(dplyr)
library(data.table)
library(NSNSDAcoustics)

# Global Variables ####
# Set your specific ARU directory here
aru_dir <- "D:/ARU_QHI_2024/ARUQ5_20Aug2024"

# Field site specific variables
qhi_latitude <- 69.5
qhi_longitude <- -138.9
qhi_timezone <- "America/Dawson"

#------------------------------
# Helper Functions ####
#------------------------------

# Check if file has valid .wav extension
is_valid_wav <- function(file) {
  str_ends(file, regex("\\.wav$", ignore_case = TRUE))
}

# Check if a .wav file has exactly 10 minutes of audio
is_ten_minutes <- function(file) {
  tryCatch({
    wave_info <- readWave(file,
                          from = 0,
                          to = 1,
                          header = TRUE)
    duration <- wave_info$samples / wave_info$sample.rate
    return(duration == 600)
  }, error = function(e) {
    warning("Error reading file: ", file)
    return(FALSE)
  })
}

# Check if .wav file is properly named
is_properly_named <- function(file) {
  str_detect(basename(file), "(0000|3000)\\.wav$")
}

# Helper function to log invalid files
log_invalid_file <- function(existing_logs, file, reason) {
  filename <- basename(file)
  add_row(
    existing_logs,
    File = filename,
    Duration = NA,
    Reason = reason
  )
}

# Create a directory if it doesn't exist
check_and_create_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# Save combined aru results to a .csv file
save_combined_results <- function(combined_results, aru_dir) {
  write.csv(
    combined_results,
    file.path(aru_dir, "ARU_formatted_results.csv"),
    row.names = FALSE
  )
}

# Helper function to read or create "toignore" CSV log file
read_or_create_log <- function(log_file) {
  if (file.exists(log_file)) {
    read.csv(log_file, stringsAsFactors = FALSE)
  } else {
    tibble(File = character(),
           Duration = numeric(),
           Reason = character())
  }
}

# Helper function to get ARU id from a directory
get_aru_id <- function(dir) {
  aru_id <- str_extract(dir, "ARUQ[^_]+") # Extract ARU ID
  return(aru_id)
}

#------------------------------
# ARU Functions ####
#------------------------------

# Helper function to check file validity
validate_file <- function(file, existing_logs) {
  if (!is_valid_wav(file)) {
    return(log_invalid_file(existing_logs, file, "Not a .wav file"))
  }
  
  if (!is_properly_named(file)) {
    return(log_invalid_file(existing_logs, file, "Incorrect filename suffix"))
  }
  
  if (!is_ten_minutes(file)) {
    return(log_invalid_file(existing_logs, file, "Incorrect duration"))
  }
  
  return(existing_logs)  # If valid, no new rows are added
}

# Main function to check ARU files for validity and log issues in "toignore" CSV
check_aru_files <- function(aru_dir) {
  data_dir <- file.path(aru_dir, "Data")  # Adjust case to "Data"
  
  cat("Starting check on ARU .wav files in", data_dir, "...\n")
  
  # Read or create the log file
  aru_id <- get_aru_id(aru_dir)  # Extract ARU ID
  log_file <- paste0("toignore_", aru_id, ".csv")  # Filepath for log file
  log_path <- file.path(data_dir, log_file)
  existing_logs <- read_or_create_log(log_path)
  
  # List all .wav files
  files <- list.files(data_dir, pattern = "\\.wav$", full.names = TRUE)
  
  # Loop through each file and validate
  for (file in files) {
    filename <- basename(file)
    if (filename %in% existing_logs$File) next
    existing_logs <- validate_file(file, existing_logs)
  }
  
  existing_logs <- log_invalid_file(existing_logs, log_file, "toignore file itself")
  
  # Write the log after processing all files
  write.csv(existing_logs, log_path, row.names = FALSE)
  cat("Finished checking files in", aru_dir, "...\n")
}

#------------------------------
# Primary Functions ####
#------------------------------

# Function to process the ARU results and return formatted results
process_aru_results <- function(aru_dir) {
  data_dir <- file.path(aru_dir, "Data")
  output_dir <- file.path(aru_dir, "Output")
  
  check_and_create_dir(output_dir)  # Create the output directory if it doesn't exist
  
  # Run the BirdNET analysis and save the results
  birdNET_batch_analyze(data_dir, output_dir, show_warnings = FALSE)
  
  cat("Gathering and formatting results from BirdNET... \n")
  # Gather and format all results from the output directory
  gathered_results <- gather_birdNET_results(output_dir)
  
  cat("Saving results for", basename(aru_dir), "... \n")
  # Save the combined results to a CSV file
  write.csv(gathered_results,
            file.path(output_dir, paste0(basename(aru_dir), ".BirdNET_formatted_results.csv")),
            row.names = FALSE)
  
  # Return the gathered results
  return(gathered_results)
}

#------------------------------
# Main Function ####
#------------------------------

run_aru_pipeline_single_folder <- function(aru_dir) {
  cat("Starting ARU pipeline for a single folder...\n")  # Indicate start of the pipeline
  pipe.start.t <- Sys.time()  # t1 POSIXct object
  cat("Starting at", format(pipe.start.t, "%H:%M:%S", tz = qhi_timezone), "\n")
  
  check_aru_files(aru_dir)  # Check ARU files for validity
  
  # Process ARU results
  cat("Finished checking ARU files. Processing results...\n")
  combined_results <- process_aru_results(aru_dir)
  
  cat("Saving combined results...\n")
  # Save combined results to a .csv file (already done in the processing function)
  
  pipe.end.t <- Sys.time()  # t1 POSIXct object
  cat("Ending at", format(pipe.end.t, "%H:%M:%S", tz = qhi_timezone), "\n")
  
  # Calculate the time difference in seconds
  pipe_time_taken <- as.numeric(difftime(pipe.end.t, pipe.start.t, units = "secs"))
  cat("Pipeline took", pipe_time_taken, "seconds\n")
  
  cat("ARU pipeline completed successfully!\n")  # Indicate end of the pipeline
  # Return combined results to the R environment
  return(combined_results)
}

# Main Function Call
ARU_output <- run_aru_pipeline_single_folder(aru_dir)  # Run the pipeline for a single ARU folder
