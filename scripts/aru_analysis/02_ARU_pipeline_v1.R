# teamshrub_bowman_honours
# 02_ARU_pipeline_v1
# By: Elias Bowman
# Created: 2024-10-16
# Last update: 2024-10-16
#
# Description: This script imports, tidies, and processes ARU data into observance records from Qikiqtaruk - Herschel Island. It checks ARU audio files, logs invalid files, and processes valid ones using BirdNET analysis functions developed in another script. It runs this processing across all directories provided in the main directory.
#------------------------------

# Importing Required Packages ####
library(seewave)
library(tuneR)
library(stringr)
library(dplyr)
library(data.table)
library(NSNSDAcoustics)

# Importing BirdNET Script functions
source("scripts/aru_analysis/01_birdNET_functions_v2.R")

# Global Variables ####
birdNET_path <- '"C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe"' # local path to BirdNET Analyzer
# Main directory where ARU data is stored in hierarchical structure
main_dir <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input"
#main_dir <- "D:/ARU_QHI_2024"
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
    wave_info <- readWave(file, from = 0, to = 1, header = TRUE)
    duration <- wave_info$samples / wave_info$sample.rate
    return(duration == 600)
  }, error = function(e) {
    warning("Error reading file: ", file)
    return(FALSE)
  })
}

# Helper function to check if a file is properly named
is_properly_named <- function(file) {
  str_detect(basename(file), "(0000|3000)\\.wav$")
}

# Helper function to log invalid files
log_invalid_file <- function(existing_logs, file, reason) {
  filename <- basename(file)
  add_row(existing_logs, File = filename, Duration = NA, Reason = reason)
}

# Create a directory if it doesn't exist
check_and_create_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# Save combined aru results to a .csv file
save_combined_results <- function(combined_results, main_dir) {
  write.csv(combined_results, file.path(main_dir, "ARU_combined_formatted_results.csv"), row.names = FALSE)
}

# Helper function to read or create "toginore" CSV log file
read_or_create_log <- function(log_file) {
  if (file.exists(log_file)) {
    read.csv(log_file, stringsAsFactors = FALSE)
  } else {
    tibble(File = character(), Duration = numeric(), Reason = character())
  }
}

# Helper function to get ARU id from a directory
get_aru_id <- function(dir){
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
  dir_list <- list.dirs(aru_dir, recursive = FALSE) # Create list of ARUs in main_dir
  
  for (dir in dir_list) {
    data_dir <- file.path(dir, "data") # Define data directory for specific ARU
    aru_id <- get_aru_id(data_dir)# Extract ARU ID
    log_file <- file.path(data_dir, paste0("toignore_", aru_id, ".csv")) # Filepath for log file
    
    # Read or create the log file
    existing_logs <- read_or_create_log(log_file)
    
    # List all .wav files
    files <- list.files(data_dir, pattern = "\\.wav$", full.names = TRUE)
    
    # Loop through each file and validate
    for (file in files) {
      filename <- basename(file)
      if (filename %in% existing_logs$File) next
      existing_logs <- validate_file(file, existing_logs)
    }
    
    # Write the log after processing all files
    write.csv(existing_logs, log_file, row.names = FALSE)
    
  }
}

# Function to filter out invalid audio files based on "toignore.csv"
filter_valid_files <- function(data_dir, all_aru_files) {
  # Construct the log file name based on the ARU ID
  log_file <- file.path(data_dir, paste0("toignore_", get_aru_id(data_dir), ".csv"))
  
  # Read the "toignore" csv file to get a list of audio files marked as 'junk'
  junk_audio <- read.csv(log_file)$File
  
  # Filter the 'all_aru_files' to remove the 'junk_audio' files
  valid_files <- all_aru_files[!all_aru_files %in% junk_audio]
  
  # Return the list of valid audio files for further processing
  return(valid_files)
}


#------------------------------
# Primary Functions ####
#------------------------------

# Function to process all ARU results and return combined formatted results
process_all_aru_results <- function(main_dir) {
  aru_dirs <- list.dirs(main_dir, recursive = FALSE)
  
  all_results <- list()  # Initialize a list to store formatted results for all ARUs
  
  cat("Starting BirdNET analysis on", length(aru_dirs), "ARU directories...\n")
  
  for (aru in aru_dirs) {
    cat("Processing ARU:", basename(aru), "\n")
    gathered_results <- run_birdNET_analysis(aru)
    all_results[[basename(aru)]] <- gathered_results  # Store results by ARU name
    cat("Finished processing ARU:", basename(aru), "\n")
  }
  
  cat("Combining results from all ARUs...\n")
  
  # Combine all formatted results into one data frame
  aru_results <- bind_rows(all_results, .id = "ARU_ID")
  
  cat("Finished combining results. Process complete.\n")
  
  return(aru_results)  # Return combined results
}

# Run BirdNET Analysis on valid ARU files
run_birdNET_analysis <- function(aru_id) {
  data_dir <- paste0(aru_id, "/Data")
  all_aru_files <- list.files(data_dir)
  
  output_dir <- paste0(aru_id, "/Output")
  check_and_create_dir(output_dir)
  
  aru_files <- filter_valid_files(data_dir, all_aru_files)
  
  for (audio_file in aru_files) {
    input_file <- paste0(data_dir, "/", audio_file)
    output_file <- paste0(output_dir, "/", sub("\\.wav$", "", audio_file), "_BirdNET.results.r.csv")
    birdNET_analyze(input_file, output_file)
  }
  
  # Format and combine results for this ARU
  gathered_results <- gather_birdNET_results(output_dir)
  
  # Save the formatted results to a .csv file
  write.csv(gathered_results, file.path(output_dir, "gathered_results.csv"), row.names = FALSE)
  
  return(gathered_results)
}

# Run BirdNET analysis on valid ARU files
run_birdNET_analysis <- function(aru_id) {
  data_dir <- paste0(aru_id, "/Data")  # Set the path to the ARU data directory
  all_aru_files <- list.files(data_dir)  # Get a list of all files in the data directory
  
  output_dir <- paste0(aru_id, "/Output")   # Set the path for the output directory
  check_and_create_dir(output_dir)   # Create the output directory if it doesn't exist

  # Filter out invalid files and keep only the valid audio files
  aru_files <- filter_valid_files(data_dir, all_aru_files)
  
  # Process each valid ARU audio file
  for (audio_file in aru_files) {
    # Create the full path for the input audio file
    input_file <- paste0(data_dir, "/", audio_file) 

    # Define the output file name and path for BirdNET results
    output_file <- paste0(output_dir, "/", sub("\\.wav$", "", audio_file), "_BirdNET.results.r.csv")
    
    # Run the BirdNET analysis and save the results
    birdNET_analyze(input_file, output_file)
  }
  
  # Gather and format all results from the output directory
  gathered_results <- gather_birdNET_results(output_dir)
  
  # Save the combined results to a CSV file
  write.csv(gathered_results, file.path(output_dir, "gathered_results.csv"), row.names = FALSE)
  
  # Return the gathered results
  return(gathered_results)
}


#------------------------------
# Main Function ####
#------------------------------

run_aru_pipeline <- function(main_dir) {
  cat("Starting ARU pipeline...\n")  # Indicate start of the pipeline
  check_aru_files(main_dir)  # Check ARU files
  
  cat("Finished checking ARU files. Processing results...\n")  # Indicate completion of checking files
  # Process all ARU results
  combined_results <- process_all_aru_results(main_dir)
  
  cat("Saving combined results...\n")  # Indicate completion of processing
  # Save combined results to a .csv file
  save_combined_results(combined_results, main_dir)
  
  cat("ARU pipeline completed successfully!\n")  # Indicate end of the pipeline
  # Return combined results to the R environment
  return(combined_results)
}

# Main Function Call
ARU_output <- run_aru_pipeline(main_dir) # Run the pipeline