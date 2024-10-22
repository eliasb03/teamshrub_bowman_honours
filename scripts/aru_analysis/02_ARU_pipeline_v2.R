# teamshrub_bowman_honours
# 02_ARU_pipeline_v2
# By: Elias Bowman
# Created: 2024-10-19
# Last update: 2024-10-19
#
# Description: This script imports, tidies, and processes ARU data into observance records from Qikiqtaruk - Herschel Island. It checks ARU audio files, logs invalid files, and processes valid ones using BirdNET analysis functions developed in another script. It runs this processing across all directories provided in the main directory. Differs from version 1 in using a batch analysis method
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

# Importing BirdNET Script functions
source("scripts/aru_analysis/01_birdNET_functions_v2.R")

# Global Variables ####

# Main directory where ARU data is stored in hierarchical structure
#main_dir <- "D:/ARU_QHI_2024"
main_dir <-
  "D:/ARU_code_optimization_data" # subsampled input folder for test work
  #"C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input"
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
save_combined_results <- function(combined_results, main_dir) {
  write.csv(
    combined_results,
    file.path(main_dir, "ARU_combined_formatted_results.csv"),
    row.names = FALSE
  )
}

# Helper function to read or create "toginore" CSV log file
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
  dir_list <-
    list.dirs(aru_dir, recursive = FALSE) # Create list of ARUs in main_dir
  
  cat("Starting check on",
      length(dir_list),
      "ARU .wav directories...\n")
  
  for (dir in dir_list) {
    data_dir <-
      file.path(dir, "data") # Define data directory for specific ARU
    aru_id <- get_aru_id(data_dir)# Extract ARU ID
    log_file <-
      paste0("toignore_", aru_id, ".csv") # Filepath for log file
    log_path <- file.path(data_dir, log_file)
    
    # Read or create the log file
    existing_logs <- read_or_create_log(log_path)
    
    # List all .wav files
    files <-
      list.files(data_dir, pattern = "\\.wav$", full.names = TRUE)
    
    # Loop through each file and validate
    for (file in files) {
      filename <- basename(file)
      if (filename %in% existing_logs$File)
        next
      existing_logs <- validate_file(file, existing_logs)
    }
    
    existing_logs <- log_invalid_file(existing_logs, log_file, "toignore file itself")
    
    # Write the log after processing all files
    write.csv(existing_logs, log_path, row.names = FALSE)
    cat("Finished checking ", dir, " files...\n")
    
  }
}

# move files from the toignore_ARU__.txt file to wavignore/ dir
remove_ignore_files <- function(main_dir) {

  aru_dirs <- list.dirs(main_dir, recursive = FALSE)
  
  for (aru_dir in aru_dirs) {
    data_dir <-
      file.path(aru_dir, "Data") # Define data directory for specific ARU
    wavignore_dir <-
      file.path(aru_dir, paste0("wavignore/"))
    aru_id <-
      get_aru_id(aru_dir)# Extract ARU ID
    log_file <-
      file.path(data_dir, paste0("toignore_", aru_id, ".csv")) # Filepath for log file
    
    cat("Removing",
        aru_id,
        " files...\n")
    
    # Read or create the wavignore dir
    check_and_create_dir(wavignore_dir)
    
    cat("Directory wavignore/ created... \n")

    
    # Read log file
    ignore_log <- read.csv(log_file)
    
    # Loop through the list of files and move them
    ignore_log %>%
      pull(File) %>%   # Replace 'file_name_column' with the actual column name in your CSV
      lapply(function(file_name) {
        source_file <- file.path(data_dir, file_name)
        target_file <- file.path(wavignore_dir, file_name)
        
        if (file.exists(source_file)) {
          file.rename(source_file, target_file)
          cat(paste("Moved:", file_name, "\n"))
        } else {
          cat(paste("File not found:", file_name, "\n"))
        }
      })
  }
}

#------------------------------
# Primary Functions ####
#------------------------------

# Function to process all ARU results and return combined formatted results
process_all_aru_results <- function(main_dir) {
  aru_dirs <- list.dirs(main_dir, recursive = FALSE)
  
  all_results <-
    list()  # Initialize a list to store formatted results for all ARUs
  
  cat("Starting BirdNET analysis on",
      length(aru_dirs),
      "ARU directories...\n")
  
  for (aru in aru_dirs) {
    t1 <- Sys.time()  # t1 POSIXct object
    cat("Processing ARU:", basename(aru), "\nStarting at", format(t1, "%H:%M:%S", tz = qhi_timezone), "\n")
    
    # Running Birdnet Analysis function
    single_aru_result <- run_birdNET_analysis(aru)
    all_results[[basename(aru)]] <- single_aru_result  # Store results by ARU name
    
    t2 <- Sys.time()  # t2 POSIXct object
    cat("Finished processing ARU:", basename(aru), "\nFinished at", format(t2, "%H:%M:%S", tz = qhi_timezone), "\n")
    
    # Calculate the time difference in seconds
    time_taken <- as.numeric(difftime(t2, t1, units = "secs"))
    cat("Processing took", time_taken, "seconds\n")
  }
  
  cat("Combining results from all ARUs...\n")
  
  # Combine all formatted results into one data frame
  aru_results <- bind_rows(all_results, .id = "ARU_ID")
  
  cat("Finished combining results. Process complete.\n")
  
  return(aru_results)  # Return combined results
}

# Run BirdNET analysis on valid ARU files
run_birdNET_analysis <- function(aru_id) {
  data_dir <-
    paste0(aru_id, "/Data")  # Set the path to the ARU data directory
  output_dir <-
    paste0(aru_id, "/Output")   # Set the path for the output directory
  
  check_and_create_dir(output_dir)   # Create the output directory if it doesn't exist
  
  # Run the BirdNET analysis and save the results
  birdNET_batch_analyze(data_dir, output_dir, show_warnings = FALSE)
  
  cat("Gathering and formatting results from BirdNET... \n")
  # Gather and format all results from the output directory
  gathered_results <- gather_birdNET_results(output_dir)
  
  cat("Saving results of", basename(aru_id), "... \n")
  # Save the combined results to a CSV file
  write.csv(gathered_results,
            file.path(output_dir, paste0(basename(aru_id), ".BirdNET_formatted_results.csv")),
            row.names = FALSE)
  
  # Return the gathered results
  return(gathered_results)
}

#------------------------------
# Main Function ####
#------------------------------

run_aru_pipeline <- function(main_dir) {
  cat("Starting ARU pipeline...\n")  # Indicate start of the pipeline
  pipe.start.t <- Sys.time()  # t1 POSIXct object
  cat("Starting at", format(pipe.start.t, "%H:%M:%S", tz = qhi_timezone), "\n")
  
  cat("Checking which .wav files to process... \n")
  
  check_aru_files(main_dir)
  
  cat("Removing toignore files...\n")
  
  remove_ignore_files(main_dir) # move files from the toignore_ARU__.txt file to wavignore/ dir
  
  cat("toignore files removed...\n")  # Indicate completion of checking files
  
  # Process all ARU results
  cat("Finished checking ARU files. Processing results...\n")  # Indicate completion of checking files
  combined_results <- process_all_aru_results(main_dir)
  
  cat("Saving combined results...\n")  # Indicate completion of processing
  # Save combined results to a .csv file
  save_combined_results(combined_results, main_dir)
  
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
ARU_output <- run_aru_pipeline(main_dir) # Run the pipeline


