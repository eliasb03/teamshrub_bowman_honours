# teamshrub_bowman_honours
# 0X_ARU_pipeline_v1
# By: Elias Bowman
# Created: 2024-10-16
# Last update: 2024-10-16
#
# Description: This script imports, tidies, and processes ARU data from Qikiqtaruk - Herschel Island.
# It checks ARU audio files, logs invalid files, and processes valid ones using BirdNET analysis.
#------------------------------
# Importing Required Packages ####
library(seewave)
library(tuneR)
library(stringr)
library(dplyr)
library(data.table)
library(NSNSDAcoustics)

# Importing BirdNET Script functions
source("scripts/aru_analysis/0X_birdNET_v2.R")

# Global Variables ####
birdnet_path <- '"C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe"' # local path to BirdNET Analyzer
# Main directory where ARU data is stored in hierarchical structure
main_dir <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input"
# Field site specific variables
qhi_latitude <- 69.5
qhi_longitude <- -138.9
qhi_timezone <- "America/Dawson"


#------------------------------
# Minor Helper Functions ####
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

# Create a directory if it doesn't exist
check_and_create_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# Function to save combined results to a .csv file
save_combined_results <- function(combined_results, main_dir) {
  write.csv(combined_results, file.path(main_dir, "ARU_combined_formatted_results.csv"), row.names = FALSE)
}

#------------------------------
# ARU Functions ####
#------------------------------

# Filter out invalid audio files based on "toignore.csv"
filter_valid_files <- function(data_dir, all_aru_files) {
  toignore_csv <- all_aru_files[grepl("^toignore", all_aru_files) & grepl("\\.csv$", all_aru_files)]
  junk_audio <- read.csv(file.path(data_dir, toignore_csv))$File
  valid_files <- all_aru_files[!all_aru_files %in% c(junk_audio, toignore_csv)]
  return(valid_files)
}

# Check ARU Files for validity and log issues
check_aru_files <- function(aru_dir) {
  dir_list <- list.dirs(aru_dir, recursive = FALSE)
  for (dir in dir_list) {
    data_dir <- file.path(dir, "data")
    aru_id <- str_extract(data_dir, "ARUQ[^_]+")
    log_file <- file.path(data_dir, paste0("toignore_", aru_id, ".csv"))
    existing_logs <- if (file.exists(log_file)) read.csv(log_file, stringsAsFactors = FALSE) else tibble(File = character(), Duration = numeric(), Reason = character())
    
    files <- list.files(data_dir, pattern = "\\.wav$", full.names = TRUE)
    for (file in files) {
      filename <- basename(file)
      if (filename %in% existing_logs$File) next
      if (!is_valid_wav(file)) {
        existing_logs <- add_row(existing_logs, File = filename, Duration = NA, Reason = "Not a .wav file")
        next
      }
      if (!str_detect(filename, "(0000|3000)\\.wav$")) {
        existing_logs <- add_row(existing_logs, File = filename, Duration = NA, Reason = "Incorrect filename suffix")
        next
      }
      if (!is_ten_minutes(file)) {
        existing_logs <- add_row(existing_logs, File = filename, Duration = NA, Reason = "Incorrect duration")
      }
    }
    write.csv(existing_logs, log_file, row.names = FALSE)
  }
}

#------------------------------
# Primary Running Functions ####
#------------------------------

# Function to process all ARU results and return combined formatted results
process_all_aru_results <- function(main_dir) {
  aru_dirs <- list.dirs(main_dir, recursive = FALSE)
  
  all_results <- list()  # Initialize a list to store formatted results for all ARUs
  
  for (aru in aru_dirs) {
    gathered_results <- run_birdnet_analysis(aru)
    all_results[[basename(aru)]] <- gathered_results  # Store results by ARU name
  }
  
  # Combine all formatted results into one data frame
  aru_results <- bind_rows(all_results, .id = "ARU_ID")
  
  return(aru_results)  # Return combined results
}


# Run BirdNET Analysis on valid ARU files
run_birdnet_analysis <- function(aru_id) {
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

#------------------------------
# Main Function ####
#------------------------------

run_aru_pipeline <- function(main_dir) {
  check_aru_files(main_dir)
  
  # Process all ARU results
  combined_results <- process_all_aru_results(main_dir)
  
  # Save combined results to a .csv file
  save_combined_results(combined_results, main_dir)
  
  # Return combined results to the R environment
  return(combined_results)
}

#------------------------------
# Main Function Call ####
#------------------------------
ARU_output <- run_aru_pipeline(main_dir) # Run the pipeline
