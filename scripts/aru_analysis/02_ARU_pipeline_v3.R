# teamshrub_bowman_honours
# 02_ARU_pipeline_v3
# By: Elias Bowman
# Created: 2024-10-19
# Last update: 2025-01-20
#
# Description: This script imports, tidies, and processes ARU data into observance records from Qikiqtaruk - Herschel Island. It checks ARU audio files, logs invalid files, and processes valid ones using BirdNET analysis functions developed in script 01_birdNET_functions. It runs this processing across all directories provided in the main directory.
#------------------------------
# Script Setup ####
#------------------------------

# Load Required Packages
library(seewave)
library(tuneR)
library(stringr)
library(dplyr)
library(data.table)
library(NSNSDAcoustics)

# Global Variables
main_dir <- "data/temp/birdNET_input"
  #"D:/ARU_QHI_2024/ARUQ5_20Aug2024"
  #"D:/ARU_code_optimization_data" # subsampled input folder for test work
  #"D:/ARU_QHI_2024"
  
qhi_latitude <- 69.5
qhi_longitude <- -138.9
qhi_timezone <- "America/Dawson"

#------------------------------
# Helper Functions ####
#------------------------------

# Create a directory if it doesn't exist
check_and_create_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# Extract ARU ID from a directory
get_aru_id <- function(dir) {
  str_extract(dir, "ARUQ[^_]+")
}

# Save combined results to a CSV
save_combined_results <- function(results, dir) {
  write.csv(results, file.path(dir, "ARU_combined_formatted_results.csv"), row.names = FALSE)
}

# Run BirdNET analysis on a single ARU
run_birdNET_analysis <- function(aru_dir) {
  data_dir <- file.path(aru_dir, "Data")
  output_dir <- file.path(aru_dir, "Output")
  
  check_and_create_dir(output_dir)
  birdNET_analyze(data_dir, output_dir, show_warnings = FALSE)
  
  cat("Gathering and formatting results from BirdNET...\n")
  results <- gather_birdNET_results(output_dir)
  
  write.csv(results, file.path(output_dir, paste0(basename(aru_dir), ".BirdNET_formatted_results.csv")), row.names = FALSE)
  return(results)
}

# Process all ARUs and return combined results
process_all_aru_results <- function(main_dir) {
  aru_dirs <- list.dirs(main_dir, recursive = FALSE)
  all_results <- lapply(aru_dirs, function(aru_dir) {
    cat("Processing ARU:", basename(aru_dir), "\n")
    run_birdNET_analysis(aru_dir)
  })
  bind_rows(all_results, .id = "ARU_ID")
}

#------------------------------
# Main Pipeline ####
#------------------------------

run_aru_pipeline <- function(main_dir) {
  cat("Starting ARU pipeline...\n")
  start_time <- Sys.time()
  cat("Pipeline started at", format(start_time, "%H:%M:%S", tz = qhi_timezone), "\n")
  
  cat("Processing results...\n")
  combined_results <- process_all_aru_results(main_dir)
  
  cat("Saving combined results...\n")
  save_combined_results(combined_results, main_dir)
  
  end_time <- Sys.time()
  cat("Pipeline completed at", format(end_time, "%H:%M:%S", tz = qhi_timezone), "\n")
  cat("Total runtime:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")
}

#------------------------------
# Execute Pipeline ####
#------------------------------

run_aru_pipeline(main_dir)
