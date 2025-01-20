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
