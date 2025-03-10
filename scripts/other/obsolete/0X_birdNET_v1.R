#------------------------------
# teamshrub_bowman_honours
# 0X_birdNET_v2
# By: Elias Bowman 
# Created: 2024-07-05
# Last update: 2024-10-16
# 
# Description: This script creates a function to execute python command line prompts to a locally installed BirdNET Analyzer (the Cornell Lab bird ID/classifier) software 
# BirdNET Analyzer: https://github.com/kahst/BirdNET-Analyzer
# https://kahst.github.io/BirdNET-Analyzer/
# 
#------------------------------

# installing and loading relevant packages
#install.packages('devtools')
library(devtools)
#devtools::install_github('nationalparkservice/NSNSDAcoustics')
library(NSNSDAcoustics)
#install.packages("data.table")
library(data.table)

# Global Variables ####
birdnet_path <- '"C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe"' # local path to BirdNET Analyzer
# Main directory where ARU data is stored in hierarchical structure
main_dir <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input"
# Field site specific variables
qhi_latitude <- 69.5
qhi_longitude <- -138.9
qhi_timezone <- "America/Dawson"


# BirdNET Analysis Command Function
 # all variables specified and optional in function call
 # default values are assigned, but can be overwritten when calling funciton
birdNET_analyze <- function(input, output, week = -1, sensi = 1, conf = 0.5, overlap = 1.5, 
                            rtype = "r", lat = qhi_latitude, lon = qhi_longitude){
  birdnet_analyzer_path <- birdnet_path
  input_path <- input 
  output_path <- output
  rt <- rtype # output dataframe result type
  wk <- week # week of data
  ol <- overlap # how many seconds each test should overlap, measurements are in 3s clips
  min_conf <- conf # minimum confidence threshold
  sensitivity <- sensi # sensitivity of detection, in range of 0.5 - 1.5
  latitude <- lat
  longitude <- lon
  
  # Ensure the output file name is correctly formatted for "r" type exports
  output_file <- if (rtype == "r") {
    sub("\\.csv$", ".BirdNET.results.csv", output)
  } else {
    output
  }
  
  to_execute <-
    paste0(birdnet_path,
           " --i ", input_path,
           " --o ", output_path,
           " --lat ", latitude,
           " --lon ", longitude,
           " --week ", wk,
           " --sensitivity ", sensitivity,
           " --min_conf ", min_conf,
           " --overlap ", ol,
           " --rtype ", rt
    )
  
  system(to_execute)
}


# Function to format and join BirdNET Results
  # function implements two NSNSDAcoustics functions together
  # function both fills results_dir with formatted files and returns a gathered dataframe
format_birdNET_results <- function(results_dir, timezone = qhi_timezone) {
  # Reformat all raw BirdNET results
  birdnet_format(
    results.directory = results_dir, 
    timezone = timezone  # Pass the timezone as an argument
  )
  
  # Gather all formatted results into one data.frame
  formatted_results <- birdnet_gather(
    results.directory = results_dir, 
    formatted = TRUE  # Gather the formatted results
  )
  
  # Function to save combined results to a .csv file
  write.csv(formatted_results, file.path(results_dir, "gathered_results.csv"), row.names = FALSE)
  
  # Return the combined results
  return(formatted_results)
}



# ######## Testing
# input_dir <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input/ARUQ1_17Aug2024/Data"
# output_dir <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input/ARUQ1_17Aug2024/Test_output2"
# 
# # Function to select files for processing
# select_files_for_processing <- function(input_dir, num_files = 3) {
#   files <- list.files(input_dir, pattern = "\\.wav$", full.names = TRUE)
#   selected_files <- sample(files, num_files)  # Randomly select files
#   return(selected_files)
# }
# 
# # Function to process selected files and run BirdNET analysis
# process_selected_files <- function(input_dir, output_dir, num_files = 3) {
#   selected_files <- select_files_for_processing(input_dir, num_files)
#   
#   for (file in selected_files) {
#     file_name <- basename(file)
#     output_file <- paste0(output_dir, "/", sub("\\.wav$", "", file_name), "_BirdNET.results.r.csv")
#     
#     birdNET_analyze(file, output_file, rtype = "r")
#     cat("Processed:", file_name, "Output saved to:", output_file, "\n")
#   }
# }
# 
# # Combined function to process files and format results
# process_and_format_birdnet_results <- function(input_dir, output_dir, num_files = 3, timezone = qhi_timezone) {
#   # Step 1: Process selected files
#   process_selected_files(input_dir, output_dir, num_files)
#   
#   # Step 2: Format and join the BirdNET results after processing
#   formatted_results <- format_birdNET_results(output_dir, timezone)
#   
#   # Step 3: Return the combined formatted results
#   return(formatted_results)
# }
# 
# # Run the entire process with 5 selected files
# final_results <- process_and_format_birdnet_results(input_dir, output_dir, num_files = 5, timezone = qhi_timezone)
# 
# # Preview the final combined results
# print(final_results)




