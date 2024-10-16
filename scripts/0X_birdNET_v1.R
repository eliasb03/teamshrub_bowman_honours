#------------------------------
# teamshrub_bowman_honours
# 0X_birdNET_v1
# By: Elias Bowman 
# Created: 2024-07-05
# Last update: 2024-10-16
# 
# Description: This script creates a function to execute python command line prompts to a locally installed BirdNET Analyzer (the Cornell Lab bird ID/classifier) software
#------------------------------

# specifying global variables
birdnet_path <- '"C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe"' # path to birdnet on this computer
qhi_latitude <- 69.5
qhi_longitude <- -138.9

# function to carry out BirdNET analysis 
  # all variables specified and optional in function call
  # default values are assigned, but can be overwritten when calling funciton
birdNET_analyze <- function(input, output, week = -1, sensi = 1, conf = 0.5, overlap = 1.5, rtype = "csv"){
  birdnet_analyzer_path <- birdnet_path
  input_path <- input
  output_path <- output
  rt <- rtype
  wk <- week # week of data
  ol <- overlap # how many seconds each test should overlap, measurements are in 3s clips
  min_conf <- conf # minimum confidence threshold
  sensitivity <- sensi # sensitivity of detection, in range of 0.5 - 1.5
  
  to_execute <-
    paste0(birdnet_path,
           " --i ", input_path,
           " --o ", output_path,
           " --lat ", qhi_latitude,
           " --lon ", qhi_longitude,
           " --week ", wk,
           " --sensitivity ", sensitivity,
           " --min_conf ", min_conf,
           " --overlap ", ol,
           " --rtype ", rt
    )
  
  system(to_execute)
}


## Example Inputs:
# input_audio <- '"C:/Users/elias/OneDrive/Documents/Audacity/test3_bird_audio_july8.wav"'
# output_file <- '"C:/Users/elias/OneDrive/Desktop/test_birdnet_output/function_test6_17Sept2024.csv"'

## Example Calls:
# birdNET_analyze(input_audio, output_file) # example without additional parameters
# birdNET_analyze(input_audio, output_file, -1, 0.5, 0.5, 0, "csv") # example with parameters specified 
