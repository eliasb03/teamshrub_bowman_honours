#------------------------------
# teamshrub_bowman_honours
# 0X_birdNET_v1
# By: Elias Bowman 
# Created: 2024-07-05
# Last update: 2024-09-07
# 
# Description: This script will install and start working with BirdNET the Cornell Lab bird ID software
# This script will make a function, that wraps the BirdNET software and can call promts to the command line based on inputs form R 
#------------------------------

#devtools::install_github('nationalparkservice/NSNSDAcoustics')
library(NSNSDAcoustics)

# specifying global variables
birdnet_path <- '"C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe"'# path to birdnet on this computer
qhi_latitude <- 69.5
qhi_longitude <- -138.9


birdNET_analyze <- function(input, output, week, sensi, conf, overlap, rtype){
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


birdNET_analyze_simple <- function(input, output){
  birdnet_analyzer_path <- birdnet_path
  input_path <- input
  output_path <- output
  rtype <- "csv" 
  wk <- -1 # week of data
  overlap <- 1.5 # how many seconds each test should overlap, measurements are in 3s clips
  min_conf <- 0.50 # minimum confidence threshold
  sensitivity <- 1 # sensitivity of detection, in range of 0.5 - 1.5
  
  to_execute <-
    paste0(birdnet_path,
           " --i ", input_path,
           " --o ", output_path,
           " --lat ", qhi_latitude,
           " --lon ", qhi_longitude,
           " --week ", wk,
           " --sensitivity ", sensitivity,
           " --min_conf ", min_conf,
           " --overlap ", overlap,
           " --rtype ", rtype
    )
  
  system(to_execute)
}

## Example Calls:
# birdNET_analyze_simple(input_audio, output_file)
# birdNET_analyze(input_audio, output_file, -1, 0.5, 0.5, 0, csv)

## Example Inputs:
# input_audio <- '"C:/Users/elias/OneDrive/Documents/Audacity/test3_bird_audio_july8.wav"'
# output_file <- '"C:/Users/elias/OneDrive/Desktop/test_birdnet_output/function_test6_17Sept2024.csv"'