#------------------------------
# teamshrub_bowman_honours
# 0X_birdNET_v0
# By: Elias Bowman 
# 2024-07-05
# Description: This script will install and start working with BirdNET the Cornell Lab bird ID software
# This script will make a function, that wraps the BirdNET software and can call promts to the command line based on inputs form R 
#------------------------------

#devtools::install_github('nationalparkservice/NSNSDAcoustics')
library(NSNSDAcoustics)

# specifying global variables
birdnet_path <- '"C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe"'# path to birdnet on this computer
qhi_latitude <- 69.5
qhi_longitude <- -138.9


bird_analyze <- function(input, output, week, sensi, conf, overlap, rtype){
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


bird_analyze_simple <- function(input, output){
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
# bird_analyze_simple(input_audio, output_file)
# bird_analyze(input_audio, output_file, -1, 0.5, 0.5, 0, csv)





##########
# 
# 
# 
# 
# fmin <- 0 # min freq of bandpass filter
# fmax <- 15000 # max freq of bandpass filter
# 
# command.promt <- paste0(
#   birdnet_path,
#   ' --i ', input_audio,
#   ' --o ', output_file
# )
# command.promt
# system(command.promt)
# 
# 
# command_prompt <-
#   paste0(birdnet_path,
#          " --i ", input_audio,
#          " --o ", output_file,
#          " --lat ", qhi_latitude,
#          " --lon ", qhi_longitude,
#          " --week -1",
#          " --rtype csv"
#   )
# system(command_prompt)
# 
# 
# ######
# 
# # Generate a single command to loop through several folders:
# ## NOTE: be mindful of your quotations when editing!
# all.commands <- paste0(
#   birdnet_path,
#   '" --i "', input_audio,
#   '" --o "', output_file,
#   '" --lat -1 --lon -1 --week -1'
# )
# all.commands
# # Test that one command runs
# system(all.commands)
# 
# prelim.commands <- paste0(
#   birdnet_path,
#   ' --i ', input_audio,
#   ' --o ', output_file
# )
# prelim.commands
# system(prelim.commands)
# 
# test.commands <- '"C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe" --i "C:/Users/elias/OneDrive/Documents/Audacity/test2_bird_audio_july8.wav" --o "C:/Users/elias/OneDrive/Desktop/test_birdnet_output/test2_17Sept2024.csv" --lat -1 --lon -1 --week -1'
# test.commands
# system(test.commands)
# 
# # test_again_commands <- '"C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe" --i "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNet_input/ARUQ1_20240622_170000.wav" --o "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/outputs/test_birdnet_outputs/output_test1_17Sept2024.csv" --lat 69 --lon -138 --week -1 --rtype "csv"'
# # 
# # system(test_again_commands)
# 
# 
# 
# ######
# bird_analyze <- function(input, output){
#   birdnet_analyzer_function <- "C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe"
#   input_path <- input
#   output_path <- output
#   
#   
#   command_promt <- paste0(birdnet_analyzer_function, " --i ", input_path, " --o ", output_path, " --lat ", qhi_latitude, " --lon ", qhi_longitude, " --week -1", " --rtype ")
#   
#   system(command_prompt)
# }
# ######
# 
# birdnet_analyzer(
#   birdnet.version = birdnet_version,
#   birdnet.path = birdnet_path,
#   i.audio = input_audio,
#   o.results = output_folder,
#   week <- wk,
#   lat = latitude,
#   lon = longitude
# )
# 
# 
# 
# birdnet_func <- function(input, output, lat, lon, week)

