#------------------------------
# teamshrub_bowman_honours
# 0X_ARU_process_v0
# By: Elias Bowman 
# Created: 2024-09-07
# 
# Description: This script will use the birdNET_analyze function created in the birdNET script
# This script will lead to the creation of outputted song recognition data based on ARU recordings
#
#------------------------------

library(dplyr)

# Function Declaration
check_and_create_dir <- function(dir_path) { # function to see if a directory exists, if not create it
  # this function declaration could be more useful eslewhere in this project
  if (!dir.exists(dir_path)) {
    cat("Directory does not exist. Creating directory:", dir_path, "\n")
    dir.create(dir_path, recursive = TRUE)
  } else {
    cat("Directory already exists:", dir_path, "\n")
  }
}

# Listing ARU Directories ####
ARU_dir <- "D:/ARU_QHI_2024" # Specify ARU data location
ARU_dirs <- list.dirs(ARU_dir, recursive = FALSE) # Find list of ARU dir within base dir

# Iterating through ARU directories ####
#for(aru in ARU_dirs){ # cycle through ARU data
# temp disable
aru <- "D:/ARU_QHI_2024/ARUQ1_17Aug2024" #temp modification just to test how long one aru takes
  aru_data <- paste0(aru, "/Data") # Specify path to data folder
  all_aru_files <- list.files(aru_data) # Find list of audio files
  toignore_csv <- all_aru_files[grepl("^toignore", all_aru_files) & grepl("\\.csv$", all_aru_files)]
  junk_audio <- read.csv(file.path(aru_data, toignore_csv))$File
  files_to_exclude <- c(junk_audio, toignore_csv)
  aru_files <- all_aru_files[!all_aru_files %in% files_to_exclude]
  
  # Creating output directory and generating file name
  output_dir <- paste0(aru, "/Output") 
  check_and_create_dir(output_dir)
  
  for(audio_file in aru_files){ # cycle through audio files
      input_file <- paste0(aru_data, "/", audio_file)
      output_file <- paste0(output_dir, "/", sub("\\.wav$", "", audio_file), "_output.csv")
      birdNET_analyze_simple(input_file, output_file)
  }
#}
  

  