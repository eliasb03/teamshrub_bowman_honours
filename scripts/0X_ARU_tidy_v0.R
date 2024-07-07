#------------------------------
# teamshrub_bowman_honours
# 0X_ARU_tidy_v0
# By: Elias Bowman 
# 2024-07-05
# Description: This script will import and work with the massive ARU data received from the devices deployed around Qikiqtaruk - Herschel Island. I intend this script to do the initial processing and tidying of the data. This will likely involve importing each file. Confirming its not corrupted and is functional. Trimming off the start end of the clips of the recording process.
#
#  
#------------------------------

# importing relevant packages
library(seewave)
library(tuneR)

# importing data from hard drive
  # note: add related large files to .gitignore

# file_list <- list.files(path = "D:/Bowman_birds/ARU_recordings")

main_dir <- "D:/Bowman_birds/ARU_recordings"
dir_list <- list.dirs(main_dir, recursive = FALSE)


for (dir in dir_list) {
  # access the data directory within the folder directory
  data_dir <- file.path(dir, "data") # directory for already exisiting data
  
  # these subdirectories will exist within data, as subdirectories
  new_dir <- file.path(data_dir, "audio") # directory for the newly processed data, good audio data with the proper length
  if (!dir.exists(new_dir)) { # checking the directory does not already exist, and then creating it if it doesn't
    dir.create(new_dir, recursive = TRUE)
  }
  
  amusing_dir <- file.path(data_dir, "potentially_amusing_audio") # directory for bad audio, less than 10 minutes, could be funny
  if (!dir.exists(amusing_dir)) { # checking if this directory already exists, and if not it creates it
    dir.create(amusing_dir, recursive = TRUE)
  }
  
  # list all audio files (.wav) in the current directory
  files <- list.files(data_dir, pattern = "\\.wav$", full.names = TRUE)
  
  # loop through each of the ARU .wav file
  for (file in files) {
    # read audio files
    audio <- readWave(file)
    
    # process the audio file (placeholder for your processing code)
    # for example, to get the duration of the audio file:
    duration <- duration(audio)
    cat("Processing file:", file, "- Duration:", duration, "seconds\n")
    
    # if the duration is exactly 10 minutes (600 seconds), rename and move the file
      # Files less than 10 minutes are either erronous, or pre-deployment
    if (duration == 600) {
      # create the new file path for the new directory, with only good files
      new_file_path <- file.path(new_dir, basename(file))
      
      # rename and move the file
      file.rename(file, new_file_path)
      
      cat("File renamed and moved to:", new_file_path, "\n")
    } else {
      # create the new file path for the new directory, with only good files
      new_file_path2 <- file.path(amusing_dir, basename(file))
      
      # rename and move the file
      file.rename(file, new_file_path2)
      
      cat("File renamed and moved to:", new_file_path2, "\n")
    }
    
  }
  
}

