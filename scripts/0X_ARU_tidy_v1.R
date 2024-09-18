#------------------------------
# teamshrub_bowman_honours
# 0X_ARU_tidy_v1
# By: Elias Bowman
# Created: 2024-07-05
# Last update: 2024-09-07
# 
# Description: This script will import and work with the massive ARU data received from the devices deployed around Qikiqtaruk - Herschel Island. I intend this script to do the initial processing and tidying of the data. This will likely involve importing each file. Confirming its not corrupted and is functional. Trimming off the start end of the clips of the recording process.
#
#
#------------------------------

# Importing Packages ####
library(seewave)
library(tuneR)
library(stringr)

# Designing Functions ####
is_ten_minutes <- function(file) { # function quickly checks if a file is 10 minutes
  # previous scripts used duration() which requires fully loading the file
  # this function only needs to read the header information
  tryCatch({
  wave_info <-
    tuneR::readWave(file,
                    from = 0,
                    to = 1,
                    header = TRUE)
  
  # calculate duration using samples and sampling rate
  num_samples <- wave_info$samples
  samp_rate <- wave_info$sample.rate
  audio_duration <- num_samples / samp_rate
  
  # check if the duration is exactly 600 seconds (10 minutes)
  return(audio_duration == 600)
  }, error = function(e) {
    # Handle errors and return FALSE if the file is not valid
    cat("Error reading file:", file, "\n")
    return(FALSE)
  })
}

is_valid_wav <- function(file) { # functions confirms file is valid wave file type
  return(grepl("\\.wav$", file, ignore.case = TRUE))
}

# Importing Data ####
  # note: add related large files to .gitignore
main_dir <- "D:/ARU_QHI_2024" # designating hard drive ARU data file
dir_list <-
  list.dirs(main_dir, recursive = FALSE) # creating a list of directories within the ARU_QHI_2024 folder

# Iterating through All ARU Directories
for (dir in dir_list) {
  #access the data directory within the folder directory
  data_dir <- file.path(dir, "data")
  
  # Path for the log file that will contain names of files to ignore
  log_file <- file.path(data_dir, paste0("toignore", str_extract(data_dir, "ARUQ[^_]+"),".csv"))
  
  # Create the log file if it doesn't exist, and write the header
  if (!file.exists(log_file)) {
    write.csv(
      data.frame(
        File = character(),
        Duration = numeric(),
        Reason = character()
      ),
      log_file,
      row.names = FALSE
    )
  }
  
  # Check if the file is a .wav file
  if (!is_valid_wav(file)) {
    write.table(data.frame(File = filename, Duration = NA, Reason = "Not a .wav file"), log_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
    cat("File logged due to not being a .wav file:", filename, "\n")
    next
  }
  
  # list all audio files (.wav) in the current directory
  files <-
    list.files(data_dir, pattern = "\\.wav$", full.names = TRUE)
  
  # loop through each ARU .wav file
  for (file in files) {
    # extract the filename without the path
    filename <- basename(file)
    
    # check if the file ends with "0000" or "3000"
    ends_with_correct_suffix <-
      grepl("0000.wav$", filename) || grepl("3000.wav$", filename)
    
    # If the filename doesn't meet the criteria, log it and skip further processing
    if (!ends_with_correct_suffix) {
      # log the file name with reason for logging
      write.table(
        data.frame(
          File = filename,
          Duration = NA,
          Reason = "Incorrect filename suffix"
        ),
        log_file,
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE,
        sep = ","
      )
      
      cat("File logged due to incorrect suffix:", filename, "\n")
      
      # skip further processing for this file
      next
    }
    
    # implementing is_ten_minutes function
    if (!is_ten_minutes(file)) {
      write.table(
        data.frame(
          File = filename,
          Duration = NA,
          Reason = "Incorrect duration"
        ),
        log_file,
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE,
        sep = ","
      )
      cat("File logged due to incorrect length:", filename, "\n")
      
    }
  }
}