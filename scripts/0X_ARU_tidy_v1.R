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
# Importing Data ####
main_dir <- "D:/ARU_QHI_2024" 
dir_list <- list.dirs(main_dir, recursive = FALSE)

# Iterating through All ARU Directories ####
for (dir in dir_list) {
  cat("Processing directory:", dir, "\n") # Debugging statement
  
  data_dir <- file.path(dir, "data")
  
  # Extract ARU ID for log file name
  aru_id <- str_extract(data_dir, "ARUQ[^_]+")
  log_file <- file.path(data_dir, paste0("toignore_", aru_id, ".csv"))
  
  # Check if the log file exists and load its contents
  existing_logs <- NULL
  if (file.exists(log_file)) {
    existing_logs <- read.csv(log_file, stringsAsFactors = FALSE)
    cat("Log file already exists with", nrow(existing_logs), "entries.\n")
  } else {
    # If no log file exists, initialize an empty data frame
    existing_logs <- data.frame(File = character(), Duration = numeric(), Reason = character(), stringsAsFactors = FALSE)
  }
  
  # list all audio files (.wav) in the current directory
  files <- list.files(data_dir, pattern = "\\.wav$", full.names = TRUE)
  
  # loop through each ARU .wav file
  for (file in files) {
    filename <- basename(file)
    
    # Check if the file is already logged
    if (filename %in% existing_logs$File) {
      cat("File already logged, skipping:", filename, "\n")
      next
    }
    
    # Check if the file is a .wav file
    if (!is_valid_wav(file)) {
      cat("File is not a .wav file:", filename, "\n")
      new_entry <- data.frame(File = filename, Duration = NA, Reason = "Not a .wav file", stringsAsFactors = FALSE)
      existing_logs <- rbind(existing_logs, new_entry)
      next
    }
    
    # Check filename suffix
    ends_with_correct_suffix <- grepl("0000.wav$", filename) || grepl("3000.wav$", filename)
    if (!ends_with_correct_suffix) {
      cat("File has incorrect suffix:", filename, "\n")
      new_entry <- data.frame(File = filename, Duration = NA, Reason = "Incorrect filename suffix", stringsAsFactors = FALSE)
      existing_logs <- rbind(existing_logs, new_entry)
      next
    }
    
    # Check if the file is 10 minutes
    if (!is_ten_minutes(file)) {
      cat("File has incorrect duration:", filename, "\n")
      new_entry <- data.frame(File = filename, Duration = NA, Reason = "Incorrect duration", stringsAsFactors = FALSE)
      existing_logs <- rbind(existing_logs, new_entry)
    }
  }
  
  # Write the updated log file, ensuring no duplicates
  write.csv(existing_logs, log_file, row.names = FALSE)
  cat("Log file updated with new entries.\n")
}
