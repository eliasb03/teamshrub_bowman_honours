#------------------------------
# teamshrub_bowman_honours
# 0X_ARU_tidy_v0
# By: Elias Bowman
# 2024-07-05
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
  
  #################
  #
  #   # read the audio file if it passed the suffix check
  #   audio <- readWave(file)
  #
  #   # get the duration of the audio file
  #   audio_duration <- duration(audio)
  #   cat("Processing file:", file, "- Duration:", audio_duration, "seconds\n")
  #
  #   # if the duration is not exactly 600 seconds, log the file
  #   if (audio_duration != 600) {
  #     # log the file name and duration to the 'toignore.csv' file with reason for logging
  #     write.table(data.frame(File = filename, Duration = audio_duration, Reason = "Incorrect duration"), log_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
  #
  #     cat("File logged due to incorrect duration:", filename, "\n")
  #   }
  # }
  
  #}
  
  ##########
  
  #
  # # temporarily removed loop to deal with all ARU data
  # #for (dir in dir_list) {
  #   # access the data directory within the folder directory
  #   data_dir <- file.path(dir, "data") # directory for already exisiting data
  #
  #   # # these subdirectories will exist within data, as subdirectories
  #   # new_dir <- file.path(data_dir, "audio") # directory for the newly processed data, good audio data with the proper length
  #   # if (!dir.exists(new_dir)) { # checking the directory does not already exist, and then creating it if it doesn't
  #   #   dir.create(new_dir, recursive = TRUE)
  #   # }
  #
  #   # amusing_dir <- file.path(data_dir, "potentially_amusing_audio") # directory for bad audio, less than 10 minutes, could be funny
  #   # if (!dir.exists(amusing_dir)) { # checking if this directory already exists, and if not it creates it
  #   #   dir.create(amusing_dir, recursive = TRUE)
  #   # }
  #
  #   # path for the log file that will contain names of files to ignore
  #   log_file <- file.path(data_dir, "toignore.csv")
  #
  #   # create the log file if it doesn't exist, and write the header
  #   if (!file.exists(log_file)) {
  #     write.csv(data.frame(File = character(), Duration = numeric()), log_file, row.names = FALSE)
  #   }
  #
  #   # list all audio files (.wav) in the current directory
  #   files <- list.files(data_dir, pattern = "\\.wav$", full.names = TRUE)
  #
  #   # loop through each of the ARU .wav file
  #   for (file in files) {
  #
  #
  #     # read audio files
  #     audio <- readWave(file)
  #
  #     # process the audio file (placeholder for your processing code)
  #     # for example, to get the duration of the audio file:
  #     audio_duration <- duration(audio)
  #     cat("Processing file:", file, "- Duration:", audio_duration, "seconds\n")
  #
  #     # if the duration is not exactly 10 minutes (600 seconds), rename and move the file to the junk/amusing audio file
  #       # Files less than 10 minutes are either erronous, or pre-deployment
  #     if (audio_duration != 600) {
  #
  #
  #       # log the file name and duration to the 'toignore.csv' file
  #       write.table(data.frame(File = basename(file), Duration = audio_duration), log_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
  #
  #       cat("File logged:", basename(file), "\n")
  #
  #       # # create the new file path for the new directory, with only good files
  #       # new_file_path <- file.path(amusing_dir, basename(file))
  #       #
  #       # # rename and move the file
  #       # file.rename(file, new_file_path)
  #       #
  #       # cat("File renamed and moved to:", new_file_path, "\n")
  #     }
  #
  #   }
  #
  # #}
  