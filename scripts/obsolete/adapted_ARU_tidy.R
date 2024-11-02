# Adapted ARU Tidy

library(seewave)
library(tuneR)

main_dir <- "E:/ARU_QHI_2024"
output_dir <- "D:/Bowman_birds/ARU_recordings"
ez_start_dir <- file.path(main_dir, "ARUQ1_17Aug2024")
ez_start_dir_list <- list.dirs(ez_start_dir, recursive = FALSE)

# list all audio files (.wav) in the current directory
files <- list.files(ez_start_dir_list, pattern = "\\.wav$", full.names = TRUE)

for (file in files) {
  # read audio files
  audio <- readWave(file)
  
}




# importing packages
#devtools::install_github('nationalparkservice/NSNSDAcoustics')
library(NSNSDAcoustics)

birdnet_version <- "v1.1.0"
birdnet_path <- "C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe"

#input_audio <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNet_input/ARUQ1_20240622_170000.wav"
input_audio <- "C:/Users/elias/OneDrive/Documents/Audacity/test3_bird_audio_july8.wav"
output_file <- "C:/Users/elias/OneDrive/Desktop/test_birdnet_output/output_file3.r"

qhi_latitude <- 69 #approx
qhi_longitude <- -139 #approx
sf.thresh <- 0.03 # species occurence threshold for local area, default 0.03

wk <- -1

rtype <- 'r' # defaults to 'r', no need to input unless changing

min.conf <- 0.1 # minimum confidence

fmin <- 0 # minimum freq of bandpass filter
fmax <- 15000# max freq of bandpass filter

######

# Generate a single command to loop through several folders:
## NOTE: be mindful of your quotations when editing!
all.commands <- paste0(
  '"', birdnet_path,
  '" --i "', input_audio,
  '" --o "', output_file,
  '" --lat -1 --lon -1 --week -1'
)
test.commands <- '"C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe" --i "C:/Users/elias/OneDrive/Documents/Audacity/test2_bird_audio_july8.wav" --o "C:/Users/elias/OneDrive/Desktop/test_birdnet_output/output3.csv" --lat -1 --lon -1 --week -1'
system(test.commands)

test_again_commands <- '"C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe" --i "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNet_input/ARUQ1_20240622_170000.wav" --o "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/outputs/test_birdnet_outputs/output_test1.csv" --lat 69 --lon -138 --week -1 --rtype "csv"'

system(test_again_commands)

# Test that one command runs
system(all.commands)

######
bird_analyze <- function(input, output){
  birdnet_analyzer_function <- "C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe"
  input_path <- input
  output_path <- output
  
  
  command_promt <- paste0(birdnet_analyzer_function, " --i ", input_path, " --o ", output_path, " --lat ", qhi_latitude, " --lon ", qhi_longitude, " --week -1", " --rtype ")
  
  system(command_prompt)
}
######

birdnet_analyzer(
  birdnet.version = birdnet_version,
  birdnet.path = birdnet_path,
  i.audio = input_audio,
  o.results = output_folder,
  week <- wk,
  lat = latitude,
  lon = longitude
)
