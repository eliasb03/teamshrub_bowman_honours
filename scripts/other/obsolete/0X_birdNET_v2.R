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

# Loading Packages
library(devtools)
library(data.table)
library(NSNSDAcoustics) #devtools::install_github('nationalparkservice/NSNSDAcoustics')
library(viridis) 
library(dplyr)

# Global Variables ####
birdnet_path <- '"C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe"' # local path to BirdNET Analyzer
# Main directory where ARU data is stored in hierarchical structure
main_dir <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input"
# Field site specific variables
qhi_latitude <- 69.5
qhi_longitude <- -138.9
qhi_timezone <- "America/Dawson"


#------------------------------
# BirdNET Analysis Command Line Function ####
#------------------------------
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

#------------------------------
# BirdNET Format and Gather Function ####
#------------------------------
# Function to format and join BirdNET Results
  # function implements three NSNSDAcoustics functions together
  # function both fills results_dir with formatted files and returns a gathered dataframe
gather_birdNET_results <- function(results_dir, timezone = qhi_timezone) {
  # Reformat all raw BirdNET results
  birdnet_format(
    results.directory = results_dir, 
    timezone = timezone  # Pass the timezone as an argument
  )
  
  # Gather all formatted results into one data.frame
  gathered_results <- birdnet_gather(
    results.directory = results_dir, 
    formatted = TRUE  # Gather the formatted results
  )
  
  # Add time columns using the qhi_timezone variable for both recorder and local time
  gathered_results <- add_time_cols(
    dt = gathered_results,
    tz.recorder = qhi_timezone,
    tz.local = qhi_timezone
  )
  
  # Add locationID column
  gathered_results$locationID <- "qhi"
  
  # Format dateTimeLocal column
  gathered_results$dateTimeLocal <- as.POSIXct(gathered_results$dateTimeLocal)
  
  
  # Return the combined results
  return(gathered_results)
}

#------------------------------
# NSNSDAcoustics Spectrogram Function ####
#------------------------------
# Function wrapped from NSNSDAcoustics package to plot spectorgram of bird calls
  # specific parameters can be modified in function to alter output
plot_spectro <- function(audio_dir, dataframe, bird_name){
  
  plot.calls <- dataframe[dataframe$common_name == bird, ]
  
  birdnet_spectro(
    data = plot.calls,
    audio.directory = audio_dir,
    title = paste0(bird, " Calls"),
    frq.lim = c(0.5, 12),
    new.window = TRUE,
    spec.col = viridis::viridis(30),
    box = FALSE,
  )
}

## Example Function Call
# example_audio_dir <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input/ARUQ2_17Aug2024/Data"
# example_dataframe <- read.csv("C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input/ARUQ2_17Aug2024/Output/formatted_output.csv")
# bird <- "Lapland Longspur"
# plot_spectro(example_audio_dir, example_dataframe, bird)

#------------------------------
# NSNSDAcoustics Graphing Functions ####
#------------------------------
# Wrapping NSNSDAs graphing functions

# always using static ggplot style (interactive = FALSE)
# optional to add focal species and colors
birdnet_bar <- function(dat, f.species = NULL, f.colors = NULL) {
  if(is.null(f.colors) && !(is.null(f.species))){
    f.colors = viridis(length(f.species))
  }
  dat$dateTimeLocal <- as.POSIXct(dat$dateTimeLocal)
  dat <- as.data.table(dat)
  
  birdnet_barchart(
    data = dat,
    interactive = FALSE,
    focal.species = f.species,
    focal.colors = f.colors
  )
  
}

birdnet_bar_interactive <- function(dat) {
  dat$dateTimeLocal <- as.POSIXct(dat$dateTimeLocal)
  dat <- as.data.table(dat)
  
  birdnet_barchart(
    data = dat,
    interactive = TRUE
  )
  
}


# Example Calls
# plot_data <- read.csv("C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input/ARUQ2_17Aug2024/Output/gathered_results.csv")
#plot_data <- read.csv("C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input/ARU_combined_formatted_results.csv")
# species <- c("Snow Bunting","Lapland Longspur", "Savannah Sparrow")
# colors <- c('#00BE67', '#C77CFF', '#c51b8a', '#c26b2a')
# birdnet_plot(plot_data, f.species = species, f.colors = colors)
# birdnet_plot(plot_data)
# birdnet_plot_interactive(plot_data)
