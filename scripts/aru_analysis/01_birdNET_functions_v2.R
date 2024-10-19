#------------------------------
# teamshrub_bowman_honours
# 01_birdNET_functions_v2
# By: Elias Bowman 
# Created: 2024-07-05
# Last update: 2024-10-16
# 
# Description: This script creates a function to execute python command line prompts to a locally installed BirdNET Analyzer (the Cornell Lab bird ID/classifier) software. This script also works with the NSNSDAcoustics to wrap functions with relevant modifications to my workflow
# BirdNET Analyzer: https://github.com/kahst/BirdNET-Analyzer
# https://kahst.github.io/BirdNET-Analyzer/
# 
#------------------------------

# Loading Packages
library(devtools)
library(data.table)
#devtools::install_github('nationalparkservice/NSNSDAcoustics')
library(NSNSDAcoustics)
library(lubridate)
library(viridis) 
library(dplyr)

# Global Variables ####
birdNET_path <- "C:/Program Files (x86)/BirdNET-Analyzer/BirdNET-Analyzer.exe" # local path to BirdNET Analyzer
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
birdNET_analyze <- function(input, output, week = -1, sensi = 1, conf = 0.1, overlap = 1.5, 
                            rtype = "r", lat = qhi_latitude, lon = qhi_longitude, 
                            species_list = NULL, thread_count = NULL, show_warnings = TRUE) {
  
  # Ensure the output file name is correctly formatted for "r" type exports
  output_file <- if (rtype == "r") {
    sub("\\.csv$", ".BirdNET.results.csv", output)
  } else {
    output
  }
  
  # Prepare the base command with mandatory parameters
  to_execute <- paste0(
    " --i ", input,
    " --o ", output_file,
    " --lat ", lat,
    " --lon ", lon,
    " --week ", week, 
    " --sensitivity ", sensi, 
    " --min_conf ", conf, 
    " --overlap ", overlap, 
    " --rtype ", rtype
  )
  
  # Conditionally add species list and thread count if provided
  if (!is.null(species_list)) to_execute <- paste0(to_execute, " --slist ", species_list)
  if (!is.null(thread_count)) to_execute <- paste0(to_execute, " --threads ", thread_count)
  
  # Display the command to be executed
  cat("Executing command line prompt:\n", birdNET_path, to_execute, "\n")
  
  # Execute the command with or without warnings based on show_warnings
  output <- system2(birdNET_path, args = to_execute, stdout = TRUE, stderr = if (show_warnings) "" else NULL)
  
  # Check if there was an error
  if (!is.null(attr(output, "status")) && attr(output, "status") != 0) {
    cat("Error occurred during execution:\n")
    cat(output, sep = "\n")
  } else {
    cat("Execution completed successfully.\n")
  }
  
  return(output)
}

birdNET_batch_analyze <- function(input, output, week = -1, sensi = 1, conf = 0.1, overlap = 1.5, 
                                  rtype = "r", lat = qhi_latitude, lon = qhi_longitude, 
                                  species_list = NULL, thread_count = NULL, show_warnings = TRUE) {
  
  # Prepare the base command with mandatory parameters
  to_execute <- paste0(
    " --i ", input,
    " --o ", output,
    " --lat ", lat,
    " --lon ", lon,
    " --week ", week, 
    " --sensitivity ", sensi, 
    " --min_conf ", conf, 
    " --overlap ", overlap, 
    " --rtype ", rtype
  )
  
  # Conditionally add species list and thread count if provided
  if (!is.null(species_list)) to_execute <- paste0(to_execute, " --slist ", species_list)
  if (!is.null(thread_count)) to_execute <- paste0(to_execute, " --threads ", thread_count)
  
  # Display the command to be executed
  cat("Executing command line prompt:\n", birdNET_path, to_execute, "\n")
  
  # Execute the command with or without warnings based on show_warnings
  output <- system2(birdNET_path, args = to_execute, stdout = TRUE, stderr = if (show_warnings) "" else NULL)
  
  # Check if there was an error
  if (!is.null(attr(output, "status")) && attr(output, "status") != 0) {
    cat("Error occurred during execution:\n")
    cat(output, sep = "\n")
  } else {
    cat("Execution completed successfully.\n")
  }
  
  return(output)
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


#################################################
# FOLLOWING EXPERIMENTATION - NOT USED IN PROJECT YET

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
birdNET_bar <- function(dat, f.species = NULL, f.colors = NULL) {
  if(is.null(f.colors) && !(is.null(f.species))){
    f.colors = viridis(length(f.species))
  }
  dat$dateTimeLocal <- as.POSIXct(dat$dateTimeLocal)
  dat <- as.data.table(dat)
  
  birdnet_barchart(
    data = dat,
    interactive = FALSE,
    focal.species = f.species,
    focal.colors = f.colors,
    julian.breaks = seq(from = 173, to = 186, by = 1)
  )
  
}

birdNET_bar_interactive <- function(dat) {
  dat$dateTimeLocal <- as.POSIXct(dat$dateTimeLocal)
  dat <- as.data.table(dat)
  
  birdnet_barchart(
    data = dat,
    interactive = TRUE,
    julian.breaks = seq(from = 173, to = 186, by = 1)
  )
  
}


# Example Calls
# plot_data <- read.csv("C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input/ARUQ2_17Aug2024/Output/gathered_results.csv")
#plot_data <- read.csv("C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/birdNET_input/ARU_combined_formatted_results.csv")


# plot_data <- read.csv("D:/ARU_QHI_2024/ARUQ1_17Aug2024/Output/gathered_results.csv")
#species <- c("Snow Bunting","Lapland Longspur", "Savannah Sparrow")
#colors <- c('#00BE67', '#C77CFF', '#c51b8a', '#c26b2a')
#birdNET_bar(plot_data, f.species = species, f.colors = colors)
# birdNET_bar_interactive(plot_data)

##############
# birdnet_heatmap(
#   data = plot_data,
#   locationID = 'qhi',
#   common.name = 'Savannah Sparrow',
#   conf.threshold = 0.2,
#   dates.sampled = plot_data$date,
#   julian.breaks = seq(from = 173, to = 186, by = 1),
#   comparable.color.breaks = FALSE
# )
# #############
# sp.list <- c("Snow Bunting", "Lapland Longspur", "Savannah Sparrow", "Semipalmated Plover")
# 
# # Empty list to store plots
# plots <- list()
# 
# # Loop through species and generate heatmaps
# for (i in 1:length(sp.list)) {
#   
#   print(paste0('Working on ', sp.list[i]))
#   
#   # Generate the heatmap for the current species
#   g <- birdnet_heatmap(
#     data = plot_data,
#     locationID = 'qhi',
#     common.name = sp.list[i],
#     conf.threshold = 0.2,
#     dates.sampled = plot_data$date,
#     julian.breaks = seq(from = 173, to = 186, by = 1),
#     comparable.color.breaks = TRUE
#   )
#   
#   # Store the plot in the list
#   plots[[i]] <- g
# }
# 
# # Display the 4 heatmaps in a grid
# cowplot::plot_grid(plotlist = plots, ncol = 2)



        