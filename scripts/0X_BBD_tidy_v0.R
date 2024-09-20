#------------------------------
# teamshrub_bowman_honours
# 0X_BBD_tidy_v0
# By: Elias Bowman 
# Created: 2024-09-19
# 
# Description: This script will import and tidy the Ranger collected Breeding Bird Data from Qikiqtaruk - Herschel Island Territorial Park 
#
#------------------------------

# Importing Packages ####
library(dplyr)
library(tidyverse)
library(skimr)
library(lubridate)
library(ggplot2)

# Importing Data
bbs.data.path <- "D:/breeding_bird_survey_QHI_2024/QHI_BBS_survey_data_1990_2024.csv"
bbs.weather.path <- "D:/breeding_bird_survey_QHI_2024/QHI_BBS_weather_data_1990_2024.csv"

bbs.survey <- read.csv(bbs.data.path)
bbs.weather <- read.csv(bbs.weather.path)

unique(bbs.survey$SPECIES)

# to do:
# - num of observers
# - date_time formated
# - time spent sampling
# - sampling effort (time spent sampling * num observers)

# Creating a new formatted Date column ####
bbs.survey$DATE_YMD <- paste(bbs.survey$DAY, bbs.survey$MONTH, bbs.survey$YEAR, sep = "-")
# Removing trailing or leading spaces that might cause formatting errors
bbs.survey$DATE_YMD <- trimws(bbs.survey$DATE_YMD)
# Add leading zero for single digit days
bbs.survey$DATE_YMD <- sub("^(\\d{1})-", "0\\1-", bbs.survey$DATE_YMD)

# Convert DATE to proper date format
  # Should be in Year-Month-Date format ex: 2024-09-19
bbs.survey$DATE_YMD <- dmy(bbs.survey$DATE_YMD)

# Creating a new indexing column ####
bbs.survey <- bbs.survey %>%
  mutate(transect_id = paste(DATE, PERIOD, SURVEY_NUM, TRANSECT, sep = "_"))

# Creating a new formatted Time column ####
# Function to standardize time format, adding missing colons
standardize_time <- function(time_str) {
  # Remove any spaces or extra characters
  time_str <- gsub("\\s+", "", time_str)
  
  if (nchar(time_str) == 3 & grepl("^[0-9]{3}$", time_str)) {
    # For cases like "959" -> "9:59"
    time_str <- paste0(substr(time_str, 1, 1), ":", substr(time_str, 2, 3))
  } else if (nchar(time_str) == 4 & grepl("^[0-9]{4}$", time_str)) {
    # For cases like "1030" -> "10:30"
    time_str <- paste0(substr(time_str, 1, 2), ":", substr(time_str, 3, 4))
  }
  
  # If the time is "00:00", return NA
  if (time_str == "00:00"|time_str == "0:00"|time_str == "0"|time_str == "00"|time_str == "000") {
    return(NA)
  }
  
  return(time_str)
}

# Apply function to Start.time and End.time
# Standardize TIME columns, adding missing colons if necessary
bbs.survey$TIME <- sapply(bbs.survey$TIME, standardize_time)
bbs.survey$Start.time <- sapply(bbs.survey$Start.time, standardize_time)
bbs.survey$End.time <- sapply(bbs.survey$End.time, standardize_time)

# Parse columns as Time Data types
bbs.survey$Start.time <- hms::parse_hm(bbs.survey$Start.time)
bbs.survey$End.time <- hms::parse_hm(bbs.survey$End.time)
bbs.survey$TIME <- hms::parse_hm(bbs.survey$TIME)

# Fill in Start and End Times for all observations
bbs.survey <- bbs.survey %>%
  group_by(transect_id) %>%
  fill(Start.time, .direction = "downup") %>%
  fill(End.time, .direction = "downup")

# Creating date_time column
bbs.survey <- bbs.survey %>%
  mutate(date_time = as.POSIXct(paste(DATE_YMD, TIME), format = "%Y-%m-%d %H:%M:%S"))

# Calculate time spent sampling (difference between End.time and Start.time in minutes)
bbs.survey <- bbs.survey %>%
  mutate(time_spent_sampling = as.numeric(difftime(End.time, Start.time, units = "mins")))

# Create a column for number of observers
bbs.survey <- bbs.survey %>%
  mutate(num_observers = sapply(strsplit(Observers, ","), length))

# Calculate sampling effort (time spent sampling * number of observers)
bbs.survey <- bbs.survey %>%
  mutate(sampling_effort = time_spent_sampling * num_observers)
