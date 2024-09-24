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

# Defining Relevant Functions ####

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

# Function to format column names
format_column_names <- function(df) {
  colnames(df) <- colnames(df) %>%
    tolower() %>%     # Convert to lowercase
    gsub("_", ".", .) # Replace underscores with dots
  return(df)
}

# Function to calculate sampling time and effort
calculate_sampling_effort <- function(data) {
  data <- data %>%
    mutate(sampling.time = as.numeric(difftime(End.time, Start.time, units = "mins")),
           num_observers = sapply(strsplit(Observers, ","), length),
           sampling_effort = sampling.time * num_observers)
  
  return(data)
}

# Modifying and Tidying Existing Dataframe ####
# Giving each observation an id
bbs.survey <- bbs.survey %>%
  mutate(observation.id = row_number())

# Filling in total with 1 when NA or 0, and noting it in a new column
bbs.survey <- bbs.survey %>%
  mutate(
    # Create a new boolean column indicating if total was changed
    total_changed = ifelse(is.na(TOTAL) | TOTAL == 0, TRUE, FALSE),
    
    # Replace NA and 0 with 1
    TOTAL = ifelse(is.na(TOTAL) | TOTAL == 0, 1, TOTAL)
  )

# Create a new formatted Date column ####
bbs.survey$DATE_YMD <- paste(bbs.survey$DAY, bbs.survey$MONTH, bbs.survey$YEAR, sep = "-") %>%
  trimws() %>%                         # Remove spaces
  sub("^(\\d{1})-", "0\\1-", .)       # Add leading zero for single digit days

# Convert DATE to proper date format (Year-Month-Day)
bbs.survey$DATE_YMD <- dmy(bbs.survey$DATE_YMD)

# Amalgamate notes into a single column
bbs.survey <- bbs.survey %>%
  mutate(notes = case_when(
    trimws(NOTES) != "" & trimws(X) != "" ~ paste(NOTES, X, sep = ", "),  # Both notes exist
    trimws(NOTES) != "" ~ NOTES,                                            # Only NOTES exists
    trimws(X) != "" ~ X,                                                    # Only X exists
    TRUE ~ ""                                                               # Both notes are NA or empty
  )) %>%
  select(-NOTES, -X)  # Remove original note columns

# Create two new indexing columns ####
bbs.survey <- bbs.survey %>%
  mutate(transect_id = paste(DATE, PERIOD, SURVEY_NUM, TRANSECT, sep = "_")) %>%
  mutate(survey_id = paste(DATE, PERIOD, SURVEY_NUM, sep = "_"))

# Standardize TIME columns ####
bbs.survey$TIME <- sapply(bbs.survey$TIME, standardize_time)
bbs.survey$Start.time <- sapply(bbs.survey$Start.time, standardize_time)
bbs.survey$End.time <- sapply(bbs.survey$End.time, standardize_time)

# Parse columns as Time Data types
bbs.survey$Start.time <- hms::parse_hm(bbs.survey$Start.time)
bbs.survey$End.time <- hms::parse_hm(bbs.survey$End.time)
bbs.survey$TIME <- hms::parse_hm(bbs.survey$TIME)

# Create date_time column ####
bbs.survey <- bbs.survey %>%
  mutate(date_time = as.POSIXct(paste(DATE_YMD, TIME), format = "%Y-%m-%d %H:%M:%S"))

# Fill in Start and End Times for all observations ####
bbs.survey <- bbs.survey %>%
  group_by(transect_id) %>%
  fill(Start.time, .direction = "downup") %>%
  fill(End.time, .direction = "downup")

bbs.survey.temp <- bbs.survey %>%
  filter((End.time - Start.time) < 5 | is.na(Start.time) | is.na(End.time)) %>%  # Select rows with less than 5 mins of sampling time or NA values
  group_by(transect_id) %>%
  mutate(
    # Fill Start.time with min(TIME) if NA and if there are valid TIME values
    Start.time = ifelse(is.na(Start.time), 
                        ifelse(any(!is.na(TIME)), hms::as_hms(min(TIME, na.rm = TRUE)), NA), 
                        Start.time),
    
    # Fill End.time with max(TIME) if NA and if there are valid TIME values
    End.time = ifelse(is.na(End.time), 
                      ifelse(any(!is.na(TIME)), hms::as_hms(max(TIME, na.rm = TRUE)), NA), 
                      End.time),
    
    # If Start.time is filled but End.time is still NA, fill End.time with Start.time if there's only one unique time
    End.time = ifelse(is.na(End.time) & !is.na(Start.time) & n_distinct(na.omit(TIME)) == 1, 
                      Start.time, End.time),
    
    # Ensure End.time is corrected if sampling time is less than 5 minutes
    End.time = ifelse((End.time - Start.time) < 5, 
                      ifelse(any(!is.na(TIME)), hms::as_hms(max(TIME, na.rm = TRUE)), End.time), 
                      End.time),
    
    end.time.filled = TRUE
  ) %>%
  ungroup()

# Reintroduce modified bbs.survey with corrected end times
bbs.survey <- bbs.survey %>%
  mutate(end.time.filled = FALSE) %>%
  rows_update(bbs.survey.temp, by = c("observation.id"))

# Create a transect level and survey level dataset ####
bbs.survey.transect <- bbs.survey
bbs.survey.temp <- bbs.survey %>%
  group_by(survey_id) %>%
  summarize(
    # Get the earliest start time and latest end time for each survey.id
    End.time = hms::as_hms(max(End.time, na.rm = TRUE)),
    Start.time = hms::as_hms(min(Start.time, na.rm = TRUE)),
    
    .groups = 'drop'  # Ungroup after summarizing
  )

# Join the summarized dataset back to the original, dropping the old start.time and end.time
bbs.survey <- bbs.survey %>%
  select(-Start.time, -End.time) %>%  # Remove old start.time and end.time
  left_join(bbs.survey.temp, by = "survey_id")  # Join with the summary

# Calculate Sampling Time and Effort
bbs.survey <- calculate_sampling_effort(bbs.survey)
bbs.survey.transect <- calculate_sampling_effort(bbs.survey.transect)


# Reformat Dataframe Column Names ####
bbs.survey <- format_column_names(bbs.survey)
bbs.survey.transect <- format_column_names(bbs.survey.transect)

# Reformat column order
bbs.survey <- bbs.survey %>%
  select(
    observation.id,
    survey.id,
    date.ymd,
    species,
    spec.code,
    total,
    time,
    sampling.time,
    num.observers,
    sampling.effort,
    breed,
    behaviour,
    notes,
    observers,
    date,
    period,
    year,
    month,
    day,
    survey.num,
    transect,
    start.time,
    end.time,
    rec.num,
    end.time.filled,
    transect.id
  )

# Create long format version
bbs.long <- bbs.survey %>%
  uncount(total) 
