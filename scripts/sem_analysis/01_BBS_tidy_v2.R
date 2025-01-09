#------------------------------
# teamshrub_bowman_honours
# 0X_BBS_tidy_v2
# By: Elias Bowman 
# Created: 2024-11-17
# Updated: 2024-01-09
#
# Description: This script summarizes Breeding Bird Data into guild levels
# Should only be run after BBS_tidy
#------------------------------

# Setting Project Working Environment ####
proj.path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/"
setwd(proj.path)

# Loading Packages
library(tidyverse)  # For dplyr, tidyr, etc.
library(rlang)      # For tidy evaluation tools
library(dplyr)
library(skimr)
library(lubridate)
library(hms)
library(stringi)

# Declaring and Importing Relevant Data ####
bbs.data.path <- "D:/BBS_QHI_2024/QHI_BBS_survey_data_1990_2024.csv"
bbs.data.path <- "data/raw/breeding_bird_survey_QHI_2024/QHI_BBS_survey_data_1990_2024.csv"

guild.mapping.path <- "D:/bird_guild_mapping.csv"
guild.mapping.path <- "data/raw/bird_guild_mapping.csv"

species.list <- c("Common Eider", "Semipalmated Plover", "Semipalmated Sandpiper", 
                  "Baird's Sandpiper", "Red-necked Phalarope", "Glaucous Gull", 
                  "Lapland Longspur", "Snow Bunting", "Savannah Sparrow", 
                  "Common Redpoll", "Hoary Redpoll")

bbs.survey <- read.csv(bbs.data.path)
guild.mapping <- read.csv(guild.mapping.path)

#------------------------------
# Function Definitions
#------------------------------
# 1. Functions to clean data set ####
# Helper Function: Rename all column names to lowercase
rename_columns_lowercase <- function(df) {
  df %>%
    rename_with(tolower)
}

# Helper function: Convert column names from snake_case to dot notation
convert_column_names_to_dot <- function(df) {
  colnames(df) <- gsub("_", ".", colnames(df))
  return(df)
}

# Helper function: Capitalize species codes
capitalize_species_code <- function(df) {
  df %>%
    mutate(spec.code = toupper(spec.code))
}

# Helper function: Add observation ID and handle NA totals
add_observation_id <- function(df) {
  df %>%
    mutate(
      observation.id = row_number(),
      total.changed = ifelse(is.na(total) | total == 0, TRUE, FALSE),
      total = ifelse(is.na(total) | total == 0, 1, total)
    )
}

# Helper function: Combine notes columns
combine_notes <- function(df) {
  df %>%
    mutate(
      notes.1 = case_when(
        trimws(notes) != "" & trimws(x) != "" ~ paste(notes, x, sep = ", "),
        trimws(notes) != "" ~ notes,
        trimws(x) != "" ~ x,
        TRUE ~ ""
      )
    ) %>%
    select(-notes) %>%
    rename(notes = notes.1)
}

# Helper function: Create indexing columns
create_indexing_columns <- function(df) {
  df %>%
    mutate(
      transect.id = paste(date, period, survey.num, transect, sep = "_"),
      survey.id = paste(date, period, survey.num, sep = "_")
    )
}

# Helper function: Correct species mapping errors
correct_species_mapping <- function(df) {
  manual_updates <- tibble::tribble(
    ~species, ~spec.code,
    "Greater White-fronted Goose", "GWFG",
    "Surf Scoter", "SUSC",
    "bird spp.", "BIRD",
    "Herring Gull", "HERG"
  )
  
  df %>%
    mutate(
      species = case_when(
        species %in% manual_updates$species ~ manual_updates$species[match(species, manual_updates$species)],
        spec.code %in% manual_updates$spec.code ~ manual_updates$species[match(spec.code, manual_updates$spec.code)],
        TRUE ~ species
      ),
      spec.code = case_when(
        species %in% manual_updates$species ~ manual_updates$spec.code[match(species, manual_updates$species)],
        spec.code %in% manual_updates$spec.code ~ manual_updates$spec.code[match(spec.code, manual_updates$spec.code)],
        TRUE ~ spec.code
      )
    )
}

# Main function to clean the dataset
clean_bbs_data <- function(df) {
  df %>%
    rename_columns_lowercase %>%           # Rename columns to lowercase
    convert_column_names_to_dot() %>%      # Convert column names to dot notation
    capitalize_species_code() %>%          # Capitalize species codes
    add_observation_id() %>%               # Add observation ID and handle NA totals
    combine_notes() %>%                    # Combine notes columns
    create_indexing_columns() %>%          # Create indexing columns
    correct_species_mapping()              # Correct species mapping errors
}


# 2. Functions to fix time columns ####
# Helper function: Calculate day of year
calculate_doy <- function(date_column) {
  yday(date_column)
}

# Helper function: Zero-pad day, month, and year and create date string
create_date_string <- function(day, month, year) {
  sprintf("%02d-%02d-%04d", day, month, year)
}

# Helper function: Format the date column
format_date_column <- function(df) {
  df %>%
    mutate(date.ymd = dmy(create_date_string(day, month, year)))
}

# Helper function: Create a date-time column
create_date_time_column <- function(df) {
  df %>%
    mutate(date.time = as.POSIXct(paste(date.ymd, time), format = "%Y-%m-%d %H:%M:%S"))
}

# Helper function: Standardize time format, adding missing colons
standardize_time <- function(time.str) {
  # Remove any spaces or extra characters
  time.str <- gsub("\\s+", "", time.str)
  
  if (nchar(time.str) == 3 & grepl("^[0-9]{3}$", time.str)) {
    # For cases like "959" -> "9:59"
    time.str <- paste0(substr(time.str, 1, 1), ":", substr(time.str, 2, 3))
  } else if (nchar(time.str) == 4 & grepl("^[0-9]{4}$", time.str)) {
    # For cases like "1030" -> "10:30"
    time.str <- paste0(substr(time.str, 1, 2), ":", substr(time.str, 3, 4))
  }
  
  # If the time is "00:00", return NA
  if (time.str == "00:00"|time.str == "0:00"|time.str == "0"|time.str == "00"|time.str == "000"|time.str == ""|time.str == " ") {
    return(NA)
  }
  
  return(time.str)
}

# Helper function: Standardize and parse time columns
standardize_and_parse_times <- function(df) {
  df %>%
    mutate(
      time = sapply(time, standardize_time),
      start.time = sapply(start.time, standardize_time),
      end.time = sapply(end.time, standardize_time)
    ) %>%
    mutate(
      time = hms::parse_hm(time),
      start.time = hms::parse_hm(start.time),
      end.time = hms::parse_hm(end.time)
    )
}

# Helper function: Find the latest start time in a survey
find_latest_end_time <- function(df) {
  df %>%
    group_by(survey.id) %>%
    mutate(
      latest.end = case_when(
        any(!is.na(end.time)) ~ suppressWarnings(
          max(
            c(
              max(as.duration(end.time), na.rm = TRUE),
              max(as.duration(time), na.rm = TRUE)
            ), na.rm = TRUE
          )
        ),
        TRUE ~ NA_real_  # If both end.time and time are missing, return NA
      )
    ) %>%
    ungroup() %>%
    mutate(latest.end = as_hms(latest.end))  # Convert to hms format
}

# Helper function: Find the earliest start time in a survey
find_earliest_start_time <- function(df) {
  df %>%
    group_by(survey.id) %>%
    mutate(
      earliest.start = case_when(
        !all(is.na(start.time)) ~ suppressWarnings(
          min(
            c(
              min(as.duration(start.time), na.rm = TRUE),
              min(as.duration(time), na.rm = TRUE)
            ), na.rm = TRUE
          )
        ),
        all(is.na(start.time)) & !all(is.na(time)) ~ suppressWarnings(min(as.duration(time), na.rm = TRUE)),  # If no start.time, use min(time)
        TRUE ~ NA_real_  # If both start.time and time are missing, return NA
      )
    ) %>%
    ungroup() %>%
    mutate(earliest.start = as_hms(earliest.start))  # Convert to hms format
}
# Helper function: Fill in start and end times for each observation
fill_survey_times <- function(df) {
  df %>%
    group_by(survey.id) %>%
    mutate(
      start.time = earliest.start,
      end.time = latest.end
    ) %>%
    ungroup() %>%
    mutate(start.time = as_hms(start.time)) %>% # Convert to hms format
    mutate(end.time = as_hms(end.time))  # Convert to hms format
  
}

# Big Helper function: Adjust survey times by survey id
adjust_survey_times <- function(df) {
  df <- df %>%
    # Standardize and parse time columns
    mutate(
      time = hms::parse_hm(time),
      start.time = hms::parse_hm(start.time),
      end.time = hms::parse_hm(end.time)
    )
  
  # Apply the helper functions in sequence
  df <- df %>%
    find_earliest_start_time() %>%
    find_latest_end_time() %>%
    fill_survey_times()
  
  return(df)
}

# Helper function: Recode period labels and calculate day of year
recode_period_labels <- function(df) {
  df %>%
    mutate(period = recode(period, FALL = "LATE", `LATE-1` = "LATE")) %>%
    filter(period != "MID") %>%
    mutate(
      period = factor(period, levels = c("EARLY", "LATE")),
      doy = calculate_doy(date.ymd) # Using the helper function here
    )
}

# Main function to clean times
clean_times <- function(df) {
  df %>%
    # Step 1: Format the date column using the helper function
    format_date_column() %>%
    
    # Step 2: Standardize and parse time columns using the helper function
    standardize_and_parse_times() %>%
    
    # Step 3: Create a date-time column using the helper function
    create_date_time_column() %>%
    
    # Step 4: Clean and fill missing or short sampling times using the helper function
    adjust_survey_times() %>%
    
    # Step 5: Recode period labels and calculate day of year using the helper function
    recode_period_labels()
}

# 3. Function to apply guilds ####
# Function to left_join guild mapping data into bbs.data
apply_guilds <- function(data, guild.mapping) {
  
  # Check if 'species' column already exists in the data
  if ("species" %in% colnames(data)) {
    # If species exists, just join and resolve any conflicts
    data <- data %>%
      left_join(guild.mapping, by = "spec.code") %>%
      mutate(species = coalesce(species.x, species.y)) %>%  # Resolve species conflicts
      select(-species.x, -species.y)  # Remove the extra columns
  } else {
    # If species doesn't exist, directly add the species column from guild.mapping
    data <- data %>%
      left_join(guild.mapping, by = "spec.code")
  }
  
  return(data)
}

# 4. Function to calculate sampling effort metric ####

  # Creating Yearly Relative Abundance Metric, scaled totals, etc.
# Function to calculate sampling time, effort, and effort multiplier
calculate_sampling_metrics <- function(data) {
  data <- data %>%
    mutate(
      sampling.time = as.numeric(difftime(end.time, start.time, units = "mins")),
      num.observers = sapply(strsplit(observers, ","), length),
      sampling.effort = sampling.time * num.observers
    )
  
  # Get the min and max sampling effort values
  min.effort <- min(data$sampling.effort, na.rm = TRUE)
  max.effort <- max(data$sampling.effort, na.rm = TRUE)
  
  # Dynamically calculate the max multiplier as the ratio of max to min effort
  if (min.effort == max.effort) {
    data <- data %>%
      mutate(effort.multiplier = 1)
  } else {
    max.multiplier <- max.effort / min.effort
    
    # Scale the effort multiplier to range between 1 and max_multiplier
    data <- data %>%
      mutate(effort.multiplier = 1 + (max.multiplier - 1) * (max.effort - sampling.effort) / (max.effort - min.effort))
  }
  
  return(data)
}

# 5. Function to convert to long ####
# 6. Function to summarize to year level ####
# 7. Function to fill in missing years with 0s ####
fill_missing_years <- function(data, species.col, year.col, count.col, fill.value = 0) {
  # Check if the required columns exist
  if (!all(c(species.col, year.col, count.col) %in% colnames(data))) {
    stop("One or more of the specified columns do not exist in the data.")
  }
  
  # Extract unique species and years
  all.species <- unique(data[[species.col]])
  all.years <- unique(data[[year.col]])
  
  # Create a complete grid of species and years
  complete_data <- expand.grid(
    species = all.species,
    year = all.years
  ) %>%
    rename(
      !!species.col := species, 
      !!year.col := year
    )
  
  # Merge with the original data and fill missing values
  filled.data <- complete.data %>%
    left_join(data, by = c(species.col, year.col)) %>%
    mutate(
      !!count.col := coalesce(.data[[count.col]], fill.value) # Fill with the specified fill_value
    )
  
  return(filled_data)
}

# 6. Function to filter observations to species list

#------------------------------
# Main Processing Pipeline
#------------------------------

bbs.summary.test <- bbs.survey %>%
  clean_bbs_data() %>%
  clean_times() %>%
  apply_guilds(guild.mapping) %>%
  calculate_sampling_metrics() 


# %>%
#   fill_missing_years(
#     species.col = "spec.code", 
#     year.col = "year", 
#     count.col = "total.count"
#   )
  

#------------------------------
# Execution
#------------------------------



