#------------------------------
# teamshrub_bowman_honours
# 0X_BBS_guilds_v0
# By: Elias Bowman 
# Created: 2024-11-17
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

# 1. Functions to clean dataset ####
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

# 2. Functions to fix time columns ####
# Main function to clean times
clean_times <- function(df) {
  df %>%
    # Step 1: Format the date column using the helper function
    format_date_column() %>%
    
    # Step 2: Standardize and parse time columns using the helper function
    standardize_and_parse_times() %>%
    
    # Step 3: Create a date-time column using the helper function
    create_date_time_column() %>%
    
    # Step 4: Fill missing start and end times within groups using the helper function
    fill_missing_times() %>%
    
    # Step 5: Clean and fill missing or short sampling times using the helper function
    clean_sampling_times() %>%
    
    # Step 6: Recode period labels and calculate day of year using the helper function
    recode_period_labels()
}

# Helper function: Calculate day of year
calculate_doy <- function(date_column) {
  yday(date_column)
}

# Helper function: Zero-pad day, month, and year and create date string
create_date_string <- function(day, month, year) {
  sprintf("%02d-%02d-%04d", day, month, year)
}

# Helper function: Fill missing times within groups
fill_missing_group_times <- function(df, group.col, time.cols) {
  df %>%
    group_by(across(all_of(group.col))) %>%
    fill(all_of(time.cols), .direction = "downup") %>%
    ungroup()
}

# Helper function: Adjust incorrect short sampling times
adjust_short_sampling_times <- function(start.time, end.time, valid.times) {
  # Ensure inputs are in a consistent format
  duration <- as.numeric(difftime(end.time, start.time, units = "mins"))
  
  # Adjust short sampling times
  adjusted.end.time <- ifelse(
    duration < 5 & !is.na(start.time),
    hms::as_hms(max(valid.times, na.rm = TRUE)),
    end.time
  )
  
  return(adjusted.end.time)
}

# Helper functio: Summarize start and end times by group
summarize_sampling_times <- function(df, group.col, time.cols) {
  df %>%
    group_by(across(all_of(group.col))) %>%
    summarize(
      across(all_of(time.cols), ~ hms::as_hms(ifelse(all(is.na(.)), NA, min(., na.rm = TRUE)), .groups = 'drop'))
    )
}

# Helper function: Format the date column
format_date_column <- function(df) {
  df %>%
    mutate(date.ymd = dmy(create_date_string(day, month, year)))
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

# Helper function: Create a date-time column
create_date_time_column <- function(df) {
  df %>%
    mutate(date.time = as.POSIXct(paste(date.ymd, time), format = "%Y-%m-%d %H:%M:%S"))
}

# Helper function: Fill missing start and end times within groups
fill_missing_times <- function(df) {
  fill_missing_group_times(df, "transect.id", c("start.time", "end.time"))
}

# Helper function: Clean and fill missing or short sampling times
clean_sampling_times <- function(df, group.col = "survey.id") {
  df %>%
    mutate(
      # Ensure all time columns are <hms>
      start.time = if (!inherits(start.time, "hms")) hms::as_hms(start.time) else start.time,
      end.time = if (!inherits(end.time, "hms")) hms::as_hms(end.time) else end.time,
      time = if (!inherits(time, "hms")) hms::as_hms(time) else time
    ) %>%
    group_by(across(all_of(group.col))) %>%
    mutate(
      # Fill missing start and end times with valid times from `time`
      start.time = coalesce(
        start.time,
        if (all(is.na(time))) NA else hms::as_hms(min(time, na.rm = TRUE))
      ),
      end.time = coalesce(
        end.time,
        if (all(is.na(time))) NA else hms::as_hms(max(time, na.rm = TRUE))
      )
    ) %>%
    # Reapply type enforcement after adjusting times
    mutate(
      end.time = adjust_short_sampling_times(
        start.time,
        end.time,
        valid.times = time
      ),
      end.time = if (!inherits(end.time, "hms")) hms::as_hms(end.time) else end.time
    ) %>%
    ungroup()
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

# # 3. Function to apply guilds
# # apply_guilds(guild.mapping) %>%
# # Function to add observation ID and handle NA totals
# 
# # 3. Functions to calculate sampling metric
# calculate_sampling_metrics() %>%
#   # Creating Yearly Relative Abundance Metric, scaled totals, etc.
# 
# # 4. Function to fill in missing years with 0s
# 
# # 5. Function to filter observations to species list
# 
# #------------------------------
# # Main Processing Pipeline
# #------------------------------
# 
# 
# #------------------------------
# # Execution
# #------------------------------

