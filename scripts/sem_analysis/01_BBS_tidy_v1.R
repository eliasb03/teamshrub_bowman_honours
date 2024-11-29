#------------------------------
# teamshrub_bowman_honours
# 01_BBS_tidy_v1
# By: Elias Bowman 
# Created: 2024-09-19
# Last updated: 2024-11-17
#
# Description: This script will import and tidy the Ranger collected Breeding Bird Data 
# from Qikiqtaruk - Herschel Island Territorial Park.
#
#------------------------------

# Importing Packages ####
library(dplyr)
library(tidyverse)
library(skimr)
library(lubridate)
library(ggplot2)
library(hms)
library(stringi)

# Importing Data
bbs.data.path <- "D:/BBS_QHI_2024/QHI_BBS_survey_data_1990_2024.csv"
bbs.survey <- read.csv(bbs.data.path)

guild.mapping <- read.csv("data/raw/bird_guild_mapping.csv")

# bbs.weather.path <- "D:/BBS_QHI_2024/QHI_BBS_weather_data_1990_2024.csv"
# bbs.weather <- read.csv(bbs.weather.path)

# Function to add observation ID and handle NA totals
add_observation_id <- function(df) {
  df %>%
    mutate(
      observation.id = row_number(),
      total_changed = ifelse(is.na(total) | total == 0, TRUE, FALSE),
      total = ifelse(is.na(total) | total == 0, 1, total)
    )
}

# Function to format the date column
format_date_column <- function(df) {
  df %>%
    mutate(date_ymd = paste(day, month, year, sep = "-") %>%
             trimws() %>%
             sub("^(\\d{1})-", "0\\1-", .)) %>%
    mutate(date_ymd = dmy(date_ymd))
}

# Function to combine notes columns
combine_notes <- function(df) {
  df %>%
    mutate(
      notes_1 = case_when(
        trimws(notes) != "" & trimws(x) != "" ~ paste(notes, x, sep = ", "),
        trimws(notes) != "" ~ notes,
        trimws(x) != "" ~ x,
        TRUE ~ ""
      )
    ) %>%
    select(-notes, -x) %>%
    rename(notes = notes_1)
}

# Function to create indexing columns
create_indexing_columns <- function(df) {
  df %>%
    mutate(
      transect.id = paste(date, period, survey_num, transect, sep = "_"),
      survey.id = paste(date, period, survey_num, sep = "_")
    )
}

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

# Function to standarize and parse times, applies standardize_time
standardize_and_parse_times <- function(df) {
  df %>%
    mutate(
      time = sapply(time, standardize_time),
      start.time = sapply(start.time, standardize_time),
      end.time = sapply(end.time, standardize_time)
    ) %>%
    mutate(
      start.time = hms::parse_hm(start.time),
      end.time = hms::parse_hm(end.time),
      time = hms::parse_hm(time)
    )
}

# Function to create a date-time column
create_date_time_column <- function(df) {
  df %>%
    mutate(date.time = as.POSIXct(paste(date_ymd, time), format = "%Y-%m-%d %H:%M:%S"))
}

# Function to fill missing start and end times within groups
fill_missing_times <- function(df) {
  df %>%
    group_by(transect.id) %>%
    fill(start.time, .direction = "downup") %>%
    fill(end.time, .direction = "downup") %>%
    ungroup()
}

# Function to recode period labels, remove "MID" observations, and convert PERIOD to a factor
recode_period_labels <- function(df) {
  df %>%
    mutate(period = as.character(period)) %>%
    mutate(period = ifelse(period %in% c("FALL", "LATE-1"), "LATE", period)) %>%
    filter(period != "MID") %>%
    mutate(
      period = factor(period, levels = c("EARLY", "LATE")),  # Set factor levels as desired
      doy = yday(date_ymd)
    )
}

# Function to rename all column names to lowercase
rename_columns_lowercase <- function(df) {
  df %>%
    rename_with(tolower)
}

# Function to calculate sampling time, effort, and effort multiplier
calculate_sampling_metrics <- function(data) {
  data <- data %>%
    mutate(
      sampling.time = as.numeric(difftime(end.time, start.time, units = "mins")),
      num_observers = sapply(strsplit(observers, ","), length),
      sampling_effort = sampling.time * num_observers
    )
  
  # Get the min and max sampling effort values
  min_effort <- min(data$sampling_effort, na.rm = TRUE)
  max_effort <- max(data$sampling_effort, na.rm = TRUE)
  
  # Dynamically calculate the max multiplier as the ratio of max to min effort
  if (min_effort == max_effort) {
    data <- data %>%
      mutate(effort_multiplier = 1)
  } else {
    max_multiplier <- max_effort / min_effort
    
    # Scale the effort multiplier to range between 1 and max_multiplier
    data <- data %>%
      mutate(effort_multiplier = 1 + (max_multiplier - 1) * (max_effort - sampling_effort) / (max_effort - min_effort))
  }
  
  return(data)
}

# Function to clean and fill missing or short sampling times
clean_sampling_times <- function(data) {
  # Step 1: Select rows with less than 5 mins of sampling time or NA Start/End times
  data_temp <- data %>%
    filter((end.time - start.time) < 5 | is.na(start.time) | is.na(end.time)) %>%
    group_by(transect.id) %>%
    mutate(
      # Fill Start.time with min(TIME) if NA and if valid TIME values exist
      start.time = ifelse(
        is.na(start.time),
        ifelse(any(!is.na(time)), hms::as_hms(min(time, na.rm = TRUE)), NA),
        start.time
      ),
      
      # Fill End.time with max(TIME) if NA and if valid TIME values exist
      end.time = ifelse(
        is.na(end.time),
        ifelse(any(!is.na(time)), hms::as_hms(max(time, na.rm = TRUE)), NA),
        end.time
      ),
      
      # If Start.time is filled but End.time is still NA, set End.time to Start.time if there's only one unique TIME
      end.time = ifelse(
        is.na(end.time) & !is.na(start.time) & n_distinct(na.omit(time)) == 1,
        start.time,
        end.time
      ),
      
      # Ensure End.time is adjusted if sampling time is less than 5 minutes
      end.time = ifelse(
        (end.time - start.time) < 5,
        ifelse(any(!is.na(time)), hms::as_hms(max(time, na.rm = TRUE)), end.time),
        end.time
      ),
      
      # Flag that End.time was filled in this step
      end.time.filled = TRUE
    ) %>%
    ungroup()
  
  # Step 2: Update the original dataset with corrected End.time values
  data <- data %>%
    mutate(end.time.filled = FALSE) %>%
    rows_update(data_temp, by = c("observation.id"))
  
  # Step 3: Summarize by survey.id to get the earliest Start.time and latest End.time for each survey
  data_summary <- data %>%
    group_by(survey.id) %>%
    summarize(
      start.time = hms::as_hms(min(start.time, na.rm = TRUE)),
      end.time = hms::as_hms(max(end.time, na.rm = TRUE)),
      .groups = 'drop'
    )
  
  # Step 4: Join the summarized Start.time and End.time back to the main data
  data <- data %>%
    select(-start.time, -end.time) %>%  # Remove old Start.time and End.time columns
    left_join(data_summary, by = "survey.id")  # Join the summarized Start.time and End.time
  
  return(data)
}

# Function to convert column names from snake_case to dot notation
convert_column_names_to_dot <- function(data) {
  colnames(data) <- gsub("_", ".", colnames(data))
  return(data)
}

# Function to capitalize species codes
capitalize_species_code <- function(data) {
  data %>% # Solves for an error where a common eider had species code "cOEI"
    mutate(spec.code = toupper(spec.code))
}

# Function to manually correct species mapping errors
correct_species_mapping <- function(data) {
  # # Run once to determine species with  incorrect mapping
  # species_mapping <- unique(bbs.survey %>% select(spec.code, species))
  # 
  # conflicts_species <- species_mapping %>%
  #   group_by(species) %>%
  #   summarise(n_codes = n_distinct(spec.code)) %>%
  #   filter(n_codes > 1)
  # 
  # conflicts_codes <- species_mapping %>%
  #   group_by(spec.code) %>%
  #   summarise(n_species = n_distinct(species)) %>%
  #   filter(n_species > 1)
  
  # Define manual updates
  manual_updates <- tibble::tribble(
    ~species, ~spec.code,
    "Greater White-fronted Goose", "GWFG",
    "Surf Scoter", "SUSC",
    "bird spp.", "BIRD",
    "Herring Gull", "HERG"
  )
  
  # Apply updates to the dataset
  data <- data %>%
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
  
  return(data)
}

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

# Function to reformat column order in the main dataset
reformat_column_order <- function(data) {
  data %>% 
    select(
      observation.id,
      survey.id,
      date.ymd,
      doy,
      species,
      spec.code,
      guild,
      total,
      time,
      sampling.time,
      num.observers,
      sampling.effort,
      effort.multiplier,
      breed,
      behaviour,
      notes,
      observers,
      guild2,
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
}

# Function to create long dataset
convert_to_long <- function(data) {
  data %>%
    mutate(original.total = total) %>%  # Add the original total as a new column
    uncount(total)  # Uncount the rows based on the 'total' column
}

# Summary Function ####
# Summary Function to join by year, period, and species
summarize_by_year_period_spec <- function(data) {
  summarized_data <- data %>%
    group_by(spec.code, year, period) %>%
    summarise(
      total.count = n(),  # Count the total number of observations
      # Summarize metadata using the first value for each group
      survey.id = first(survey.id),
      observers = first(observers),
      sampling.time = first(sampling.time),
      sampling.effort = first(sampling.effort),
      effort.multiplier = first(effort.multiplier),
      num.observers = first(num.observers),
      start.time = first(start.time),
      end.time = first(end.time),
      date.ymd = first(date.ymd),
      .groups = 'drop'  # Ungroup after summarizing
    )
  
  return(summarized_data)
}

# Summary Function to select the row with the largest total.count per species and year
select_larger_count <- function(data) {
  data_with_max <- data %>%
    group_by(spec.code, year) %>%
    slice_max(total.count, n = 1) %>%  # Select the row with the maximum total_count
    ungroup()  # Ungroup after selection
  
  return(data_with_max)
}

# Summary Function to reformat summary columns
reformat_summary <- function(data) {
  data %>% 
    select(
      spec.code,
      species,
      total.count,
      guild,
      guild2,
      year,
      date.ymd,
      observers,
      sampling.effort,
      effort.multiplier,
      survey.id,
      sampling.time,
      num.observers
    )
}

# Sumary Function to correct for the non breaking space issue in species
clean_non_breaking_spaces <- function(df, column) {
  df[[column]] <- stri_replace_all_fixed(df[[column]], "\u00A0", " ")
  df[[column]] <- enc2utf8(df[[column]]) # Ensure UTF-8 encoding
  return(df)
}


### Applying transformations to bbs.survey ####
bbs.survey <- bbs.survey %>%
  rename_columns_lowercase() %>%
  add_observation_id() %>%
  format_date_column() %>%
  combine_notes() %>%
  create_indexing_columns() %>%
  standardize_and_parse_times() %>%
  create_date_time_column() %>%
  fill_missing_times() %>%
  clean_sampling_times () %>%
  recode_period_labels() %>%
  calculate_sampling_metrics() %>%
  convert_column_names_to_dot() %>%
  capitalize_species_code() %>% 
  correct_species_mapping() %>%
  apply_guilds(guild.mapping) %>%
  reformat_column_order


# Creating long and summary bbs data ####
# Create long format version
bbs.long <- convert_to_long(bbs.survey)

# Create species ~ species.code mappings
species.mapping <- unique(bbs.survey %>% select(spec.code, species))

# Create list of species notable in the 2012 Monitoring Report
species.list <- c("Common Eider", "Semipalmated Plover", "Semipalmated Sandpiper", "Baird's Sandpiper", "Red-necked Phalarope", "Glaucous Gull", "Lapland Longspur", "Snow Bunting", "Savannah Sparrow", "Common Redpoll", "Hoary Redpoll") # list of species notable in the 2012 Monitoring Report

# Perform left join to get species codes for notable species
species.list <- left_join(tibble(species = species.list), # Convert species_list into a tibble with a proper character vector column
                          species.mapping, by = "species")

# Summarize to the year level
bbs.summary <- bbs.long %>%
  summarize_by_year_period_spec() %>%
  select_larger_count() %>%
  apply_guilds(guild.mapping) %>%
  reformat_summary() %>%
  clean_non_breaking_spaces("species")

# save bbs.summary, bbs.survey, bbs.long to the data/processed/bbs folder
write.csv(bbs.summary, "data/clean/bbs/bbs_yearly_summary.csv", row.names = FALSE)
write.csv(bbs.survey, "data/clean/bbs/bbs_survey_processed.csv", row.names = FALSE)
write.csv(bbs.long, "data/clean/bbs/bbs_long_processed.csv", row.names = FALSE)

# save species_list to the data/processed/bbs folder
write.csv(species.list, "data/clean/bbs/species_list.csv", row.names = FALSE)
write.csv(species.mapping, "data/clean/bbs/species_code_mapping.csv", row.names = FALSE)

