#------------------------------
# teamshrub_bowman_honours
# 01a_bbs_tidy_v2
# By: Elias Bowman 
# Created: 2024-11-17
# Updated: 2024-02-10
#
# Description: This script imports, tidies, and summarizes the Breeding Bird Survey data for the Qikiqtaruk - Herschel Island site. It associates guilds to species and creates scaled and non-scaled relative and logistic abundance values.
# Caution using current scaled totals - over inflate low effort years - consider using something like a power law to reduce the impact of having few observers and low sampling time
#------------------------------
# Loading Packages
library(tidyverse)  # For dplyr, tidyr, etc.
library(rlang)      # For tidy evaluation tools
library(dplyr)
library(tidyr)
library(skimr)
library(lubridate)
library(hms)
library(stringi)

# Declaring and Importing Relevant Data ####
#bbs.data.path <- "D:/BBS_QHI_2024/QHI_BBS_survey_data_1990_2024.csv"
bbs.data.path <- "data/raw/breeding_bird_survey_QHI_2024/QHI_BBS_survey_data_1990_2024.csv"

#guild.mapping.path <- "D:/bird_guild_mapping.csv"
guild.mapping.path <- "data/raw/bird_guild_mapping.csv"

# hard coded species list, as vector
species.list <- data.frame(
  species = c(
    "Common Eider",
    "Semipalmated Plover",
    "Semipalmated Sandpiper",
    "Baird's Sandpiper",
    "Red-necked Phalarope",
    "Glaucous Gull",
    "Lapland Longspur",
    "Snow Bunting",
    "Savannah Sparrow",
    "Common Redpoll",
    "Hoary Redpoll"
  ),
  spec.code = c(
    "COEI",
    "SEPL",
    "SESA",
    "BASA",
    "RNPL",
    "GLGU",
    "LALO",
    "SNBU",
    "SAVS",
    "CORE",
    "HORE"
  )
)
# importing species list, as dataframe
species.list <- read.csv("data/clean/bbs/species_list.csv")

# Expanded species list
species.list.exp <- data.frame(
  species = c(
    "Common Eider",
    "Semipalmated Plover",
    "Semipalmated Sandpiper",
    "Baird's Sandpiper",
    "Red-necked Phalarope",
    "Glaucous Gull",
    "Lapland Longspur",
    "Snow Bunting",
    "Savannah Sparrow",
    "Common Redpoll",
    "Hoary Redpoll",
    "Greater White-fronted Goose",
    "Northern Pintail",
    "Red-throated Loon",
    "Long-tailed Jaeger",
    "Black Guillemot"
  ),
  spec.code = c(
    "COEI",
    "SEPL",
    "SESA",
    "BASA",
    "RNPL",
    "GLGU",
    "LALO",
    "SNBU",
    "SAVS",
    "CORE",
    "HORE",
    "GWFG",
    "NOPI",
    "RTLO",
    "LTJA",
    "BLGU"
  )
)

bbs.survey <- read.csv(bbs.data.path)
guild.mapping <- read.csv(guild.mapping.path)


test <- guild.mapping %>%
  filter(species %in% species.list.exp$species)

# logistic high vs low relative abundance threshold
logistic.threshold <- 0.5


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

special_cases <- function(df){
  # in 1992 the late survey happened over two days, i want to manually rewrite the first part of the transect (transect 1), to have the same date as transect 2 and 3l and add a note that i did this
  df <- df %>%
    mutate(date = case_when(date == "29-Jun-92" ~ "28-Jun-92",
                            TRUE ~ date),
           notes = case_when(date == "29-Jun-92" ~ "Date changed from 29-June-92 to 28-Jun-92 match other transects",
                             TRUE ~ notes))
  
  
}

# Main function to clean the dataset
clean_bbs_data <- function(df) {
  df %>%
    rename_columns_lowercase %>%           # Rename columns to lowercase
    convert_column_names_to_dot() %>%      # Convert column names to dot notation
    capitalize_species_code() %>%          # Capitalize species codes
    add_observation_id() %>%               # Add observation ID and handle NA totals
  
    special_cases() %>%                    # Handle special cases
      
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
          max(as.duration(c(end.time, time)), na.rm = TRUE)
        ),
        any(!is.na(time)) ~ suppressWarnings(
          max(as.duration(time), na.rm = TRUE)
        ),
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    mutate(latest.end = ifelse(is.na(latest.end), NA, hms::as_hms(latest.end)))  # Convert to hms or keep NA
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
  
  if ("guild" %in% colnames(data) & "guild2" %in% colnames(data)) {
    data <- data %>%
      select(-guild, -guild2)  # Remove exisiting 'guild1' and 'guild2'
  }  
  
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
convert_to_long <- function(data) {
  data %>%
    mutate(original.total = total) %>%  # Add the original total as a new column
    uncount(total)  # Uncount the rows based on the 'total' column
}

# 6. Function to summarize to year level ####
# Helper function: join by year, period, and species
summarize_by_year_period_spec <- function(data) {
  summarized_data <- data %>%
    group_by(spec.code, year, period) %>%
    summarise(
      total.count = n(),  # Count the total number of observations
      # Summarize metadata using the first value for each group
      species = first(species),
      guild = first(guild),
      guild2 = first(guild2),
      survey.id = first(survey.id),
      observers = first(observers),
      sampling.time = first(sampling.time),
      sampling.effort = first(sampling.effort),
      effort.multiplier = first(effort.multiplier),
      num.observers = first(num.observers),
      original.total = first(original.total),
      start.time = first(start.time),
      end.time = first(end.time),
      time = first(time),
      date.ymd = first(date.ymd),
      doy = first(doy),
      day = first(day),
      month = first(month),
      year = first(year),
      notes = first(notes),
      .groups = 'drop'  # Ungroup after summarizing
    )
  
  return(summarized_data)
}

# Helper function: Select the row with the largest total.count per species and year
select_larger_count <- function(data) {
  data_with_max <- data %>%
    group_by(spec.code, year) %>%
    slice_max(total.count, n = 1) %>%  # Select the row with the maximum total_count
    ungroup()  # Ungroup after selection
  
  return(data_with_max)
}

summarize_to_year <- function(data) {
  summarized_data <- data %>%
    summarize_by_year_period_spec() %>%
    select_larger_count()
  
  return(summarized_data)
}

# 7. Function to fill in missing years with 0s #### 
  expand_and_fill <- function(df) {
  # List of metadata columns
  metadata_cols <- names(df)[!(names(df) %in% c("year", "spec.code", "total.count"))]
  
  # Generate all combinations of year and spec.code
  expanded_df <- df %>%
    expand(year, spec.code) %>%
    left_join(df, by = c("year", "spec.code")) %>%
    mutate(
      total.count = replace_na(total.count, 0),
      across(all_of(metadata_cols), ~ replace_na(., NA))
    )
  
  return(expanded_df)
}

# 7. Function and helpers to create scaled totals and relative abundances ####
create_abundances <- function(data, threshold) {
  data <- data %>%
    create_scaled_total() %>%
    create_relative_and_logistic_abundance(., threshold)
  
  return(data)
}
  
create_scaled_total <- function(data) {
  data <- data %>%
    mutate(total.scaled = ifelse(!is.na(effort.multiplier), total.count * effort.multiplier, 0))
  
  return(data)
}

create_relative_and_logistic_abundance <- function(data, threshold) {
  # Add calculated columns
  data <- data %>%
    group_by(spec.code) %>%  # Group by species
    mutate(
      # Relative abundances
      rel.abundance.total = total.count / max(total.count, na.rm = TRUE),
      rel.abundance.scaled = total.scaled / max(total.scaled, na.rm = TRUE),
      
      # Logistic IDs based on relative abundances
      logistic.id.total = factor(
        if_else(rel.abundance.total > threshold, "high", "low"),
        levels = c("low", "high")),
        logistic.id.scaled = factor(
          if_else(rel.abundance.scaled > threshold, "high", "low"),
          levels = c("low", "high"))
    ) %>%
    ungroup()
  
  return(data)
}

# 8. Function to reorder columns ####
reorder_columns <- function(data) {
  data %>%
    select(year, spec.code, species, total.count, rel.abundance.total, total.scaled, logistic.id.total, rel.abundance.scaled, logistic.id.scaled, guild, guild2, observers, sampling.effort, effort.multiplier, sampling.time, num.observers, notes, survey.id, period, start.time, end.time, time, date.ymd, doy, day, month)
}

# 9.Function to filter according to species list ####
filter_bbs <- function(data) {
  data %>%
    #filter(spec.code %in% species.list$spec.code)
    filter(spec.code %in% species.list.exp$spec.code)
}


#------------------------------
# Main Processing Pipeline
#------------------------------

bbs.survey.long <- bbs.survey %>% # Creating a tidy long version of the data
  clean_bbs_data() %>%
  clean_times() %>%
  apply_guilds(guild.mapping) %>%
  calculate_sampling_metrics() %>%
  convert_to_long()

bbs.summary <- bbs.survey.long %>% # Summarizing to the year level
  summarize_to_year() %>%
  expand_and_fill() %>%
  apply_guilds(guild.mapping) %>%
  create_abundances(threshold = logistic.threshold) %>%
  reorder_columns()

bbs.clean <- bbs.summary %>% # Filtering the data to only include species in the species list as specified by Cameron Eckert
  filter_bbs()

# Saving bbs.clean
output_path <- "data/clean/sem/" # Output path
write_csv(bbs.clean, paste0(output_path, "bbs_data.csv"))

# Removing unneeded objects to keep clean working directory
rm(bbs.data.path, guild.mapping.path)
rm(bbs.survey, bbs.survey.long)
rm(add_observation_id, adjust_survey_times, apply_guilds, calculate_doy, calculate_sampling_metrics, capitalize_species_code, clean_bbs_data, clean_times, combine_notes, convert_column_names_to_dot, convert_to_long, correct_species_mapping, create_abundances, create_date_string, create_date_time_column, create_indexing_columns, create_relative_and_logistic_abundance, create_scaled_total, expand_and_fill, fill_survey_times, filter_bbs, find_earliest_start_time, find_latest_end_time, format_date_column, recode_period_labels, rename_columns_lowercase, reorder_columns, select_larger_count, special_cases, standardize_and_parse_times, standardize_time, summarize_by_year_period_spec, summarize_to_year)
