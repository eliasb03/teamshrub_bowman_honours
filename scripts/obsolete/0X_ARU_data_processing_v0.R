#------------------------------
# teamshrub_bowman_honours
# 0X_ARU_data_processing_v0
# By: Elias Bowman 
# Created: 2024-11-18
#
# Description: This script will process the csv outputs from BirdNET runs on ARU recordings
# Should handle the full csv output from 02_ARU_pipeline
#------------------------------

# Load necessary libraries
library(dplyr)
library(data.table)
library(stringr)
#library(terra)
#library(sf)
#library(stringi)


# Define the path to the ARU data folder

base_dir <- "D:/ARU_QHI_2024"
aru_all_output_folder <- "D:/ARU_QHI_2024/output_total"


# Columns to remove (replace with actual column names you want to exclude)
columns_to_remove <- c("lat",	"lon",	"week",	"overlap",	"sensitivity", "species_list", "dateTimeRecorder", "dateTimeUTC", "timezone", "min_conf")

# Confidence threshold
confidence_threshold <- 0.5

# Remove 1.5 overlap
# Define the function to remove 1.5 overlap
remove_overlap <- function(dt) {
  dt[!(start %% 1 == 0.5)] # Keep rows where 'start' does not have a fractional part of 0.5
}

# Define a function to extract locationID from recordingID
extract_locationID <- function(dt) {
  dt[, locationID := sub("^(ARUQ\\d+)_.*", "\\1", recordingID)]
  return(dt)
}

# Remove faulty files
# toignore <- process_and_compile_toignore(base_dir)
#write.csv((toignore), paste0(base_dir, "/full_toignore.csv"))
toignore <- read_csv(paste0(base_dir, "/full_toignore.csv"))

# Define a function to process each file
process_aru_folder <- function(folder_path, cols_to_remove, confidence_threshold, toignore) {
  # Get a list of CSV files in the folder
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Process each CSV file
  lapply(csv_files, function(file_path) {
    # Read the CSV using fread
    dt <- fread(file_path)
    
    # Remove unwanted columns
    dt <- dt[, !cols_to_remove, with = FALSE]
    
    # Remove rows with 1.5 overlap
    dt <- remove_overlap(dt)
    
    # Filter rows based on the confidence value
    dt <- dt[confidence > confidence_threshold]
    
    # Remove rows with file paths in the `toignore` dataset
    dt <- dt[!file_path %in% toignore$File]
    
    # Extract locationID from recordingID
    dt <- extract_locationID(dt)
    
    return(dt)
  })
}

# Process all folders
processed_aru_data <- lapply(aru_all_output_folder, function(folder) {
  process_aru_folder(folder, columns_to_remove, confidence_threshold, toignore)
})


# ################
# # Read ARU formatted results files
# aruq3_records <- read.csv(
#   "D:/ARU_QHI_2024/ARUQ3_21Aug2024/Output/ARUQ3_21Aug2024.BirdNET_formatted_results.csv"
# )
# aruq5_records <- read.csv(
#   "D:/ARU_QHI_2024/ARUQ5_20Aug2024/Output/ARUQ5_20Aug2024.BirdNET_formatted_results.csv"
# )
# aruq6_records <- read.csv(
#   "D:/ARU_code_optimization_data/ARUQ6_17Aug2024/Output/ARUQ6_17Aug2024.BirdNET_formatted_results.csv"
# )

# Define the TOMST-to-ARU mapping
# tomst_to_aru <- data.frame(
#   tomst = c("TOMST29_QHI", "TOMST39_QHI", "TOMST4_QHI"), # TOMST IDs
#   aru = c("ARUQ3", "ARUQ5", "ARUQ6")                     # Corresponding ARU names
# )

# # THIS SHOULD 


tomst.mapping <- read.csv("data/raw/aru_tomst_mapping.csv")
guild.mapping <- read.csv("data/raw/bird_guild_mapping.csv")

month.tomst.data <- monthly_values %>%
  filter(sensor_name == "TMS_T3_mean") %>%
  filter(month %in% c("6", "7"))

tomst.mapping <- tomst.mapping %>%
  mutate(locality_id = paste0("TOMST", tomst_num, "_QHI")) %>%
  left_join(month.tomst.data, by = "locality_id") %>%
  filter(month == 7)

# clean_non_breaking_spaces <- function(df, column) {
#   df[[column]] <- stri_replace_all_fixed(df[[column]], "\u00A0", " ")
#   df[[column]] <- enc2utf8(df[[column]]) # Ensure UTF-8 encoding
#   return(df)
# }
# 
# # Define guild mapping
# apply_guilds_aru <- function(data) {
#   
#   guild.mapping <- guild.mapping %>%
#     clean_non_breaking_spaces("species") %>%
#     mutate(common_name = species) %>%
#     select(common_name, guild, guild2, species)
#   
#   data <- data %>%
#     left_join(guild.mapping, by = "common_name")
#   
#   return(data)
# }

# Combine records from all ARUs into a single dataset
# three_arus <- bind_rows(aruq3_records, aruq5_records, aruq6_records) %>%
#   # Extract ARU name (e.g., "ARUQ3") from the recordingID
#   mutate(aru_name = str_extract(recordingID, "ARUQ[0-9]+"))
# 
# three_arus_processed <- three_arus %>%
#   # Filter to retain only records from specific ARUs
#   filter(aru_name %in% c("ARUQ3", "ARUQ5", "ARUQ6")) %>%
#   
#   # Join with metadata from the aru_locations dataset
#   left_join(
#     aru_locations %>%
#       select(name, unit_name, grid_code, geometry),
#     by = c("aru_name" = "name")
#   ) %>%
#   
#   # Convert to an `sf` object (preserving the geometry column)
#   st_as_sf() %>%
#   
#   # Assign a location ID based on ARU name
#   mutate(
#     locationID = case_when(
#       aru_name == "ARUQ3" ~ "upland",
#       aru_name %in% c("ARUQ5", "ARUQ6") ~ "creek bed",
#       TRUE ~ locationID # Retain existing value (if any) for other cases
#     )
#   ) %>%
#   
#   # Filter out observations with low confidence scores
#   filter(confidence >= 0.5) %>%
#   # Join with TOMST-to-ARU mapping
#   # Add TOMST column to three_arus_processed
#   left_join(tomst.mapping, by = "aru_name") %>%
#   apply_guilds_aru() # apply guild mapping
# #  
# arus_summarized <- three_arus_processed %>%
#   filter(!is.na(common_name)) %>%
#   group_by(aru_name, date, common_name) %>%
#   summarise(
#     count = n(),
#     mean_confidence = mean(confidence),
#   ) %>%
#   apply_guilds_aru() %>%
#   left_join(tomst.mapping, by = "aru_name") %>%
#   mutate(tomst_num = tomst_num.x) %>%
#   select(-aru_name_other, aru_notes, tomst_notes, sensor_name, tomst_num.x, tomst_num.y)
#   
# arus_prob <- arus_summarized %>%
#   group_by(common_name) %>%
#   mutate(
#     detection_prob = count / sum(count) # Normalize detections to probabilities
#   )
# 
# library(DescTools)
# 
arus_prob <- arus_summarized %>%
  group_by(common_name, aru_name) %>%
  mutate(
    detection_prob = count / sum(count),
    count_winsorized = Winsorize(count, val = quantile(count, probs = c(0, 0.90))), #Cap at 5th and 90th percentiles
    detection_prob_win = count_winsorized / sum(count_winsorized)
  )
 
test <- arus_prob %>%
  filter(common_name == "Lapland Longspur") %>%
  filter(aru_name %in% c("ARUQ3"))

ggplot(test, aes(x = date, y = detection_prob_win, group = aru_name)) +
  #geom_line(aes(color = aru_name)) +
  geom_smooth(aes(color = aru_name)) +
  #geom_point(aes(color = aru_name)) +
  labs(
    title = "Adjusted Detection Probabilities by Species",
    x = "Date",
    y = "Detection Probability"
  ) +
  theme_half_open(font_size = 14)


# ####### Stretch based
# stretch_length <- 2
# 
# arus_stretch_prob <- arus_summarized %>%
#   mutate(
#     stretch = as.integer(as.numeric(as.Date(date)) / stretch_length) # Create stretch variable
#   ) %>%
#   group_by(common_name, aru_name) %>%
#   # Normalize and Winsorize within stretches
#   mutate(
#     detection_prob = count / sum(count), # Raw probabilities
#     count_winsorized = Winsorize(count, val = quantile(count, probs = c(0, 0.90))), # Winsorize counts
#     detection_prob_win = count_winsorized / sum(count_winsorized) # Adjusted probabilities
#   ) %>%
#   group_by(common_name, aru_name, stretch) %>%
#   # Summarize probabilities for each stretch
#   summarize(
#     stretch_start = min(as.Date(date)), # Start of the stretch
#     stretch_end = max(as.Date(date)), # End of the stretch
#     detection_prob_stretch = sum(detection_prob, na.rm = TRUE), # Sum of probabilities
#     detection_prob_stretch_win = sum(detection_prob_win, na.rm = TRUE), # Sum of winsorized probabilities
#     total_count = sum(count, na.rm = TRUE), # Total count
#     .groups = "drop"
#   )
# 
# test <- filter(arus_stretch_prob, common_name == "American Pipit")
# 
# ggplot(test, aes(x = stretch, y = detection_prob_stretch_win, group = aru_name)) +
#   #geom_line(aes(color = aru_name)) +
#   geom_smooth(aes(color = aru_name)) +
#   #geom_point(aes(color = aru_name)) +
#   labs(
#     title = "Adjusted Detection Probabilities by Species",
#     x = "Date",
#     y = "Detection Probability"
#   ) +
#   theme_minimal(base_size = 14)
# 
# ###### weekly
# arus_weekly_prob <- arus_summarized %>%
#   # Add a week column
#   mutate(week = floor_date(as.Date(date), unit = "week")) %>%
#   group_by(common_name, aru_name) %>%
#   # Normalize and Winsorize within weeks
#   mutate(
#     detection_prob = count / sum(count), # Normalize detections within the week
#     count_winsorized = Winsorize(count, val = quantile(count, probs = c(0, 0.90))), # Winsorize counts
#     detection_prob_win = count_winsorized / sum(count_winsorized) # Adjusted probabilities
#   ) %>%
#   group_by(common_name, aru_name, week) %>%
#   # Summarize probabilities for each week
#   summarize(
#     weekly_detection_prob = sum(detection_prob, na.rm = TRUE), # Sum of probabilities
#     weekly_detection_prob_win = sum(detection_prob_win, na.rm = TRUE), # Sum of winsorized probabilities
#     total_count = sum(count, na.rm = TRUE), # Total count for reference
#     .groups = "drop"
#   )
# 
# test <- filter(arus_weekly_prob, common_name == "American Pipit")
# 
# ggplot(test, aes(x = week, y = weekly_detection_prob, group = aru_name)) +
#   geom_line(aes(color = aru_name)) +
#   geom_smooth(aes(color = aru_name)) +
#   geom_point(aes(color = aru_name)) +
#   labs(
#     title = "Adjusted Detection Probabilities by Species",
#     x = "Date",
#     y = "Detection Probability"
#   ) +
#   theme_minimal(base_size = 14)

######################## brief dummy attempt at final script #######################
# # Load necessary libraries
# library(dplyr)
# library(stringr)
# library(purrr)
# 
# # Specify the folder containing ARU CSV files
# aru_folder <- "D:/ARU_QHI_2024/ARU_Files"
# 
# # Read all CSV files from the folder
# aru_files <- list.files(aru_folder, pattern = "\\.csv$", full.names = TRUE)
# 
# # Combine all ARU records into a single dataframe
# all_aru_records <- map_dfr(aru_files, read.csv)
# 
# # Extract unique ARU names from the dataset
# aru_names <- all_aru_records %>%
#   mutate(name = str_extract(recordingID, "ARUQ[0-9]+")) %>% 
#   distinct(name) %>%
#   pull(name)
# 
# # Display the detected ARU names (for reference)
# cat("Detected ARU names:", paste(aru_names, collapse = ", "), "\n")
# 
# # Filter for specific ARUs of interest (or process all if no filter is needed)
# selected_arus <- aru_names # Replace with specific ARU names, e.g., c("ARUQ3", "ARUQ5", "ARUQ6")
# 
# # Process combined dataset
# processed_aru_data <- all_aru_records %>%
#   # Extract ARU name from the recordingID
#   mutate(name = str_extract(recordingID, "ARUQ[0-9]+")) %>%
#   
#   # Filter to retain only the selected ARUs
#   filter(name %in% selected_arus) %>%
#   
#   # Join with metadata from the aru_locations dataset
#   left_join(
#     aru_locations %>%
#       select(name, unit_name, grid_code, geometry),
#     by = "name"
#   ) %>%
#   
#   # Assign a location ID based on ARU name (customize as needed)
#   mutate(
#     locationID = case_when(
#       name == "ARUQ3" ~ "upland",
#       name %in% c("ARUQ5", "ARUQ6") ~ "creek bed",
#       TRUE ~ "unknown" # Assign "unknown" for other ARUs (or customize further)
#     )
#   ) %>%
#   
#   # Filter out observations with low confidence scores
#   filter(confidence >= 0.5)
# 
# # Save the processed data (optional)
# write.csv(processed_aru_data, "processed_aru_data.csv", row.names = FALSE)
# 
# # View the first few rows of the processed data (optional)
# head(processed_aru_data)