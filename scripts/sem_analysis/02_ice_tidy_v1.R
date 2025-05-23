#------------------------------
# teamshrub_bowman_honours
# 02_ice_tidy_v1
# By: Elias Bowman 
# Created: 2024-10-15
# Last update: 2025-01-16
# 
# Description: This script imports ice data from the Canadian Ice Service and processes that data.
#------------------------------
# Import packages
library(dplyr)
library(lubridate)

# File path to ice data
file_path <- "data/raw/ice_cover/cwa01_02_awtt_1968_2024_0402_1126.csv"

# Define the ice cover threshold for drop date
ice_cover_threshold <- 0.35 #0.85 # 0.85 was old value


# Creating functions to process ice data
# Function: Clean column names
clean_column_names <- function(file_path) {
  colnames <- readLines(file_path, n = 9)[9] %>%
    gsub("<[^>]*>", "", .) %>%
    strsplit(split = ",") %>%
    unlist() %>%
    trimws() %>%
    tolower() %>%
    gsub(" ", "_", .) %>%
    gsub("-", "_", .) %>%
    gsub("[^a-z0-9_]", "", .) %>%
    gsub("ct", "conc", .)
  return(colnames)
}

# Function: Import ice data
import_ice_data <- function(file_path) {
  colnames <- clean_column_names(file_path)
  
  # Import data, skipping unnecessary lines
  data <- read.csv(file_path, skip = 10, header = FALSE, col.names = colnames)
  
  # Remove rows after "Legend"
  legend_index <- which(data[[1]] == "Legend: / ")
  if (length(legend_index) > 0) {
    data <- data[1:(legend_index[1] - 1), ]
  }
  
  # Process and add necessary date columns
  if (colnames[1] == "season") {
    data$season <- as.numeric(substring(data[[1]], 1, 4))  # Extract year
  }
  if ("week" %in% colnames) {
    data <- data %>%
      mutate(
        week = ymd(week),
        mmdd = format(week, "%m-%d"),
        week_relative = as.Date(paste("0-", mmdd, sep = ""), "%Y-%m-%d"),
        year = as.numeric(format(week, "%Y"))
      )
  }
  
  return(data)
}

# Function: Calculate yearly ice statistics
# Should find the latest date before the summer minimum when the ice concentration went below the threshold
calculate_yearly_stats <- function(data, threshold = ice_cover_threshold) {
  stats <- data %>%
    group_by(year) %>%
    summarise(
      min_conc = min(conc, na.rm = TRUE),
      min_conc_day = week[which.min(conc)],
      max_conc_prior = max(conc[week < min_conc_day], na.rm = TRUE),
      max_conc_day_prior = max(week[week < min_conc_day & conc == max_conc_prior], na.rm = TRUE),
      # I'm not sure this line actually makes sense right now
      #target_conc = min_conc + threshold * (max_conc_prior - min_conc),
      spring_drop_day = max(week[week < min_conc_day & conc >= ice_cover_threshold], na.rm = TRUE)
      #spring_drop_day = max(week[week < min_conc_day & conc >= target_conc], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      min_conc_doy = yday(min_conc_day),
      max_conc_prior_doy = yday(max_conc_day_prior),
      spring_drop_doy = yday(spring_drop_day)
    )
  return(stats)
}


# Running functions ####
ice_data <- import_ice_data(file_path)
ice_data_yearly <- calculate_yearly_stats(ice_data, ice_cover_threshold)

# Saving ice data
output_path <- "data/clean/sem/" # Output path
write_csv(ice_data_yearly, paste0(output_path, "ice_data.csv"))

library(cowplot)
yearly_lines <- data.frame(
  year_start = seq(from = as.Date("1960-01-01"), to = as.Date("2024-01-01"), by = "year")
)

ggplot(ice_data, aes(x = week, y = conc)) +
  geom_line(color = "blue", size = 1) +  # Line plot
  geom_vline(data = ice_data_yearly, aes(xintercept = as.numeric(spring_drop_day)), 
             color = "red", linetype = "dashed", size = 1) +  # Red vertical lines
  geom_vline(data = yearly_lines, aes(xintercept = as.numeric(year_start)), 
             color = "black",  size = 0.5) +
  scale_x_date(limits = c(as.Date("2020-02-01")-4000, as.Date("2024-05-01"))-4000) +  # Restrict x-axis limits
  theme_half_open(font_size = 14)

# Removing unneeded workspace objects
#rm(ice_data)
rm(file_path, ice_cover_threshold)
rm(calculate_yearly_stats, clean_column_names, import_ice_data)