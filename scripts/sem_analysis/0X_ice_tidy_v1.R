#------------------------------
# teamshrub_bowman_honours
# 0X_ice_tidy_v1
# By: Elias Bowman 
# Created: 2024-10-15
# Last update: 2024-11-16
# 
# Description: This script imports ice data from the Canadian Ice Service and processes that data.
#------------------------------
# Import packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

# File path to ice data
file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_16Nov2024/cwa01_02_awtt_1968_2024_0402_1126.csv"

# Define the ice cover threshold for drop date
ice_cover_threshold <- 0.85

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
calculate_yearly_stats <- function(data, threshold = ice_cover_threshold) {
  stats <- data %>%
    group_by(year) %>%
    summarise(
      min_conc = min(conc, na.rm = TRUE),
      min_conc_day = week[which.min(conc)],
      max_conc_prior = max(conc[week < min_conc_day], na.rm = TRUE),
      max_conc_day_prior = max(week[week < min_conc_day & conc == max_conc_prior], na.rm = TRUE),
      target_conc = min_conc + threshold * (max_conc_prior - min_conc),
      spring_drop_day = max(week[week < min_conc_day & conc >= target_conc], na.rm = TRUE)
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

##################################
# Various Plots ####
# Plotting ice drop date
ggplot(ice_data_yearly, aes(x = year, y = spring_drop_doy)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "darkblue") +
  labs(
    title = "Latest Target Day vs. Year",
    x = "Year",
    y = "Latest Target Day (DOY)"
  ) +
  theme_half_open(font_size = 14)

# Plotting yearly concentrations
year.of.interest <- 1968
ggplot(filter(ice_data, year == year.of.interest), aes(x = week, y = conc)) +
  geom_line(color = "#A2BFFE", size = 2) +  # Line plot
  theme_minimal() +
  labs(
    x = "Year",  # X-axis label
    y = "Average Ice Coverage",  # Y-axis label
    title = "Average Concentration of Ice Cover over Time"  # Plot title
  ) +
  theme_half_open(font_size = 14)


ggplot(ice_data, aes(x = year, y = conc)) +
  #geom_line(color = "#A2BFFE", size = 2) +  # Line plot
  geom_point(size = 2, colour = "lightblue") +  # Points on the line
  geom_smooth(method = lm) +
  theme_minimal() +
  labs(
    x = "Year",  # X-axis label
    y = "Average Ice Coverage",  # Y-axis label
    title = "Average Concentration of Ice Cover over Time"  # Plot title
  ) +
  theme_half_open(font_size = 14)
