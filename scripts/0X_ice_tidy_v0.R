#------------------------------
# teamshrub_bowman_honours
# 0X_ice_tidy_v0
# By: Elias Bowman 
# Created: 2024-10-15
# Last update: 2024-10-15
# 
# Description: This script will import excel ice data from the Canadian Ice Service and tidy and prepare that data for processing.
#------------------------------

# File path to ice data
#file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_15Oct2024/cwa01_02_awtt_1968_2024_0514_1015.csv"
file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_15Oct2024/cwa01_02_stac_1968_2024_0514_1015.csv"
#file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_15Oct2024/cwa01_02_rmax_1968_2024_0514_1015.csv"
#file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_15Oct2024/cwa01_02_rmin_1968_2024_0514_1015.csv"


# Import packages
library(dplyr)
library(lubridate)
library(ggplot2)

# Function to import ice data
import_ice_data <- function(file_path) {
  # Read in the column names from the raw ice data file
  colnames <- readLines(file_path, n = 9)[9]
  
  # Process and format column names
  colnames <- colnames %>%
    gsub("<[^>]*>", "", .) %>%
    strsplit(split = ",") %>%
    unlist() %>%
    trimws() %>%
    tolower() %>%
    gsub(" ", "_", .) %>%
    gsub("-", "_", .) %>%
    gsub("[^a-z0-9_]", "", .) %>%
    gsub("ct", "conc", .)
  
  # Import data into the script, starting from the 10th line
  data <- read.csv(file_path, skip = 10, header = FALSE, col.names = colnames)
  
  # Find the index of the first occurrence of "legend" in column 1
  legend_index <- which(data[[1]] == "Legend: / ")
  
  # Remove rows after the first occurrence of "legend" (if it exists)
  if (length(legend_index) > 0) {
    data <- data[1:(legend_index[1] - 1), ]
  }
  
  # Conditional conversion of date columns to Date format
  if (colnames[1] == "season") {
    data$season <- as.numeric(substring(data[[1]], 1, 4))  # Extract year from "season"
  }
  if ((colnames[1] == "week")||(colnames[2] == "week")){
    data$week <- ymd(data$week)
    # Creation of new column containing just week date, independent of year
    data$mmdd <- format(data$week, "%m-%d")
    # Combine 0 year and mmdd to create a comparable weekly date column
    data$week_relative <- as.Date(paste("0-", data$mmdd, "%m-%d"), "%Y-%m-%d")  
    
  }
  
  
  return(data)
}

# Importing ice data
ice_data <- import_ice_data(file_path)

# test plotting ######
ggplot(ice_data, aes(x = season, y = conc, color = status)) +
  geom_line(color = "blue") +  # Line plot
  geom_point(size = 1) +  # Points on the line
  theme_minimal() +
  labs(
    x = "Date",  # X-axis label
    y = "Average Ice Coverage",  # Y-axis label
    title = "Average Concentration of Ice Cover over Time"  # Plot title
  )
 
# ggplot(ice_data, aes(x = season, y = week_relative)) +
#   geom_line(color = "blue") +  # Line plot
#   geom_point(size = 1, color = "red") +  # Points on the line
#   theme_minimal()
