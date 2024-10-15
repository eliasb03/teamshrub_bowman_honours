#------------------------------
# teamshrub_bowman_honours
# 0X_ice_tidy_v0
# By: Elias Bowman 
# Created: 2024-10-15
# Last update: 2024-10-15
# 
# Description: This script will import excel ice data from the Canadian Ice Service and tidy and prepare that data for processing.
#------------------------------

# Import Packages
library(dplyr)
library(lubridate)

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
  
  # Determine number of rows in the file
  total_rows <- length(readLines(file_path))
  
  # Import data into the script, excluding footer lines
  data <- read.csv(file_path, skip = 10, header = FALSE, col.names = colnames, nrows = total_rows - 18) # 18 seems to be the number of post-data legend lines there are
  
  data$week <- ymd(data$week)
  
  return(data)
}

# Example of how to use the function
file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_15Oct2024/cwa01_02_awtt_1968_2024_0514_1015.csv"
ice_data <- import_ice_data(file_path)
