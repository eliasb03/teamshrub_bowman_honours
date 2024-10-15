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
  
  # I
# View the imported data
head(ice_data)
mport data into the script
  data <- read.csv(file_path, skip = 10, header = FALSE, col.names = colnames)
  
  return(data)
}

# Example of how to use the function
file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_15Oct2024/cwa01_02_awtt_1968_2024_0514_1015.csv"
ice_data <- import_ice_data(file_path)
