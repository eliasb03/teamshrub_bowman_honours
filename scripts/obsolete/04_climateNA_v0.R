


library(tidyverse)
library(dplyr)
library(ClimateNAr) # this is a downloaded package


# Specify latitude, longitude, and elevation
latitude <- 69.5723
longitude <- -138.9064
elevation <- 2

# Create input dataframe
input_df <- data.frame(
  ID1 = c(1, 2),
  ID2 = c(1, 2),
  lat = c(latitude, 1),
  lon = c(longitude, 1),
  elev = c(elevation, 1)
) 

# Write dataframe to file
file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/climateNA/climateNA_input_test.csv"
write_csv(input_df, file_path)

output_dir <- file.path("C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/climateNA", "output//")

# Specify year range
year_range <- 1985:1990 #1961:2024

# Create list of year inputs
year_inputs <- paste0("Year_", year_range, ".ann")

# Create list of climate variables
clim_var_list <- c("Tave_sp", "Tmin_sp", "Tmax_sp", "Tave_sm", "Tmin_sm", "Tmax_sm", "Tave04", "Tave05", "Tave06", "Tave07", "Tave08") 

climateNAr(
  inputFile = file_path,
  periodList = year_inputs, #year_inputs,
  varList = clim_var_list,
  outDir = output_dir
)


process_climateNA_files <- function(directory) {
  # List all CSV files in the directory
  file_list <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize an empty list to store dataframes
  data_list <- list()
  
  # Loop through each file
  for (file in file_list) {
    # Extract the year from the file name (assuming it's a 4-digit number)
    year <- str_extract(basename(file), "\\d{4}")
    
    # Read the CSV file
    file_data <- read_csv(file, col_names = TRUE, skip = 0, show_col_types = FALSE)
    
    # Select the second row and convert to a dataframe
    value_row <- file_data[1, ] %>%
      as_tibble() %>%
      mutate(Year = as.numeric(year)) # Add the year column
    
    # Append to the list
    data_list[[file]] <- value_row
  }
  
  # Combine all rows into a single dataframe
  final_data <- bind_rows(data_list)
  
  final_data <- final_data %>%
    select(-ID1, -ID2) %>% # Remove the ID columns
    select(Year, everything()) # Move the Year column to the front
  
  return(final_data)
}

directory <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/climateNA/output/"

climate_data <- process_climateNA_files(directory)


