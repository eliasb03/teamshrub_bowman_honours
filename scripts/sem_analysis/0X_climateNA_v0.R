#------------------------------
# teamshrub_bowman_honours
# 0X_climateNA_v0
# By: Elias Bowman 
# 2024-07-03
# Description: This script will implement the use of the ClimateNA package and tool to import relevant grid based climate data at the local level. Depending on use I will either have hyperspecific points to each data source or I will have kind of a generalized regional temp.
# 
# At the moment this is very early and preliminary 
#####
# Mission is to use and implement climate NA to download time sereis climate data from Qikiqtaruk
# clean and make this data workable
# then organize the data and order years from warmest to coldest june temperatures
# make a list of years within the dataset form warmest to coldest
# this is a proxy for earliest and latest springs
#
# 
#------------------------------

# importing packages
library(tidyverse)
library(dplyr)
library(ClimateNAr) # this is a downloaded package

climate_data <- read.csv("data/raw/qhi_june_temp_handmade.csv")


### Several Attempts to use ClimateNAr package exist below, none of them seem to work ####
#######
run_climateNA <- function(lat, lon, elev, year_range, MSY = "M", delay = 0.6) {
  # Initialize an empty list to store results
  all_results <- list()
  
  # Loop through each year in the specified range
  for (year in year_range) {
    # Run the ClimateNA_API function for the given year
    result <- ClimateNA_API(
      ClimateBC_NA = "NA",
      c(lat, lon, elev),
      period = as.character(year),
      MSY = MSY
    )
    
    # Add the year column to the result
    result$year <- year
    
    # Append the result to the list
    all_results[[as.character(year)]] <- result
    
    # Pause to prevent overloading the server (delay in seconds)
    Sys.sleep(delay)
  }
  
  # Combine all results into a single data frame
  final_dataset <- do.call(rbind, all_results)
  
  # Return the final dataset
  return(final_dataset)
}

# Specify latitude, longitude, and elevation
latitude <- 69.5723
longitude <- -138.9064
elevation <- 2

# Specify year range
year_range <- 1961:2024

# Run the function with a delay of 0.5 seconds between requests
climate_data <- run_climateNA(latitude, longitude, elevation, year_range, delay = 0.6)

# View the resulting dataset
head(climate_data)

#######

result1 <- ClimateNA_API(
  ClimateBC_NA = "NA",
  c(latitude, longitude, elevation),
  period = as.character(2020),
  MSY = "M"
)
result2 <- ClimateNA_API(
  ClimateBC_NA = "NA",
  c(latitude, longitude, elevation),
  period = as.character(1965),
  MSY = "M"
)

latLonEl <- c(48.98,-115.02,1000)
clm <- ClimateNA_API(ClimateBC_NA='NA', latLonEl=latLonEl,period='Normal_1961_1990.nrm',MSY='Y');

### Creating an input data frame ####
# setting csv location
climate_data_file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw"

input_climateNA_data <- data.frame(
  ID1 = 1,          # Identifier 1 (can be any number) these are meaningless in my context but needed for cliamteNAr
  ID2 = 1,          # Identifier 2 (can be any number)
  lat = 69.5723,    # Latitude midway along beach
  lon = -138.9064,  # Longitude midway along beach
  elevation = 2     # Estimated elevation of beach
)

input_file_path <- (file.path(climate_data_file_path, "climateNA_input.csv"))
output_file_path <- (file.path(climate_data_file_path, "/"))#, "climateNA_output.csv"))
  
# Save to CSV
write.csv(input_climateNA_data, input_file_path, row.names = FALSE)

#
# Specify file paths and
inputFile <- input_file_path
outDir <- output_file_path

# Specify parameter
varList <- c("Tave06", "MAT")                # June average temperature
periodList <- "Normal_1961_1990.nrm"#1961:2023               # Years of interest

# Run climateNAr
climateNAr(inputFile, periodList, varList, outDir)

inputFile
periodList
varList
outDir
climateNAr(inputFile, periodList, varList, outDir)

result <- climateNAr(input_climateNA_data, periodList, varList, outDir)

# Check the result
print(result)

install.packages("climateNAr")
library(ClimateNAr)

test <- ClimateNA_API(
  ClimateBC_NA = "NA",
  c(69.5723, -138.9064, 2),
  period = "2020",
  MSY = "M"
)



