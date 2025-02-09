#----------------------------------------------------------------
# teamshrub_bowman_honours
# Script: 0X_ebird_tidy_v0
# Author: Elias Bowman
# Created: 2024-07-03
# Updated: 2024-11-02
# Description: This script imports a large eBird dataset for the whole Yukon,
# cleans it to retain relevant variables, and filters observations
# near Qikiqtaruk - Herschel Island (QHI) for analysis.
#----------------------------------------------------------------

#------------------------------
# Load Required Packages
#------------------------------
library(tidyverse)    # Data manipulation
library(readr)        # Data reading functions
library(ggplot2)      # Plotting
library(skimr)        # Data summary
library(terra)        # Spatial data handling
library(leaflet)      # Interactive mapping
library(maptiles)     # Download and Display Maps


#------------------------------
# Set Script-Wide Variables
#------------------------------
# File paths
ebird_file_path <- file.path("data/raw/ebd_CA-YT_unv_smp_relMay-2024/ebd_CA-YT_unv_smp_relMay-2024.txt")
qhi_outline_path <- "data/clean/shape/qhi_region.shp"

# Coordinate Reference System (CRS)
ebird_coordinate_system <- "+proj=longlat +datum=WGS84"

#------------------------------
# Define Functions
#------------------------------

# Function to load and project the QHI outline shapefile
load_and_project_qhi_outline <- function(shapefile_path, crs) {
  qhi_outline <- vect(shapefile_path)
  qhi_outline <- project(qhi_outline, crs, partial = FALSE)
  return(qhi_outline)
}

# Function to read eBird data
read_ebird_data <- function(file_path) {
  read.table(file_path, header = TRUE, sep = "\t", quote = "", fill = TRUE, stringsAsFactors = FALSE)
}

# Function to filter and clean eBird data
process_ebird_data <- function(data) {
  columns_to_keep <- c(
    "OBSERVATION.DATE",
    "COMMON.NAME",
    "SCIENTIFIC.NAME",
    "OBSERVATION.COUNT",
    "COUNTY",
    "LOCALITY",
    "BREEDING.CODE",
    "BREEDING.CATEGORY",
    "BEHAVIOR.CODE",
    "AGE.SEX",
    "YEAR",
    "LATITUDE",
    "LONGITUDE",
    "TRIP.COMMENTS",
    "OBSERVER.ID",
    "SPECIES.COMMENTS",
    "GROUP.IDENTIFIER",
    "LOCALITY.ID",
    "REVIEWED",
    "APPROVED",
    "DURATION.MINUTES",
    "NUMBER.OBSERVERS",
    "SAMPLING.EVENT.IDENTIFIER",
    "TAXONOMIC.ORDER",
    "TAXON.CONCEPT.ID",
    "PROTOCOL.TYPE",
    "CATEGORY",
    "GLOBAL.UNIQUE.IDENTIFIER",
    "IBA.CODE",
    "BCR.CODE",
    "USFWS.CODE",
    "EFFORT.DISTANCE.KM",
    "EFFORT.AREA.HA"
  )
  
  data %>%
    filter(LATITUDE > 69) %>%  # Only northern observations
    mutate(
      OBSERVATION.DATE = as.Date(OBSERVATION.DATE, format = "%Y-%m-%d"),
      YEAR = as.integer(format(OBSERVATION.DATE, "%Y")),
      OBSERVATION.COUNT = as.double(OBSERVATION.COUNT)  # Convert observation.count to double
    ) %>%
    select(all_of(columns_to_keep)) %>%
    rename_all(tolower)
}

# Function to filter eBird data by QHI outline
filter_ebird_by_qhi <- function(ebird_data, qhi_outline, crs) {
  ebird_vect <- vect(ebird_data, geom = c("longitude", "latitude"), crs = crs)
  qhi_ebird_vect <- intersect(ebird_vect, qhi_outline)
  
  # Return both the spatial object and the data frame
  list(
    spatial_data = qhi_ebird_vect,
    data_frame = as.data.frame(qhi_ebird_vect)
  )
}

#------------------------------
# Load and Process Data
#------------------------------
# Load and project the Qikiqtaruk Island outline shapefile
qhi_outline <- load_and_project_qhi_outline(qhi_outline_path, ebird_coordinate_system)

# Load and process eBird data
ebird_full_data <- read_ebird_data(ebird_file_path)
ebird_processed <- process_ebird_data(ebird_full_data)

# Filter eBird data by QHI outline and keep spatial version
filtered_ebird <- filter_ebird_by_qhi(ebird_processed, qhi_outline, ebird_coordinate_system)

# Extract the spatial and data frame versions
qhi_ebird_spatial <- filtered_ebird$spatial_data
qhi_ebird_df <- filtered_ebird$data_frame

#------------------------------
# Visualization
#------------------------------
# Set map background
bg <- get_tiles(ext(qhi_ebird_spatial))

# Plot the background and points on the map
plotRGB(bg)
points(qhi_ebird_spatial, col = "yellow")

# Optionally, create a summary of observations in QHI
observation_summary <- qhi_ebird_df %>%
  group_by(common.name) %>%
  summarise(count = n(), .groups = 'drop')

# Display summary
print(observation_summary)

#------------------------------
# Outputs:
# qhi_ebird_spatial: Spatial object of eBird observations within QHI
# qhi_ebird_df: Data frame of eBird observations within QHI
# observation_summary: Summary table of observations by common name
#------------------------------

# Removing temp variables
rm(bg, ebird_full_data, filtered_ebird, observation_summary, ebird_processed)
rm(ebird_file_path, qhi_outline_path)
rm(filter_ebird_by_qhi, load_and_project_qhi_outline, process_ebird_data, read_ebird_data)
