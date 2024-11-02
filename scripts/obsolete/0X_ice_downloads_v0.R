#------------------------------
# teamshrub_bowman_honours
# 0X_ice_downloads_v0
# By: Elias Bowman 
# Created: 2024-09-27
# 
# Description: This script will do initial donwloads and processing of satellite data for a remote sensing analysis on sea ice.
#------------------------------

#devtools::install_github("atlanhq/rLandsat")
install.packages("getSpatialData")
devtools::install_github("16EAGLE/getspatialdata")
#install.packages('luna', repos='https://rspatial.r-universe.dev')

library("rLandsat")
library("landsat")
library("getSpatialData") # this one looks promising but is in beta
library("raster")
library("sf")
library("tidyverse")
library("rasterVis")
library("lubridate")
library("keyring")

# Retrieve USGS credentials from keyring
username <- keyring::key_get("USGS")
password <- keyring::key_get("USGS", username)

key_get("USGS")

rLandsat::setUSGSLogin(username, password)

# Example: Search Landsat 8 data for a specific date range and region
result <- landsat_search(
  min_date = "2004-01-01", 
  max_date = "2024-12-31", 
  country = "Canada"
)

# bounding box of the QHI region
  # this will need to be redone and thought about more thoroughly 
bounding_box <- c(xmin = -143, ymin = 68.5, xmax = -133, ymax = 70.5)

# create area of interest as polygon
qhi_ice_region <- st_as_sfc(st_bbox(bounding_box))


download_landsat_data <- function(region, start_year = 2004, end_year = 2024) {
  # Create a date range
  date_range <- seq(ymd(paste0(start_year, "-01-01")), ymd(paste0(end_year, "-12-31")), by = "years")
  
  # Loop through each year in the date range
  for (date in date_range) {
    year <- year(date)
    
    # Use the landsat package to search for and download data for the specified region and year
    # You can adjust parameters based on your needs (e.g., cloud cover)
    data <- landsatDownload(
      lon_min = st_bbox(region)["xmin"], 
      lon_max = st_bbox(region)["xmax"],
      lat_min = st_bbox(region)["ymin"], 
      lat_max = st_bbox(region)["ymax"],
      startDate = paste0(year, "-01-01"),
      endDate = paste0(year, "-12-31"),
      path = "path/to/save/files"
    )
    
    # Load the downloaded data as a raster
    landsat_raster <- raster(data$B4)  # Example: Band 4 for Red band
    
    # Crop the raster to the bounding box region
    cropped_raster <- crop(landsat_raster, region)
    
    # Plot the cropped raster
    plot(cropped_raster, main = paste("LandSAT Band 4 for", year))
  }
}

# Example: Run the function with your region and date range
download_landsat_data(qhi_ice_region, start_year = 2004, end_year = 2024)
