#------------------------------
# teamshrub_bowman_honours
# 0X_qhi_map_v0
# By: Elias Bowman 
# 2024-07-03
# Description: This R script should create a rough map of Qikiqtaruk - Herschel Island (QHI). It will import relevant data and create several files. A raster or vector basemap of QHI and the north slope, a higher resolution similar map of QHI, and a vector outline of just QHI, and just the north slopes, are ideal outputs of this script
# 
# 
#------------------------------

# importing packages
library(tidyverse)
library(terra)
library(sp)
library(sf)
library(dplyr)

# setting script variables
buffer_size = 25000 # how wide the qhi buffer region is
coordinate_system <- "+proj=longlat +datum=WGS84 +no_defs" # setting project coordinate system
shape_file <- "data/shape/yukon_borders_surveyed/Yukon_Borders_-_Surveyed.shp"

# importing and creating script data
# yukon outline data
# Source: https://hub.arcgis.com/datasets/2a65ccced3a2421783eeddae32b7d58b_2/explore?location=63.730317%2C-131.505522%2C4.52
yukon <- st_read(shape_file) %>%
  st_transform(crs = coordinate_system) # setting project coordinate system

# setting qhi polygon bounds
qhi_coords <- list(matrix(c(
  -139.101, 69.504,  
  -139.330, 69.546,  
  -139.280, 69.628,  
  -139.013, 69.659,  
  -138.799, 69.573,
  -139.101, 69.504
), ncol = 2, byrow = TRUE))

# creating polygon of qhi bounds
qhi_bounds <- qhi_coords %>%
  st_polygon() %>%
  st_geometry() %>%
  st_set_crs(coordinate_system) # setting coordinate system# setting coordinate system

qhi_coast <- st_intersection(yukon, qhi_bounds) %>% # calculating intersection, this returns the qhi map instead of the pentagon
  st_union() %>%
  st_polygonize() %>%
  st_transform(crs = coordinate_system)

# creating cropped area of just the yukon north slope
north_coast <- yukon %>%
  st_crop(xmin = -143, ymin = 68.5, xmax = -133, ymax = 70.5) %>% # cropping to relevant areas
  st_difference(qhi_bounds) %>% # removing the area 
  st_geometry() %>%
  st_sf() %>%
  st_transform(coordinate_system) %>% # transfomring the coordinate system
  st_union() # creating a continous line out of the north coast

# create qi buffer region, and split by the north coast line
qhi_buffer <- qhi_coast %>%
  st_buffer(buffer_size) %>%
  lwgeom::st_split(north_coast) %>%
  st_transform(coordinate_system)

# selecting the area of interest
qhi_region <- qhi_buffer[[1]][[1]] %>%
  st_sfc() %>%
  st_sf(geometry = .) %>% # converting the geometry/file type
  st_set_crs(coordinate_system) # converting to right coordinate system

# displaying the layers made in this script
ggplot() +
  geom_sf(data = qhi_region, color = "black", fill = "yellow") +
  geom_sf(data = qhi_coast, color = "blue", fill = "blue") +
  geom_sf(data = north_coast, color = "red")

# Outputs:
# qhi_region 
#   This is the buffered region around Qikiqtaruk, with the area south of the north coast removed
# qhi_coast
#   This is the polygon in the shape of Qikiqtaruk
# north_coast
#   This is the outline of the Yukon North Coast area, a continuous line, but not a closed polygon
