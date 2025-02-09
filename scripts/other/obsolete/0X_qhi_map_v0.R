#------------------------------
# teamshrub_bowman_honours
# 0X_qhi_map_v0
# By: Elias Bowman 
# 2024-07-03
# Description: This R script should create a rough map of Qikiqtaruk - Herschel Island (QHI). It will import relevant data and create several files. A raster or vector basemap of QHI and the north slope, a higher resolution similar map of QHI, and a vector outline of just QHI, and just the north slopes, are ideal outputs of this script
# 
# Roughly script count: #1
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

# 
# ggplot() +
#   geom_sf(data = qhi_polygon3, color = "black")

####################################


# # calculating intersection of yukon coast and qhi
# qhi_coast <- st_intersection(yukon, qhi_bounds) 


# creating a polygon area of the qhi outline
# qhi_polygon <- qhi_coast %>%
#   st_union() %>%
#   st_polygonize() %>%
#   st_transform(coordinate_system)


# qhi_region <- qhi_buffer[[1]][[1]]
# qhi_region <- st_sfc(qhi_region)
# qhi_region <- st_sf(geometry = qhi_region)
# st_crs(qhi_region) <- coordinate_system


# # creating a straight bottom line of north coast 
# bottom_line <- st_sf(geometry = st_sfc(st_linestring(matrix(c(-141, 68.6, -136.4, 68.6), ncol = 2, byrow = TRUE)), crs = coordinate_system))
# 
# #ncoast_polygon <- rbind(north_coast, bottom_line)
# ncoast_polygon <- north_coast %>% 
#   rbind(bottom_line) %>%
#   st_crop(xmin = -141.01, ymin = 68.599, xmax = -136.44, ymax = 70.5) %>%
#   st_union() #%>%
# # st_polygonize()

# ##################################
#  Below lies the Norway problem
# proceed with caution
# 
# also known deputatsky probem
# # importing yukon map vector outline
# # source yukon gov geodatabase
# yukon_map <- vect("data/shape/yukon_and_adjoining_land_mass/Yukon_and_Adjoining_Land_Mass.shp")
# yukon_map <- st_read("data/shape/yukon_and_adjoining_land_mass/Yukon_and_Adjoining_Land_Mass.shp")
# # changing coordinate system
# coordinate_system <- "+proj=longlat +datum=WGS84"
# yukon_map <- project(yukon_map, coordinate_system)
# plot(yukon_map)
# 
# # coordinate outline for areas of interest
# # North Slope area
# y_max <- 70.5
# y_min <- 68.5
# x_max <- -133
# x_min <- -143
# north_slope <- ext(c(x_min, x_max, y_min, y_max))
# 
# plot(north_slope)
# plot(yukon_map, add = TRUE)
# 
# # window(yukon_map) <- north_slope
# 
# north_slope_coast <- crop(yukon_map, north_slope)
# 
# 
# # Qikiqtaruk area
# # pentagon surrounding qhi, seperate from the coast
# qhi_region_coords <-
#   data.frame(
#     lon = c(
#       -139.101711148163,
#       -139.330770272735,
#       -139.280239252751,
#       -139.013957769567,
#       -138.799257953520,
#       -139.101711148163
#     ),
#     lat = c(
#       69.504011032690,
#       69.546801863821,
#       69.628420776634,
#       69.659566892761,
#       69.573901614936,
#       69.504011032690
#     )
#   )
# 
# # Converts coordinate dataframe into a spatVector and spatExtent
# qhi_region_matrix <- as.matrix(qhi_region_coords[, c("lon", "lat")])
# qhi_region <- vect(qhi_region_matrix, "polygons") #spatVector
# crs(qhi_region) <- coordinate_system # resetting coordinate system
# qhi_region_ext <- ext(qhi_region) #spatExtent
# 
# qhi_coords <- matrix(c(
#   -139.101, 69.504,  
#   -139.330, 69.546,  
#   -139.280, 69.628,  
#   -139.013, 69.659,  
#   -138.799, 69.573,
#   -139.101, 69.504
# ), ncol = 2, byrow = TRUE)
# 
# # Create the crop_area SpatVector
# qhi_crop_area <- vect(qhi_coords, type = "polygons")
# plot(yukon_map)
# plot(qhi_crop_area)
# plot(yukon_map, add = TRUE)
# 
# qikiqtaruk_coast <- raster::intersect(yukon_map, qhi_crop_area)
# 
# plot(yukon_map)
# plot(qikiqtaruk_coast)
# 
# plot(qhi_region)
# plot(qhi_region_ext)
# 
# test <- mask(yukon_map, qhi_region)
# plot(test)
# qikiqtaruk_coast <- intersect(yukon_map, qhi_region)
# 
# qikiqtaruk_coast <- crop(yukon_map, qhi_region) # consider mask = TRUE??
# plot(qikiqtaruk_coast)
# 
# par(mfrow=c(1, 2))
# 
# window(yukon_map, qhi_region_ext)
# 
# # 
# # 
# # qikiqtaruk_coast <- crop(global_coastline, qhi_region_ext)
# # qhi_region_ext
# # north_slope
# # global_coastline
# # qhi_region
# # qikiqtaruk_coast
# # plot(qikiqtaruk_coast)
# # summary(qhi_region)
# # 
# # summary(north_slope_coast)
# # summary(north_slope)
# # 
# # par(mfrow=c(1, 3))
# # 
# # plot(global_coastline, main = "Global Coastlines")
# # plot(north_slope_coast, main = "North Slope Coastline")
# # plot(qikiqtaruk_coast, main = "Qikiqtaruk Slope Coastline")
# # 
# # 
# 
# 
# coords <- matrix(c(
#   -120, 35,  # First point
#   -119, 35,  # Second point
#   -119, 34,  # Third point
#   -120, 34,  # Fourth point
#   -120, 35   # Fifth point to close the polygon
# ), ncol = 2, byrow = TRUE)
# 
# # Create the crop_area SpatVector
# crop_area <- vect(coords, type = "polygons")
# 
# 
# r <- rast(xmin=0, xmax=10, ymin=0, ymax=10, nrows=25, ncols=25)
# values(r) <- 1:ncell(r)
# e <- ext(-5, 5, -5, 5)
# rc <- crop(r, e)
# 
# 
# # library(rnaturalearth)
# # library(rnaturalearthdata)
# 
# # # importing coastline from R Natural Earth
# # global_coastline <- ne_coastline(scale = 10, returnclass = c("sv"))

