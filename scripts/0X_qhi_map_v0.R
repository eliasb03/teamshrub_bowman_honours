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
# importing yukon map vector outline
  # source yukon gov geodatabase
yukon_map <- vect("data/shape/yukon_and_adjoining_land_mass/Yukon_and_Adjoining_Land_Mass.shp")
# changing coordinate system
coordinate_system <- "+proj=longlat +datum=WGS84"
yukon_map <- project(yukon_map, coordinate_system)
plot(yukon_map)
# # coordinate outline for areas of interest
# # North Slope area
# y_max <- 71.08436
# y_min <- 67.889931
# x_max <- -132.965956
# x_min <- -144.615307
# north_slope <- ext(c(x_min, x_max, y_min, y_max))
# 
# plot(yukon_map)
# 
# window(yukon_map) <- north_slope
# 
# north_slope_coast <- window(yukon_map, north_slope)



# Qikiqtaruk area
  # pentagon surrounding qhi, seperate from the coast
qhi_region_coords <-
  data.frame(
    lon = c(
      -139.101711148163,
      -139.330770272735,
      -139.280239252751,
      -139.013957769567,
      -138.799257953520,
      -139.101711148163
    ),
    lat = c(
      69.504011032690,
      69.546801863821,
      69.628420776634,
      69.659566892761,
      69.573901614936,
      69.504011032690
    )
  )

# Converts coordinate dataframe into a spatVector and spatExtent
qhi_region_matrix <- as.matrix(qhi_region_coords[, c("lon", "lat")])
qhi_region <- vect(qhi_region_matrix, "polygons") #spatVector
crs(qhi_region) <- coordinate_system # resetting coordinate system
qhi_region_ext <- ext(qhi_region) #spatExtent
  
qhi_coords <- matrix(c(
  -139.101711148163, 69.504011032690,  
  -139.330770272735, 69.546801863821,  
  -139.280239252751, 69.628420776634,  
  -139.013957769567, 69.659566892761,  
  -138.799257953520, 69.573901614936,
  -139.101711148163, 69.504011032690
), ncol = 2, byrow = TRUE)

# Create the crop_area SpatVector
qhi_crop_area <- vect(qhi_coords, type = "polygons")



plot(qhi_region)
plot(qhi_region_ext)

test <- mask(yukon_map, qhi_region)
plot(test)
qikiqtaruk_coast <- intersect(yukon_map, qhi_region)

qikiqtaruk_coast <- crop(yukon_map, qhi_region) # consider mask = TRUE??
plot(qikiqtaruk_coast)

par(mfrow=c(1, 2))

window(yukon_map, qhi_region_ext)

# 
# 
# qikiqtaruk_coast <- crop(global_coastline, qhi_region_ext)
# qhi_region_ext
# north_slope
# global_coastline
# qhi_region
# qikiqtaruk_coast
# plot(qikiqtaruk_coast)
# summary(qhi_region)
# 
# summary(north_slope_coast)
# summary(north_slope)
# 
# par(mfrow=c(1, 3))
# 
# plot(global_coastline, main = "Global Coastlines")
# plot(north_slope_coast, main = "North Slope Coastline")
# plot(qikiqtaruk_coast, main = "Qikiqtaruk Slope Coastline")
# 
# 


coords <- matrix(c(
  -120, 35,  # First point
  -119, 35,  # Second point
  -119, 34,  # Third point
  -120, 34,  # Fourth point
  -120, 35   # Fifth point to close the polygon
), ncol = 2, byrow = TRUE)

# Create the crop_area SpatVector
crop_area <- vect(coords, type = "polygons")


r <- rast(xmin=0, xmax=10, ymin=0, ymax=10, nrows=25, ncols=25)
values(r) <- 1:ncell(r)
e <- ext(-5, 5, -5, 5)
rc <- crop(r, e)


# library(rnaturalearth)
# library(rnaturalearthdata)

# # importing coastline from R Natural Earth
# global_coastline <- ne_coastline(scale = 10, returnclass = c("sv"))
