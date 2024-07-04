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
library(terra)
library(sp)
library(sf)
# importing yukon map vector outline
  # source yukon gov geodatabase
yukon_map <- vect("data/shape/yukon_and_adjoining_land_mass/Yukon_and_Adjoining_Land_Mass.shp")
yukon_map <- project(yukon_map, "+proj=longlat +datum=WGS84")

# coordinate outline for areas of interest
# North Slope area
y_max <- 71.08436
y_min <- 67.889931
x_max <- -132.965956
x_min <- -144.615307
north_slope <- c(x_min, x_max, y_min, y_max)

# Qikiqtaruk area
qhi_region_coords <-
  data.frame(
    lon = c(
      139.101711148163,
      139.330770272735,
      139.280239252751,
      139.013957769567,
      138.799257953520,
      139.101711148163
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

# Converting dataframe into a matrix
qhi_region_matrix <- as.matrix(qhi_region_coords[, c("lon", "lat")])
# Converting 
qhi_region <- vect(qhi_region_matrix, "polygons")
qhi_region_ext <- ext(qhi_region)
plot(qhi_region)

north_slope_coast <- crop(yukon_map, north_slope)

qikiqtaruk_coast <- crop(global_coastline, qhi_region_ext)
qhi_region_ext
north_slope
global_coastline
qhi_region
qikiqtaruk_coast
plot(qikiqtaruk_coast)
summary(qhi_region)

summary(north_slope_coast)
summary(north_slope)

par(mfrow=c(1, 3))

plot(global_coastline, main = "Global Coastlines")
plot(north_slope_coast, main = "North Slope Coastline")
plot(qikiqtaruk_coast, main = "Qikiqtaruk Slope Coastline")







# library(rnaturalearth)
# library(rnaturalearthdata)

# # importing coastline from R Natural Earth
# global_coastline <- ne_coastline(scale = 10, returnclass = c("sv"))
