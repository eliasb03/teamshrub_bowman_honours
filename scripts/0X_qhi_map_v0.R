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
library(rnaturalearth)
library(rnaturalearthdata)

# importing coastline from R Natural Earth
global_coastline <- ne_coastline(scale = 10, returnclass = c("sv"))

# coordinate outline for areas of interest
# North Slope area
y_max <- 71.08436
y_min <- 67.889931
x_max <- -132.965956
x_min <- -144.615307
north_slope <- ext(x_min, x_max, y_min, y_max)
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
qhi_region <- vect(qhi_region_coords, geom=c("lon", "lat"), keepgeom=FALSE)

summary(qhi_region)
plot(qhi_region)
############# Curretnly the above is not working right, trying to make a polygon surroundign the island, but i can't seem to make a spatVector polygon for some reason.




north_slope_coast <- crop(global_coastline, north_slope)
qikiqtaruk_coast <- crop(global_coastline, qhi_region)


par(mfrow=c(1, 3))
plot(global_coastline, main = "Global Coastlines")
plot(north_slope_coast, main = "North Slope Coastline")
plot(qikiqtaruk_coast, main = "Qikiqtaruk Slope Coastline")
