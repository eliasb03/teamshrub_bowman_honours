#------------------------------
# teamshrub_bowman_honours
# 0X_qhi_map_v0
# By: Elias Bowman 
# 2024-07-03
# Description: This R script creates a rough map of Qikiqtaruk - Herschel Island (QHI). 
# It imports relevant data and generates several outputs: a raster or vector basemap of QHI 
# and the north slope, a higher resolution map of QHI, and vector outlines of QHI and the north slope.
#------------------------------

#------------------------------
# Load Required Packages
#------------------------------
library(tidyverse)
library(terra)
library(sp)
library(sf)
library(dplyr)
library(viridis)

#------------------------------
# Set Script Variables
#------------------------------
# Setting script specific variables
buffer_size <- 25000  # Buffer width around QHI region
coordinate_system <- "+proj=longlat +datum=WGS84 +no_defs"  # Project coordinate system

# File import paths

# Import Yukon outline data
#   Source: https://hub.arcgis.com/datasets/2a65ccced3a2421783eeddae32b7d58b_2/explore?location=63.730317%2C-131.505522%2C4.52
yukon_shape_file <- "data/shape/yukon_borders_surveyed/Yukon_Borders_-_Surveyed.shp"
yukon <- st_read(yukon_shape_file) %>%
  st_transform(crs = coordinate_system)  # Set project coordinate system

# Import terrain classification map
terrain_classification <- "data/shape/qhi_vegetation_terrain_map/Ecological_classification_Herschel_Island/Ecological_classification_Herschel_Island.shp"
qhi_terrain <- st_read(terrain_classification) %>%
  st_transform(crs = coordinate_system)  # Set project coordinate system

# Import ARU point locations
aru_points <- "data/shape/device_location/aru_locations/aru_locations_24june.kml" # NOTE THAT THIS ISNT ALL THE LOCATIONS
aru_locations <- st_read(aru_points) %>%
  st_transform(crs = coordinate_system)  # Set project coordinate system

#------------------------------
# Create QHI polygon
#------------------------------
# Set QHI polygon bounds
qhi_coords <- list(matrix(c(
  -139.101, 69.504,  
  -139.330, 69.546,  
  -139.280, 69.628,  
  -139.013, 69.659,  
  -138.799, 69.573,
  -139.101, 69.504
), ncol = 2, byrow = TRUE))

# Create polygon of QHI bounds
qhi_bounds <- qhi_coords %>%
  st_polygon() %>%
  st_geometry() %>%
  st_set_crs(coordinate_system)  # Set coordinate system

# Calculate QHI coastline
qhi_coast <- st_intersection(yukon, qhi_bounds) %>%
  st_union() %>%
  st_polygonize() %>%
  st_transform(crs = coordinate_system)

# Create cropped area of Yukon North Slope
north_coast <- yukon %>%
  st_crop(xmin = -143, ymin = 68.5, xmax = -133, ymax = 70.5) %>%  # Crop to relevant areas
  st_difference(qhi_bounds) %>%  # Remove the QHI area
  st_geometry() %>%
  st_sf() %>%
  st_transform(coordinate_system) %>%  # Transform coordinate system
  st_union()  # Create a continuous line of the north coast

# Create QHI buffer region and split by the north coast line
qhi_buffer <- qhi_coast %>%
  st_buffer(buffer_size) %>%
  lwgeom::st_split(north_coast) %>%
  st_transform(coordinate_system)

# Select the area of interest
qhi_region <- qhi_buffer[[1]][[1]] %>%
  st_sfc() %>%
  st_sf(geometry = .) %>%  # Convert to sf object
  st_set_crs(coordinate_system)  # Set correct coordinate system

#------------------------------
# Set up terrain classification mapping data
#------------------------------
qhi_terrain <- qhi_terrain %>%
  st_make_valid()

#------------------------------
# Clean and prepare ARU location data
#------------------------------
aru_locations <- aru_locations %>%
  mutate(Name = gsub("_", "", Name),  # Remove underscores from Name
         beeproj = !grepl("B", Name)) %>%  # Create beeproj column based on presence of "B"
  st_transform(st_crs(qhi_terrain)) %>%  # Transform CRS to match qhi_terrain
  st_join(qhi_terrain, join = st_intersects) %>%  # Perform spatial join
  select(Name, Description, beeproj, UnitName, grid_code, SOC, TN, IceCont, geometry) %>%  # Select columns to keep
  rename_with(tolower)  %>%# Convert all column names to lower case
  rename(bee_proj = beeproj, ice_cont = icecont, unit_name = unitname)

aru_loc_bbox <- st_bbox(aru_locations)

#------------------------------
# Display the Layers and Data
#------------------------------
# Display qhi region map
ggplot() +
  geom_sf(data = qhi_region, color = "black", fill = "yellow") +
  geom_sf(data = qhi_coast, color = "blue", fill = "blue") +
  geom_sf(data = north_coast, color = "red")

# Display aru location map
ggplot() +
  geom_sf(data = qhi_terrain, aes(fill = UnitName), color = "black") +  # UnitName for the fill aesthetic
  geom_sf(data = aru_locations, color = "purple", size = 3) +
  scale_fill_viridis_d() +  # Use a viridis palette for the fill colors
  labs(fill = "Unit Name") +  # Add a label for the legend
  coord_sf(xlim = c(aru_loc_bbox["xmin"], aru_loc_bbox["xmax"]), 
           ylim = c(aru_loc_bbox["ymin"], aru_loc_bbox["ymax"]))

# Display qhi terrain map with only bee_proj datapoints
ggplot() +
  geom_sf(data = qhi_terrain, aes(fill = UnitName), color = "black") +  # UnitName for the fill aesthetic
  geom_sf(data = aru_locations %>% filter(bee_proj), color = "black", size = 5) +
  scale_fill_viridis_d() +  # Use a viridis palette for the fill colors
  labs(fill = "Unit Name") +  # Add a label for the legend
  coord_sf(xlim = c(aru_loc_bbox["xmin"], aru_loc_bbox["xmax"]), 
           ylim = c(aru_loc_bbox["ymin"], aru_loc_bbox["ymax"]))

# Create a table showing the count of observations in each unit_name
terrain_class_counts <- aru_locations %>%
  group_by(unit_name) %>%  # Group by unit_name
  summarise(observations = n(), .groups = 'drop') 
print(terrain_class_counts)


#------------------------------
# Outputs:
# qhi_region: Buffered region around Qikiqtaruk, with the area south of the north coast removed
# qhi_coast: Polygon in the shape of Qikiqtaruk
# north_coast: Outline of the Yukon North Coast area, a continuous line (not a closed polygon)
#------------------------------
