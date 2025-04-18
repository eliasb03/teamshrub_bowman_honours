#------------------------------
# teamshrub_bowman_honours
# 0X_qhi_map_v2
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
library(readr)

#------------------------------
# Set Script Variables
#------------------------------
buffer_size <- 25000  # Buffer width around QHI region
  # sf_use_s2() is true, so this is metres
coordinate_system <- "+proj=longlat +datum=WGS84 +no_defs"  # Project coordinate system

# File import paths
yukon_shape_file <- "data/raw/shape/yukon_borders_surveyed/Yukon_Borders_-_Surveyed.shp"
terrain_classification <- "data/raw/shape/qhi_vegetation_terrain_map/Ecological_classification_Herschel_Island/Ecological_classification_Herschel_Island.shp"
#aru_points <- "D:/ARU_mapping/aru_locations/qhi_aru_locations_2024.shp"

# Import Yukon outline data
yukon <- st_read(yukon_shape_file) %>%
  st_transform(crs = coordinate_system)  # Set project coordinate system

# Import terrain classification map
qhi_terrain <- st_read(terrain_classification) %>%
  st_transform(crs = coordinate_system) %>%
  rename_with(tolower) %>%  # Convert all column names to lower case
  rename(unit_name = unitname, ice_cont = icecont, object_id = objectid) %>% # Change UnitName to unit_name
  st_make_valid()

# Import ARU point locations
# aru_locations <- st_read(aru_points) %>%
#   st_transform(crs = coordinate_system)  # Set project coordinate system

#------------------------------
# Create QHI polygon
#------------------------------
qhi_coords <- list(matrix(c(
  -139.101, 69.504,  
  -139.330, 69.546,  
  -139.280, 69.628,  
  -139.013, 69.659,  
  -138.799, 69.573,
  -139.101, 69.504
), ncol = 2, byrow = TRUE))

qhi_bounds <- qhi_coords %>%
  st_polygon() %>%
  st_geometry() %>%
  st_set_crs(coordinate_system)  # Set coordinate system

qhi_coast <- st_intersection(yukon, qhi_bounds) %>%
  st_union() %>%
  st_polygonize() %>%
  st_transform(crs = coordinate_system)

north_coast <- yukon %>%
  st_crop(xmin = -143, ymin = 68.5, xmax = -133, ymax = 70.5) %>%
  st_difference(qhi_bounds) %>%
  st_geometry() %>%
  st_sf() %>%
  st_transform(coordinate_system) %>%
  st_union()

qhi_buffer <- qhi_coast %>%
  st_buffer(buffer_size) %>%
  lwgeom::st_split(north_coast) %>%
  st_transform(coordinate_system)

qhi_region <- qhi_buffer[[1]][[1]] %>%
  st_sfc() %>%
  st_sf(geometry = .) %>%
  st_set_crs(coordinate_system)


#------------------------------
# Clean and prepare ARU location data
#------------------------------
aru_loc_columns <- c("name", "short_name", "unit_name", "descriptio", "notes", "bee_proj", "grid_code", "soc", "tn", "ice_cont", "geometry")

aru_locations <- aru_locations %>%
  rename_with(tolower) %>%  # Convert all column names to lower case
  mutate(bee_proj = as.logical(bee_proj == "T")) %>% # convert bee_proj to boolean
  st_transform(st_crs(qhi_terrain)) %>%  # Transform CRS to match qhi_terrain
  st_join(qhi_terrain, join = st_intersects) %>%  # Perform spatial join
  select(all_of(aru_loc_columns))  # Select columns to keep

# Create bounding box for plotting aru_locations
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
  geom_sf(data = qhi_terrain, aes(fill = unit_name), color = "black") +
  #geom_sf(data = aru_locations, color = "purple", size = 3) +
  scale_fill_viridis_d() +
  labs(fill = "Unit Name") +
  coord_sf(xlim = c(aru_loc_bbox["xmin"], aru_loc_bbox["xmax"]), 
           ylim = c(aru_loc_bbox["ymin"], aru_loc_bbox["ymax"]))


ggplot() +
  geom_sf(data = qhi_coast, color = "black", fill = "black") +
  theme_void()

# # Display qhi terrain map with only bee_proj datapoints
# ggplot() +
#   geom_sf(data = qhi_terrain, aes(fill = unit_name), color = "black") +
#   geom_sf(data = aru_locations %>% filter(bee_proj), color = "black", size = 5) +
#   scale_fill_viridis_d() +
#   labs(fill = "Unit Name") +
#   coord_sf(xlim = c(aru_loc_bbox["xmin"], aru_loc_bbox["xmax"]),
#            ylim = c(aru_loc_bbox["ymin"], aru_loc_bbox["ymax"]))

# Create a table showing the count of observations in each unit_name
# terrain_class_counts <- aru_locations %>%
#   group_by(unit_name) %>%
#   summarise(observations = n(), .groups = 'drop') 

#------------------------------
# Create Ice Region Boundary
#------------------------------
# https://ice-glaces.ec.gc.ca/IA_DOC/cisads_no_003_e.pdf
# Page 88, Number Page 76
coords <- tibble::tribble(
  ~POINT_ID, ~LATITUDE, ~LONGITUDE, ~RG_NAME, ~PNT_NAME, ~KNOWN_DMS,
  6260, 72.0000, -141.0000, "cwa01_02", "US Canada border", "72 00 00 N,141 00 00 W",
  6270, 72.0000, -140.0000, "cwa01_02", NA, NA,
  6280, 72.0000, -139.0000, "cwa01_02", NA, NA,
  6290, 72.0000, -138.0000, "cwa01_02", NA, NA,
  6300, 72.0000, -137.0000, "cwa01_02", NA, NA,
  6310, 72.0000, -136.0000, "cwa01_02", NA, NA,
  6320, 72.0000, -135.0000, "cwa01_02", "Estimated", "72 00 00 N,135 00 00 W",
  6330, 72.0000, -134.0000, "cwa01_02", NA, NA,
  6340, 72.0000, -133.0000, "cwa01_02", NA, NA,
  6350, 72.0000, -132.0000, "cwa01_02", NA, NA,
  6360, 72.0000, -131.0000, "cwa01_02", "Estimated", "72 00 00 N,131 00 00 W",
  6370, 70.5761, -128.0385, "cwa01_02", NA, NA,
  6380, 70.5669, -128.0000, "cwa01_02", "Cape Bathurst", "70 34 00 N,128 00 00 W",
  6390, 70.5656, -127.9987, "cwa01_02", NA, NA,
  6400, 70.2045, -127.2075, "cwa01_02", NA, NA,
  6410, 69.1997, -126.5959, "cwa01_02", NA, NA,
  6420, 68.5500, -132.0000, "cwa01_02", NA, NA,
  6430, 68.5000, -137.0000, "cwa01_02", NA, NA,
  6440, 69.3211, -140.7362, "cwa01_02", NA, NA,
  6450, 69.6474, -141.0000, "cwa01_02", "Coast and border", "69 37 00 N,141 00 00 W",
  6460, 72.0000, -141.0000, "cwa01_02", "US Canada border", "72 00 00 N,141 00 00 W"
)

coords_sf <- st_as_sf(coords, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Ensure the polygon is closed
coords_closed <- coords_sf %>%
  bind_rows(slice(., 1))

mackenzie_ice_poly <- coords_closed %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

library(ggplot2)
ggplot() +
  geom_sf(data = mackenzie_ice_poly, fill = "lightblue", color = "black") +
  geom_sf(data = north_coast, color = "black", fill = "black") +
  geom_sf(data = coords_sf, color = "red", size = 2) +
  theme_minimal()

#------------------------------
# Save Shapefiles
#------------------------------
# Function to convert geometries appropriately
convert_geometry <- function(sf_object) {
  # Check if the object contains geometry collections and convert if necessary
  if (any(st_geometry_type(sf_object) == "GEOMETRYCOLLECTION")) {
    sf_object <- st_collection_extract(sf_object, "POLYGON")
  }
  
  # Check if the geometry is of type POINT
  if ("POINT" %in% st_geometry_type(sf_object)) {
    return(st_zm(sf_object))  # Remove Z and M dimensions for point geometries
  } else {
    return(sf_object)  # Return as is for non-point geometries
  }
}

output_dir <- "data/clean/shape"

# Convert geometries and save shapefiles
st_write(convert_geometry(qhi_region), file.path(output_dir, "qhi_region.shp"), append = FALSE)
st_write(convert_geometry(qhi_coast), file.path(output_dir, "qhi_coast.shp"), append = FALSE)
st_write(convert_geometry(north_coast), file.path(output_dir, "north_coast.shp"), append = FALSE)
st_write(convert_geometry(qhi_terrain), file.path(output_dir, "qhi_terrain.shp"), append = FALSE)
st_write(convert_geometry(aru_locations), file.path(output_dir, "aru_locations.shp"), append = FALSE)
st_write(mackenzie_ice_poly, file.path(output_dir, "cwa01_02_polygon.shp"), delete_layer = TRUE)

# Closing unnecessary objects
rm(yukon_shape_file, terrain_classification, buffer_size, yukon, qhi_buffer, qhi_coords, qhi_bounds, terrain_class_counts, convert_geometry, output_dir, aru_loc_columns)
rm(aru_points)

#------------------------------
# Outputs:
# qhi_region: Buffered region around Qikiqtaruk, with the area south of the north coast removed
# qhi_coast: Polygon in the shape of Qikiqtaruk
# north_coast: Outline of the Yukon North Coast area, a continuous line (not a closed polygon)
#------------------------------

