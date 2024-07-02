####################################
# 01_ebird_processing_v0
#
#
#
# 2024-06-26
######################################
# importing packages
library(tidyverse)
library(readr)
library(ggplot2)
library(skimr)
library(terra)

# Specify the path to file

ebird_file_path_harddrive <- "D:/Bowman_birds/ebird_download/ebd_CA-YT_unv_smp_relMay-2024/ebd_CA-YT_unv_smp_relMay-2024.txt"
ebird_file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_ecology_honours/data/raw/ebd_CA-YT_unv_smp_relMay-2024/ebd_CA-YT_unv_smp_relMay-2024.txt"

ebird_full_data <- read.table(ebird_file_path, header = TRUE, sep = "\t", quote = "", fill = TRUE, stringsAsFactors = FALSE)

columns_to_keep <- c("OBSERVATION.DATE", "COMMON.NAME", "SCIENTIFIC.NAME", "OBSERVATION.COUNT", "COUNTY", "LOCALITY", "LOCALITY.ID", "YEAR", "LATITUDE", "LONGITUDE", "OBSERVER.ID", "GROUP.IDENTIFIER", "REVIEWED", "TRIP.COMMENTS", "SPECIES.COMMENTS", "SAMPLING.EVENT.IDENTIFIER", "TAXONOMIC.ORDER", "TAXON.CONCEPT.ID", "CATEGORY", "PROTOCOL.TYPE", "DURATION.MINUTES", "NUMBER.OBSERVERS", "GLOBAL.UNIQUE.IDENTIFIER")

ebird_north_data <- ebird_full_data %>%
  filter(LATITUDE > 69) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE, format = "%Y-%m-%d")) %>%
  mutate(YEAR = as.integer(format(OBSERVATION.DATE, "%Y"))) %>%
  select(all_of(columns_to_keep)) %>%
  mutate(REGION = str_extract(LOCALITY, "^[^-]+"))


skim(ebird_north_data)

unique(ebird_north_data$LOCALITY)

# Importing Qikqtaruk Shapefile
qhi_outline <- vect("C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_ecology_honours/data/shape/qhi_bird_polygons_July1/qhi_region.shp")

plot(qhi_outline, "Shape_Area")
head(qhi_outline)

# Converting ebird_data to shapefiles
ebird_coordinate_system <- "+proj=longlat +datum=WGS84" # NOTE: this is a dummy variable, I have not confirmed this is the coordinate system that ebird data uses, but it seems likely
  # "longlat" is just saying that the data are georefrenced based on longitude and latitude
ebird_vect <- vect(ebird_north_data, geom = c("LONGITUDE", "LATITUDE"), crs = ebird_coordinate_system)

#############
# # selecting for observations within latitude and longitude bounds of region
# # rough square boundaries for QHI
# se_lat <- 69.493583
# se_lng <- -138.792971
# nw_lat <- 69.676441
# nw_lng <- -139.335002
# 
# # filtering to include only observations within the box
# qhi_ebird <- ebird_import %>%
#   filter(lat >= se_lat & lat <= nw_lat & lng >= nw_lng & lng <= se_lng)


