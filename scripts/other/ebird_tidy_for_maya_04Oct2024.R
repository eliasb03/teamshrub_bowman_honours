#------------------------------
# teamshrub_bowman_honours
# 0X_ebird_tidy_v0
# By: Elias Bowman 
# Created: 2024-07-03
# Updated: 2024-10-04
# Description: SLIGHTLY MODIFIED FOR SHARING 
# Script will import the Yukon ebird dataset.
# Clean and tidy the data to include only relevant variables for my analysis and limit the observations to those that surround QHI.
# 
# Roughly script count: #2
# 
#------------------------------

# importing packages
library(tidyverse)
library(readr)
library(ggplot2)
library(skimr)
library(terra)
library(leaflet)
library(maptiles)


# specify file path to ebird data
ebird_file_path <- "D:/ebird_download/ebd_CA-YT_unv_smp_relMay-2024/ebd_CA-YT_unv_smp_relMay-2024.txt"

# read data from ebird
ebird_full_data <- read.table(ebird_file_path, header = TRUE, sep = "\t", quote = "", fill = TRUE, stringsAsFactors = FALSE)

# specify columns of interest
columns_to_keep <- c("OBSERVATION.DATE", "COMMON.NAME", "SCIENTIFIC.NAME", "OBSERVATION.COUNT", "COUNTY", "LOCALITY", "LOCALITY.ID", "YEAR", "LATITUDE", "LONGITUDE", "OBSERVER.ID", "GROUP.IDENTIFIER", "REVIEWED", "TRIP.COMMENTS", "SPECIES.COMMENTS", "SAMPLING.EVENT.IDENTIFIER", "TAXONOMIC.ORDER", "TAXON.CONCEPT.ID", "CATEGORY", "PROTOCOL.TYPE", "DURATION.MINUTES", "NUMBER.OBSERVERS", "GLOBAL.UNIQUE.IDENTIFIER")

# process ebird data
# select only northern observations, convert date and isolate year, select columns of interest, and make all lowercase
ebird_north_data <- ebird_full_data %>%
  filter(LATITUDE > 69) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE, format = "%Y-%m-%d")) %>%
  mutate(YEAR = as.integer(format(OBSERVATION.DATE, "%Y"))) %>%
  select(all_of(columns_to_keep)) %>%
  rename_all(tolower)

# importing qikqtaruk outline shapefile
qhi_region_shp_dir <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/shape/qhi_bird_polygons_July1/qhi_region.shp"
qhi_region <- vect(qhi_region_shp_dir)

# converting the coordinate system to mathc the ebird data 
qhi_region <- project(qhi_region, "+proj=longlat +datum=WGS84", partial = FALSE)

# converting ebird_data to shapefiles
ebird_coordinate_system <- "+proj=longlat +datum=WGS84" #specifying coordinate system

# "longlat" is just saying that the data are georefrenced based on longitude and latitude
ebird_vect <- vect(ebird_north_data, geom = c("longitude", "latitude"), crs = ebird_coordinate_system)

# selecting only ebird observations that intersect with the qhi outline
qhi_ebird_vect <- intersect(ebird_vect, qhi_region)

# turning ebird qhi spatvector back into a dataframe
qhi_ebird <- as.data.frame(qhi_ebird_vect)

write.csv(qhi_ebird,"D:/ebird_data/ebird_QHI_region_unv_smp_relMay-2024.csv", row.names = FALSE)

# quick map of points
bg <- get_tiles(ext(qhi_ebird_vect))
plotRGB(bg)
points(qhi_ebird_vect, col="blue")
