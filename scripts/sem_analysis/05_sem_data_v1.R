#------------------------------
# teamshrub_bowman_honours
# 05_sem_data_v1
# By: Elias Bowman
# Created: 2025-01-18
# Last Updated: 2025-01-18
#
# Description: This script joins all of the relevant SEM data into a working dataset
#------------------------------

# Load the necessary packages
library(tidyverse)

directory <- "data/clean/sem/"

bbs.file <- "bbs_data.csv"
ice.file <- "ice_data.csv"
phenology.file <- "phenology_data.csv"
climate.file <- "climate_data.csv"

# Read in each file
bbs_data <- read_csv(file.path(directory, bbs.file))
ice_data <- read_csv(file.path(directory, ice.file))
phenology_data <- read_csv(file.path(directory, phenology.file))
climate_data <- read_csv(file.path(directory, climate.file))

# Function to join datasets by year
join_by_year <- function(bbs_data, ice_data, phenology_data, climate_data, 
                         bbs_cols, ice_cols, phenology_cols, climate_cols) {
  # Select the specified columns from each dataset
  bbs_selected <- bbs_data %>% select(all_of(bbs_cols))
  ice_selected <- ice_data %>% select(year, all_of(ice_cols))
  phenology_selected <- phenology_data %>% select(year, all_of(phenology_cols))
  climate_selected <- climate_data %>% select(year, all_of(climate_cols))
  
  # Perform a series of left joins to combine the datasets
  joined_data <- bbs_selected %>%
    left_join(ice_selected, by = "year") %>%
    left_join(phenology_selected, by = "year") %>%
    left_join(climate_selected, by = "year")
  
  return(joined_data)
}

# Example usage
# Specify the columns to keep
bbs_cols <- c("year", "spec.code", "species", "rel.abundance.total", "rel.abundance.scaled", "logistic.id.total")

ice_cols <- c("spring_drop_doy")
phenology_cols <- c("snowmelt_mean", "budburst_mean")
climate_cols <- c("Tave_sm")

# Call the function with your datasets
final_data <- join_by_year(
  bbs_data = bbs_data,
  ice_data = ice_data,
  phenology_data = phenology_data,
  climate_data = climate_data,
  bbs_cols = bbs_cols,
  ice_cols = ice_cols,
  phenology_cols = phenology_cols,
  climate_cols = climate_cols
)

# Saving dataset
write_csv(final_data, "data/clean/sem/sem_data.csv")

rm(bbs_cols, bbs.file, climate_cols, climate.file, directory, ice_cols, ice.file, phenology_cols, phenology.file, join_by_year)
