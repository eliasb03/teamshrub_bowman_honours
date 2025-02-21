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

# Specify the columns to keep
bbs_cols <- c("year", "spec.code", "species", "guild", "rel.abundance.total", "rel.abundance.scaled", "logistic.id.total", "total.count")

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


column_mapping <- c(
  "Tave_sm" = "temp",
  "budburst_mean" = "budburst",
  "snowmelt_mean" = "snowmelt",
  "spring_drop_doy" = "ice.melt",
  #"rel.abundance.scaled" = "bird.abundance"
  "total.count" = "bird.abundance"
)

final_data <- final_data %>%
  select(
    year,
    species,
    guild,
    #rel.abundance.scaled,
    total.count,
    budburst_mean,
    snowmelt_mean,
    spring_drop_doy,
    Tave_sm
  ) %>%
  rename_with(~ ifelse(. %in% names(column_mapping), column_mapping[.], .), everything())

# Filter data to remove NA bird abundances
scaled_data <- final_data %>%
  filter(!is.na(bird.abundance))

abundance_scaling <- 1 # remain at intervals of 1 bird increments
doy_scaling <- 7 # 1 week
temp_scaling <- 5 # 5 degrees C

scaled_data <- final_data %>%
  mutate(
    scaled_bird_abundance = (bird.abundance - mean(bird.abundance, na.rm = TRUE)) / abundance_scaling,
    scaled_budburst = (budburst - mean(budburst, na.rm = TRUE)) / doy_scaling,
    scaled_snowmelt = (snowmelt - mean(snowmelt, na.rm = TRUE)) / doy_scaling,
    scaled_ice_melt = (ice.melt - mean(ice.melt, na.rm = TRUE)) / doy_scaling,
    scaled_temp = (temp - mean(temp, na.rm = TRUE)) / temp_scaling
  )

# # Selecting columns to scale
# cols_to_scale <- c("bird.abundance", "budburst", "snowmelt", "ice.melt", "temp")
# 
# # Apply scale() across chosen columns and store result as a dataframe
# scaled_matrix <- scale(final_data[cols_to_scale])
# 
# # Convert to a dataframe
# scaled_data <- final_data %>%
#   select(-all_of(cols_to_scale)) %>%
#   bind_cols(as.data.frame(scaled_matrix))
# 
# # Extract means and standard deviations
# scaling_params <- data.frame(
#   variable = cols_to_scale,
#   mean = attr(scaled_matrix, "scaled:center"),
#   sd = attr(scaled_matrix, "scaled:scale")
# )

# Saving dataset
#write_csv(final_data, "data/clean/sem/sem_data_unscaled.csv")
write_csv(scaled_data, "data/clean/sem/sem_data.csv")
write_csv(scaling_params, "data/clean/sem/scaling_params.csv")

rm(bbs_cols, bbs.file, climate_cols, climate.file, directory, ice_cols, ice.file, phenology_cols, phenology.file, join_by_year)
rm(scaled_matrix)
