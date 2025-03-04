#------------------------------
# teamshrub_bowman_honours
# 05_sem_data_v2
# By: Elias Bowman
# Created: 2025-01-18
# Last Updated: 2025-02-21
#
# Description: This script joins all of the relevant SEM data into a working dataset, summarizes to the year and guild level and scales and centers the data for SEM analysis
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
climate_cols <- c("Tave_sp", "mean_temp_breeding_season")

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
  "mean_temp_breeding_season" = "breedingtemp",
  #"Tave_sm" = "regiontemp",
  "Tave_sp" = "regiontemp",
  "budburst_mean" = "budburst",
  "snowmelt_mean" = "snowmelt",
  "spring_drop_doy" = "icemelt",
  #"rel.abundance.scaled" = "birdabundance"
  #"rel.abundance.total" = "bird.abundance"
  "total.count" = "birdabundance"
)

final_data <- final_data %>%
  select( # Select relevant columns
    year,
    species,
    guild,
    #rel.abundance.scaled,
    #rel.abundance.total,
    total.count,
    budburst_mean,
    snowmelt_mean,
    spring_drop_doy,
    #Tave_sm,
    Tave_sp,
    mean_temp_breeding_season
  ) %>%
  rename_with(~ ifelse(. %in% names(column_mapping), column_mapping[.], .), everything()) #%>% # rename columns to simple modelling names 



gap_fill_and_mark <- function(df, columns_to_fill, long_term_means) {
  # Create a gap_filled column to mark where filling occurred
  df$gap_filled <- FALSE
  
  # Apply gap filling for each specified column
  df <- df %>%
    mutate(across(all_of(columns_to_fill), 
                  ~ ifelse(is.na(.x) & rowSums(is.na(df[columns_to_fill])) < 3, 
                           long_term_means[[cur_column()]], .x), 
                  .names = "filled_{.col}")) %>%
    mutate(gap_filled = if_any(starts_with("filled_"), ~ !is.na(.x) & is.na(get(sub("filled_", "", cur_column()))))) %>%
    mutate(across(starts_with("filled_"), 
                  ~ ifelse(is.na(get(sub("filled_", "", cur_column()))), .x, get(sub("filled_", "", cur_column()))),
                  .names = "{sub('filled_', '', .col)}")) %>%
    select(-starts_with("filled_"))
  
  return(df)
}

# Apply the function to your data
cols_to_fill <-c("budburst", "snowmelt", "icemelt", "regiontemp", "breedingtemp")
long_term_means <- final_data %>%
  summarise(across(all_of(cols_to_fill), ~mean(.x, na.rm = TRUE)))

final_data <- gap_fill_and_mark(final_data, cols_to_fill, long_term_means)


# Filter data to remove NA bird abundances or repeated columns
scaled_data <- final_data %>%
  filter(!is.na(birdabundance)) %>% # remove any NA bird abundances
  .[!duplicated(.), ] # remove duplicated columns

    
# Specifying scaling parameters
#abundance_scaling <- 1 # remain at intervals of 1 bird increments
doy_scaling <- 7 # 1 week
temp_scaling <- 5 # 5 degrees C

# Saving scaling and centering parameters (mean and scale value) to dataframe 
scaling_params <- data.frame(
  variable = c(#"bird.abundance", 
               "budburst", "snowmelt", "icemelt", "regiontemp", "breedingtemp"),
  mean = c(
    #mean(final_data$bird.abundance, na.rm = TRUE),
    mean(final_data$budburst, na.rm = TRUE),
    mean(final_data$snowmelt, na.rm = TRUE),
    mean(final_data$icemelt, na.rm = TRUE),
    mean(final_data$regiontemp, na.rm = TRUE),
    mean(final_data$breedingtemp, na.rm = TRUE)
  ),
  scaling_value = c(
    #abundance_scaling,
    doy_scaling,
    doy_scaling,
    doy_scaling,
    temp_scaling,
    temp_scaling
  )
)

# Scaing and centering according to specifications
scaled_data <- final_data %>%
  mutate(
    #bird.abundance = (bird.abundance - mean(bird.abundance, na.rm = TRUE)) / abundance_scaling,
    budburst = (budburst - scaling_params$mean[1]) / scaling_params$scaling_value[1],
    snowmelt = (snowmelt - scaling_params$mean[2]) / scaling_params$scaling_value[2],
    icemelt = (icemelt - scaling_params$mean[3]) / scaling_params$scaling_value[3],
    regiontemp = (regiontemp - scaling_params$mean[4]) / scaling_params$scaling_value[4],
    breedingtemp = (breedingtemp - scaling_params$mean[5]) / scaling_params$scaling_value[5]
  )

# Saving dataset
#write_csv(final_data, "data/clean/sem/sem_data_unscaled.csv")
write_csv(scaled_data, "data/clean/sem/sem_data.csv")
write_csv(scaling_params, "data/clean/sem/scaling_params.csv")

rm(bbs_cols, bbs.file, climate_cols, climate.file, directory, ice_cols, ice.file, phenology_cols, phenology.file, join_by_year)
