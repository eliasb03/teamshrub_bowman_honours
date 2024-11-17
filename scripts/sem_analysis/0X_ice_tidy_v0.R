#------------------------------
# teamshrub_bowman_honours
# 0X_ice_tidy_v0
# By: Elias Bowman 
# Created: 2024-10-15
# Last update: 2024-10-15
# 
# Description: This script will import excel ice data from the Canadian Ice Service and tidy and prepare that data for processing.
#------------------------------

# File path to ice data
#file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_15Oct2024/cwa01_02_awtt_1968_2024_0514_1015.csv"
# file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_15Oct2024/cwa01_02_stac_1968_2024_0514_1015.csv"
#file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_15Oct2024/cwa01_02_rmax_1968_2024_0514_1015.csv"
#file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_15Oct2024/cwa01_02_rmin_1968_2024_0514_1015.csv"
# file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_16Nov2024/cwa01_02_awtt_1968_2024_0430_1112.csv"
file_path <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/raw/raw_iceweb_data_16Nov2024/cwa01_02_awtt_1968_2024_0402_1126.csv"


# Import packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)

# Function to import ice data
import_ice_data <- function(file_path) {
  # Read in the column names from the raw ice data file
  colnames <- readLines(file_path, n = 9)[9]
  
  # Process and format column names
  colnames <- colnames %>%
    gsub("<[^>]*>", "", .) %>%
    strsplit(split = ",") %>%
    unlist() %>%
    trimws() %>%
    tolower() %>%
    gsub(" ", "_", .) %>%
    gsub("-", "_", .) %>%
    gsub("[^a-z0-9_]", "", .) %>%
    gsub("ct", "conc", .)
  
  # Import data into the script, starting from the 10th line
  data <- read.csv(file_path, skip = 10, header = FALSE, col.names = colnames)
  
  # Find the index of the first occurrence of "legend" in column 1
  legend_index <- which(data[[1]] == "Legend: / ")
  
  # Remove rows after the first occurrence of "legend" (if it exists)
  if (length(legend_index) > 0) {
    data <- data[1:(legend_index[1] - 1), ]
  }
  
  # Conditional conversion of date columns to Date format
  if (colnames[1] == "season") {
    data$season <- as.numeric(substring(data[[1]], 1, 4))  # Extract year from "season"
  }
  if ((colnames[1] == "week")||(colnames[2] == "week")){
    data$week <- ymd(data$week)
    # Creation of new column containing just week date, independent of year
    data$mmdd <- format(data$week, "%m-%d")
    # Combine 0 year and mmdd to create a comparable weekly date column
    data$week_relative <- as.Date(paste("0-", data$mmdd, "%m-%d"), "%Y-%m-%d")  
    # Create just year column
    data$year <- format(data$week, "%Y")
  }
  
  
  return(data)
}

# Importing ice data
ice_data <- import_ice_data(file_path)


ice.cover.threshold <- 0.8 

# Calculate yearly statistics
yearly_ice_stats <- ice_data %>%
  group_by(year) %>%
  summarise(
    min_conc = min(conc, na.rm = TRUE),                         # Minimum concentration
    min_conc_day = week[which.min(conc)],                       # Day of minimum concentration
    max_conc_prior = max(conc[week < min_conc_day], na.rm = TRUE), # Max concentration before min day
    max_conc_day_prior = max(week[week < min_conc_day & conc == max_conc_prior], na.rm = TRUE), # Day of max concentration
    target_conc = min_conc + ice.cover.threshold * (max_conc_prior - min_conc), # 80% between max_conc_prior and min_conc
    spring_drop_day = max(week[week < min_conc_day & conc >= target_conc], na.rm = TRUE) # Latest day meeting target
  ) %>%
  ungroup() %>% 
  mutate(
    min_conc_doy = yday(min_conc_day),              # DOY for min concentration day
    max_conc_prior_doy = yday(max_conc_day_prior),  # DOY for max concentration prior
    spring_drop_doy = yday(spring_drop_day)         # DOY for spring drop day
  ) %>%
  mutate(year = as.numeric(year))  # Convert year to numeric


ggplot(yearly_ice_stats, aes(x = year, y = spring_drop_doy)) +
  geom_point(color = "blue", size = 2) +  # Points for each year
  geom_smooth(method = lm) +
  labs(
    title = "Day of Spring Ice Drop vs. Year",
    x = "Year",
    y = "Day of Spring Ice Drop"
  ) + 
  theme_half_open(font_size = 14)

#########################


year.of.interest <- 1968

ggplot(filter(ice_data, year == year.of.interest), aes(x = week, y = conc)) +
  geom_line(color = "#A2BFFE", size = 2) +  # Line plot
  #geom_smooth(method = lm) +
  theme_minimal() +
  labs(
    x = "Year",  # X-axis label
    y = "Average Ice Coverage",  # Y-axis label
    title = "Average Concentration of Ice Cover over Time"  # Plot title
  ) +
  theme_half_open(font_size = 14)

ggplot(ice_data, aes(x = week_relative, y = conc, colour = year)) +
  geom_line(aes(color = year), size = 2) +  # Line plot
  #geom_smooth(method = lm) +
  theme_minimal() +
  labs(
    x = "Year",  # X-axis label
    y = "Average Ice Coverage",  # Y-axis label
    title = "Average Concentration of Ice Cover over Time"  # Plot title
  ) + 
  theme_half_open(font_size = 14)








# # test plotting ######
# ggplot(ice_data, aes(x = season, y = conc, color = status)) +
#   geom_line(color = "#A2BFFE", size = 2) +  # Line plot
#   #geom_point(size = 2) +  # Points on the line
#   geom_smooth(method = lm) +
#   theme_minimal() +
#   labs(
#     x = "Year",  # X-axis label
#     y = "Average Ice Coverage",  # Y-axis label
#     title = "Average Concentration of Ice Cover over Time"  # Plot title
#   ) + 
#   theme_half_open(font_size = 14)
#  
# 
# summary(lm(conc ~ season, ice_data))

# ggplot(ice_data, aes(x = season, y = week_relative)) +
#   geom_line(color = "blue") +  # Line plot
#   geom_point(size = 1, color = "red") +  # Points on the line
#   theme_minimal()
