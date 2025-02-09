#------------------------------
# teamshrub_bowman_honours
# 04_climate_station_v1
# By: Elias Bowman 
# Created: 2025-01-27
#
# Description: This script imports climate station data from Herschel Island Yukon Territory Climate Station
# Climate ID: 2100636
# https://climate.weather.gc.ca/climate_data/daily_data_e.html?timeframe=2&Year=2024&Month=7&Day=18&hlyRange=1994-02-01%7C2024-12-18&dlyRange=1974-07-01%7C2024-12-18&mlyRange=1974-01-01%7C2007-01-01&StationID=1560&Prov=YT&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2024&selRowPerPage=25&Line=1&searchMethod=contains&txtStationName=herschel+island
# Per: https://climate.weather.gc.ca/glossary_e.html 
# "Maximum wind or gust is displayed in the "speed of maximum gust" column when greater than 30 km/h. The stations do not report if less than 31 km/h."
#------------------------------

library(tidyverse)
library(cowplot)

data <- read_csv("data/raw/herschel_climate_station_2024/en_climate_daily_YT_2100636_2024_P1D.csv")

# windspeed <- data %>% 
#   select(c("Longitude (x)", "Latitude (y)", "Station Name", "Date/Time","Month","Day", "Spd of Max Gust (km/h)", "Dir of Max Gust (10s deg)"))
windspeed <- data %>%
  select(
    longitude = `Longitude (x)`,
    latitude = `Latitude (y)`,
    station_name = `Station Name`,
    date = `Date/Time`,
    month = Month,
    day = Day,
    max_gust_speed_kmh = `Spd of Max Gust (km/h)`,
    max_gust_dir_10deg = `Dir of Max Gust (10s deg)`
  ) %>%
  mutate(
    day = as.numeric(day),
    month = as.numeric(month),
    max_gust_speed_kmh = as.numeric(max_gust_speed_kmh),
    is_missing = is.na(max_gust_speed_kmh) # Create a flag for missing data
  ) %>%
  filter(month >= 5 & month <= 8)


# Plot windspeed over time with missing data highlighted
ggplot(windspeed, aes(x = date, y = max_gust_speed_kmh)) +
  geom_line(color = "blue", na.rm = TRUE) + # Line for available data
  geom_point(aes(color = is_missing), size = 2) + # Points, colored by missingness
  scale_color_manual(
    values = c("FALSE" = "black", "TRUE" = "red"),
    labels = c("FALSE" = "Available", "TRUE" = "Missing"),
    name = "Data Availability"
  ) +
  labs(
    title = "Max Daily Gust at Herschel Climate Station in 2024",
    x = "Date",
    y = "Max Gust Speed (km/h)"
  ) +
  theme_minimal()


missing_data <- windspeed %>%
  filter(is.na(max_gust_speed_kmh))

# Plot windspeed over time with missing data indicated
ggplot(windspeed, aes(x = date, y = max_gust_speed_kmh)) +
  geom_line(color = "blue", na.rm = TRUE, size = 2) + # Line for available data
  geom_point(data = missing_data, aes(x = date, y = 0), color = "red", size = 4, shape = 4) + # Mark missing dates
  geom_hline(yintercept = 31, linetype = "dashed", color = "black") +
  annotate(
    "text", x = max(windspeed$date), y = 20, label = "31 km/h", 
    vjust = -0.8, hjust = 7.4, color = "black", size = 5
  ) +
  labs(
    title = "Max Daily Gust at Herschel Climate Station in 2024",
    x = "Date",
    y = "Max Gust Speed (km/h)"
  ) +
  theme_minimal() +
  theme_half_open(font_size = 14)




