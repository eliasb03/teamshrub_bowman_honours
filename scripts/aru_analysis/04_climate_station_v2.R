#------------------------------
# teamshrub_bowman_honours
# 04_climate_station_v2
# By: Elias Bowman 
# Created: 2025-01-27
#
# Description: This script imports climate station data from Herschel Island Yukon Territory Climate Station
# Climate ID: 2100636
# https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=1994-02-01%7C2025-01-27&dlyRange=1974-07-01%7C2025-01-27&mlyRange=1974-01-01%7C2007-01-01&StationID=1560&Prov=YT&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2024&selRowPerPage=25&Line=0&searchMethod=contains&txtStationName=herschel+island&timeframe=1&time=UTC&time=UTC&Year=2024&Month=5&Day=27#
# Using dailyhourly values, we can create hourly windspeed data for the duration of the summer
# Using zoo.approx, we use linear interpolation to fill in data to 30 minute intervals to match ARU data
#------------------------------

library(tidyverse)
library(zoo)

library(cowplot)

# Import data
data.dir <- "data/raw/herschel_climate_station_2024"
file.list <- list.files(data.dir, full.names = TRUE)
hourly.files <- grep("hourly", file.list, value = TRUE)

combined_hourly_data <- hourly.files %>%
  map_dfr(read_csv)

windspeed <- combined_hourly_data %>%
  select(
    longitude = `Longitude (x)`,
    latitude = `Latitude (y)`,
    station_name = `Station Name`,
    datetime_utc = `Date/Time (UTC)`,
    month = Month,
    day = Day,
    wind_speed_kmh = `Wind Spd (km/h)`,
    win_dir_10deg = `Wind Dir (10s deg)`
  ) %>%
  mutate(
    datetime_utc = as.POSIXct(datetime_utc, tz = "UTC"), # Ensure date is in UTC
    datetime_whitehorse = with_tz(datetime_utc, tzone = "America/Whitehorse"), # Convert to Whitehorse timezone
    day = as.numeric(day),
    month = as.numeric(month),
    wind_speed_kmh = as.numeric(wind_speed_kmh),
    is_missing = is.na(wind_speed_kmh) # Create a flag for missing data
  ) %>%
  filter(month >= 5 & month <= 8) # Filter for months May to August


# Add windspeed data
full_time_wind <- tibble(datetime_whitehorse = seq(
  min(windspeed$datetime_whitehorse),
  max(windspeed$datetime_whitehorse),
  by = "30 mins"
))

windspeed.filled <- windspeed %>%
  right_join(full_time_wind, by = "datetime_whitehorse") %>%
  # Step 1: Create an 'infilled' column (FALSE for original, TRUE for infilled, handle case where data is missing by leaving FALSE)
  mutate(infilled = case_when(
    is_missing ~ FALSE,               # If is_missing is TRUE, set infilled to FALSE
    is.na(wind_speed_kmh) ~ TRUE,     # If wind_speed_kmh is NA, set infilled to TRUE
    !(is.na(wind_speed_kmh)) ~ FALSE,     # If wind_speed_kmh is NA, set infilled to TRUE
  )) %>%
  # Step 2: Oorder by time
  arrange(datetime_whitehorse) %>%
  # Step 3: Fill missing values using linear interpolation 
  mutate(wind_speed_kmh = ifelse((infilled == TRUE), 
                                 zoo::na.approx(wind_speed_kmh, na.rm = FALSE), 
                                 wind_speed_kmh)) %>%
  # Step 4: Select only date time and winspeed
  select(datetime_whitehorse, wind_speed_kmh)

# Write windspeed to data/clean/aru folder
write_csv(windspeed.filled, "data/clean/aru/windspeed_filled.csv")

# import windspeed data from folder
windspeed.filled <- read_csv("data/clean/aru/windspeed_filled.csv")

rm(combined_hourly_data, full_time_wind)
