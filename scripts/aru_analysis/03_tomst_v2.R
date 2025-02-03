#------------------------------
# teamshrub_bowman_honours
# 0X_tomst_v2
# By: Jeremy's original script, converted by Elias
#
# Description: This script will read TOMST data and calculate relevant values
#------------------------------

#### Import packages ####
library(myClim) ## logger data reading
library(foreach) ## efficient loop
library(data.table) ## efficient data.frame  
library(stringr) ## efficient character manipulation
library(lubridate) ## manipulate date_time
library(tidyverse) ## data manipulation

tomst_file_path <- "D:/QHI_TOMST_2024/clean/QHI_TOMST_2024_fixed"

#### Creating a meta_data file ####
## list.file(, full.names = T) get thew hole path 
list_path <- list.files(tomst_file_path,full.names = T)
list_files <- list.files(tomst_file_path,full.names = F)
locality_name <-  str_extract(list_files, "TOMST[:digit:]+_QHI") ## other way of naming it, if we have a metada file for example

files_table <- data.table(path = list_path,
                          locality_id = locality_name ,  
                          data_format = "TOMST")

locality_metadata <-  data.table(locality_id = locality_name ,  
                                 tz_offset  = -7)
##################### Can add gps coordinates as metadata

#### Reading TOMST data with MyClim ####
## Read pre-defined loggers without metadata
# read from Tomst files

tms.f <- mc_read_data(files_table,locality_metadata)

mc_info_count(tms.f) #which returns the number of localities, loggers and sensors in myClim object
mc_info(tms.f)# returning data frame with summary per sensor
mc_info_meta(tms.f)# returning the data frame with locality metadata
mc_info_clean(tms.f) #returning the data frame with cleaning log

#mc_plot_raster(tms.f)  # TMOST14_QHI and TOMST8_QHI seem unreliable
tms.f <- mc_filter(tms.f,localities = c("TOMST14_QHI","TOMST8_QHI"),reverse = T )

mc_plot_line(mc_filter(tms.f, localities = "TOMST9_QHI")) 

tms.f <- mc_prep_crop(tms.f,start = as.POSIXct("2022-07-31", tz="UTC")) # croping the TS to when they were installed (I'm guessing a date with time series here)

## calculate virtual sensor VWC from raw TMS moisture signal

## let's check the most appropriate  soiltype, default to universal now
tms.calc <- mc_calc_vwc(tms.f, soiltype = "universal")

## virtual sensor with growing and freezing degree days
tms.calc <- mc_calc_gdd(tms.calc, sensor = "TMS_T3",)
tms.calc <- mc_calc_fdd(tms.calc, sensor = "TMS_T3")

## virtual sensor to estimate snow presence from 2 cm air temperature 
tms.calc <- mc_calc_snow(tms.calc, sensor = "TMS_T2")

# import tomst mapping
tomst.mapping <- read.csv("data/raw/aru_tomst_mapping.csv") 

# make list of tomsts that pair to arus, and are thus of interest
tomst.of.interest <- c(1,  2,  3,  4,  7,  9, 11, 13, 14, 15, 16, 17, 20, 27, 29, 33, 35, 39, 40)
tomst.of.interest.names <- paste0("TOMST", tomst.of.interest, "_QHI") # create strings of the full tomst of interest names for comparison

tsd_short <- data.table(mc_reshape_long(tms.calc)) # export time series data
tsd_short[, serial_number:=NULL] # remove useless serial_number column
tsd_short <- tsd_short[locality_id %in% tomst.of.interest.names] # filter for only tomsts of interest
# filter for only May to August 2024
start.date <- as.POSIXct("2024-05-01 00:00:00", tz = "UTC")
end.date <- as.POSIXct("2024-08-31 23:59:59", tz = "UTC")
tsd_short <- tsd_short[datetime >= start.date & datetime <= end.date] 
# choosing only sensros I want
tsd_short <- tsd_short[sensor_name %in% c("TMS_T3", "snow")]
tsd_short[, tomst_num := str_extract(locality_id, "(?<=TOMST)\\d+")]
tsd_short[, datetime := ymd_hms(datetime, truncated=3)] ## :=  creates or update a column in data.table, here we switch to a lubridate format with ymd
# Note the truncated=3 argument in ymd_hms, which resolves an issue in ymd_hms, handling midnight (00:00:00) records
# Change the timezone of datetime from UTC to UTC-7 (e.g., Pacific Time)
tsd_short[, datetime := with_tz(datetime, tzone = "Etc/GMT+7")]
tsd_short[, time_to := with_tz(time_to, tzone = "Etc/GMT+7")]
tsd_short[, month := month(datetime)] ## extracting the month
tsd_short[, day := day(datetime)] ## extracting the day
tsd_short[, week := week(datetime)] ## extracting the week
tsd_short[, year := year(datetime)] ## extracting the year


tomst.mapping <- tomst.mapping %>%
  select(aru_name, tomst_num) %>%
  mutate(tomst_num = as.character(tomst_num))

# join aru id with tomst
tomst_data <- merge(tsd_short, tomst.mapping, by = "tomst_num", all.y = TRUE, allow.cartesian=TRUE)

# Creating simple version to join with ARU data
tomst_temp <- tomst_data %>%
  filter(sensor_name == "TMS_T3") %>%
  select(datetime, value, aru_name, tomst_num)

tomst_snow <- tomst_data %>%
  filter(sensor_name == "snow") %>%
  select(datetime, value, aru_name, tomst_num)

rm(tms.f, tms.calc, files_table)
rm(tsd_short)


###########
# Assuming tomst_temp and tomst_snow are your data.tables
# Convert them to data.frames if necessary
tomst_temp2 <- as.data.frame(tomst_temp)
tomst_snow2 <- as.data.frame(tomst_snow)

# Generate plots for each unique tomst_num
plots <- lapply(unique(tomst_temp2$tomst_num), function(tomst) {
  # Filter the data for the current tomst_num
  temp_data <- tomst_temp2[tomst_temp2$tomst_num == tomst, ]
  snow_data <- tomst_snow2[tomst_snow2$tomst_num == tomst, ]
  
  # Find the temperature y-axis limits for the current tomst_num
  temp_limits <- range(temp_data$value, na.rm = TRUE)
  
  # Create the plot
  p <- ggplot(temp_data, aes(x = datetime, y = value)) +
    geom_line(color = "blue") +  # Plot the temperature value
    # Overlay snow values as points at the top (1) and bottom (0) of the y-axis scale
    geom_point(data = snow_data, aes(x = datetime, y = value * (temp_limits[2] - temp_limits[1]) + temp_limits[1]), 
               color = "red", shape = 16) +  # Adjust snow y-axis position
    scale_y_continuous(
      limits = temp_limits,
      sec.axis = sec_axis(~ . - temp_limits[1], name = "Snow (0/1)")  # Secondary axis for snow
    ) +
    labs(title = paste("Tomst Number:", tomst),
         x = "Datetime",
         y = "Temperature Value") +
    theme_minimal()
  
  return(p)
})

# Wrap the plots into a grid using cowplot
plot_grid(plotlist = plots, ncol = 4)  # Adjust ncol as needed
