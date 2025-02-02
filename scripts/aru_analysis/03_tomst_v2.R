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
tsd_short[, datetime := ymd_hms(datetime)] ## :=  creates or update a column in data.table, here we switch to a lubridate format with ymd
######### ISSUE HERE WHERE TIMES WITH 00:00:00 as the hms don't parse as lubridate objects
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

rm(tms.f, tms.calc, files_table)
rm(tsd_short)
