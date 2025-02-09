#------------------------------
# teamshrub_bowman_honours
# 0X_tomst_v1
# By: Jeremy, modified by Elias
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

# aru.tomst <- tomst.mapping %>%
#   mutate(locality_id = paste0("TOMST", tomst_num, "_QHI")) %>%
#   left_join(month.tomst.data, by = "locality_id") %>%
#   filter(month == "7") %>%
#   mutate(temp_hl = ifelse(mean_value > median(mean_value), "high", "low")) %>%
#   mutate(temp_hml = case_when(
#     mean_value <= quantile(mean_value, 1/3) ~ "low",
#     mean_value <= quantile(mean_value, 2/3) ~ "med",
#     TRUE ~ "high"
#   ))   #mutate(temp_hl = ifelse(mean_value > 9.75, "cool", "warm"))
# 
# 



##############################
# Aggregate to daily/monthly averages
## aggregates all those sensors to monthly values # choose between minimu percentile
monthly.tms <- mc_agg(tms.calc,fun=c("mean","min","max"),period = "day",min_coverage=1,use_utc = F)
monthly.tms <- mc_agg(tms.calc,fun=c("mean","percentile"),percentiles = c(0.05,0.95),period = "day",min_coverage=1,use_utc = F)


## export the object out of the MC framework
export_dt <- data.table(mc_reshape_long(monthly.tms))
export_dt[, serial_number:=NULL] ##removing useless col
export_dt[, datetime := ymd(datetime)] ## :=  creates or update a column in data.table, here we swith to a lubridate format with ymd
export_dt[, month := month(datetime)] ## extracting the month
export_dt[, day := day(datetime)] ## extracting the day
export_dt[, week := week(datetime)] ## extracting the week
export_dt[, year := year(datetime)] ## extracting the year
export_dt[, tomst_num := str_extract(locality_id, "(?<=TOMST)\\d+")] # Adding a tomst_num column

export_dt <- export_dt[year == 2024,]# filter for only 2024



# Summarize by 10 minute stretches from 0:00-0:10 and from 0:30-0:40




# filter for monthly averages
monthly_values <- export_dt[,.(mean_value = mean(value,na.rm=T)),
                              by=.(month, sensor_name, locality_id, tomst_num)] # na.rm=T remove incomplete months 
# filter for daily averages
daily_values <- export_dt[,.(mean_value = mean(value,na.rm=T)),
                          by=.(month,day,sensor_name,height,week, locality_id, tomst_num)] # na.rm=T remove incomplete days 

# filter for weekly averages
weekly_values <- export_dt[,.(mean_value = mean(value,na.rm=T)),
                           by=.(month,sensor_name,height,week, locality_id, tomst_num)] # na.rm=T remove incomplete days 

# Tomsts 1:40 exist, missing/unreliable data from tomsts 2, 5, 11, 14
#setdiff(1:40, monthly_values$tomst_num)


month.tomst.data <- monthly_values %>%
  filter(sensor_name == "TMS_T3_mean") %>%
  filter(month %in% c("6", "7"))

daily.tomst.data <- daily_values %>%
  filter(sensor_name == "TMS_T3_mean") %>%
  filter(month %in% c("6", "7")) %>%
  mutate(date = ymd(paste0("2024-", month, "-", day)))

# Data manually built for matching ARU to TOMST
tomst.mapping <- read.csv("data/raw/aru_tomst_mapping.csv") 
tomst.of.interest <- c(1,  2,  3,  4,  7,  9, 11, 13, 14, 15, 16, 17, 20, 27, 29, 33, 35, 39, 40)

aru.tomst <- tomst.mapping %>%
  mutate(locality_id = paste0("TOMST", tomst_num, "_QHI")) %>%
  left_join(month.tomst.data, by = "locality_id") %>%
  filter(month == "7") %>%
  mutate(temp_hl = ifelse(mean_value > median(mean_value), "high", "low")) %>%
  mutate(temp_hml = case_when(
    mean_value <= quantile(mean_value, 1/3) ~ "low",
    mean_value <= quantile(mean_value, 2/3) ~ "med",
    TRUE ~ "high"
  ))   #mutate(temp_hl = ifelse(mean_value > 9.75, "cool", "warm"))


# # plot tomst.mapping, tms_3_mean vs ARU
ggplot(aru.tomst, aes(x = aru_name, y = mean_value, color = temp_hml)) +
  geom_boxplot() +
  #geom_point(size = 2) +
  labs(x = "ARU",
       y = "Average Air Temp") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))

