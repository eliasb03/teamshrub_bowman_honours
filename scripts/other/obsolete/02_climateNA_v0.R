#------------------------------
# teamshrub_bowman_honours
# 0X_climateNA_v0
# By: Elias Bowman 
# 2024-07-03
# Description: This script will implement the use of the ClimateNA package and tool to import relevant grid based climate data at the local level. Depending on use I will either have hyperspecific points to each data source or I will have kind of a generalized regional temp.
# 
# At the moment this is very early and preliminary 
#####
# Mission is to use and implement climate NA to download time sereis climate data from Qikiqtaruk
# clean and make this data workable
# then organize the data and order years from warmest to coldest june temperatures
# make a list of years within the dataset form warmest to coldest
# this is a proxy for earliest and latest springs
#
# Standards used ###
# generic coordiante point for qikiqtaruk: 
# time series to get yearly data from: 19XX-20XX
# How do you determine "monthly temps"
# - either use monthly data from climate na
# - instead consider calculating the data from daily or hourly data
#------------------------------

# importing packages
library(tidyverse)
library(dplyr)

# Climate NA ####


