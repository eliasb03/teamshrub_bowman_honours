#################
# 02_climateNA_v0
#
# Very early and unformatted testing of the climate NA package
# Also just getting used to R again
# 
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
#################
# importing packages
library(tidyverse)
library(dplyr)

# Climate NA

