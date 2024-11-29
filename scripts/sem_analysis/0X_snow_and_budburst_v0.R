#------------------------------
# teamshrub_bowman_honours
# 0X_snow_and_budburst_v0
# By: Elias Bowman 
# Created: 2024-11-17
#
# Description: This script will summarize the Breeding Bird Data into guild levels
# Should only be run after BBS_tidy
#------------------------------

# treat it as 3 locations, with 20 points
# average within locations
# average across transects
# get variance to go with each average

library(stats)



# Phenology
qiphen <- read.csv("data/raw/qiki_phen_with_before_2024_partial.csv", stringsAsFactors = F)

snowmelt <- qiphen %>% select(Year, Spp, Plot.ID, P1) %>% na.omit()
SALARC_budburst <- qiphen %>% filter(Spp == "SALARC") %>% select(Year, Plot.ID, P2) %>% na.omit()

SALARC_budburst_avg <- SALARC_budburst %>% na.omit() %>% group_by(Year) %>% summarise(P2_mean = mean(P2), P2_sd = sd(P2))

combine_sd <- function(sds) {
  sd_val <- sqrt(sum(sds^2) / length(sds))
  return(sd_val)
}

snowmelt_avg <- snowmelt %>% na.omit() %>% group_by(Year, Spp) %>% summarise(P1_mean = mean(P1), P1_sd = sd(P1)) %>% group_by(Year) %>% summarise(P1_mean = mean(P1_mean), P1_sd = combine_sd(P1_sd))


