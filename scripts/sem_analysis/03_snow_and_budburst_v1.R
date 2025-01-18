#------------------------------
# teamshrub_bowman_honours
# 03_snow_and_budburst_v1
# By: Elias Bowman
# Created: 2024-11-17
# Last Updated: 2025-01-13
#
# Description: This script imports data on snowmelt and budburst from the Qikiqtaruk Ecological Monitoring Program and processes that data to a simple dataframe.
#------------------------------
# Analysis approach:
# 3 locations, 20 points
# Average within locations
# Average across transects
# Keep variance for each average

# Import Packages ####
library(stats)


# Import Data ####
# Phenology
qiphen_path <- "data/raw/qiki_phen_with_before_2024_partial.csv"
qiphen <- read.csv(qiphen_path,
                   stringsAsFactors = F)


# Function to create standard deviations
combine_sd <- function(sds) {
  sd_val <- sqrt(sum(sds ^ 2) / length(sds))
  return(sd_val)
}

# Process snowmelt data
snowmelt <- qiphen %>%
  select(Year, Spp, Plot.ID, P1) %>%
  na.omit()

# Calculate snowmelt averages
snowmelt_avg <- snowmelt %>%
  na.omit() %>%
  group_by(Year, Spp) %>%
  summarise(P1_mean = mean(P1), P1_sd = sd(P1)) %>%
  group_by(Year) %>%
  summarise(P1_mean = mean(P1_mean), P1_sd = combine_sd(P1_sd))

# Process salix arctica budburst data
SALARC_budburst <- qiphen %>% filter(Spp == "SALARC") %>%
  select(Year, Plot.ID, P2) %>%
  na.omit()

# Calculate budburst averages
SALARC_budburst_avg <- SALARC_budburst %>%
  na.omit() %>%
  group_by(Year) %>%
  summarise(P2_mean = mean(P2), P2_sd = sd(P2))

# Create a single table with both datasets
qhi_phenology <- merge(snowmelt_avg, SALARC_budburst_avg, by = "Year") %>%
  rename(
    snowmelt_mean = P1_mean,
    snowmelt_sd = P1_sd,
    budburst_mean = P2_mean,
    budburst_sd = P2_sd,
    year = Year
  ) # rename p1 to snowmelt, p2 to budburst

# Saving qhi_phenology
output_path <- "data/clean/sem/" # Output path
write_csv(qhi_phenology, paste0(output_path, "phenology_data.csv"))

# Close unused data
rm(qiphen,
   SALARC_budburst,
   SALARC_budburst_avg,
   snowmelt,
   snowmelt_avg)
rm(combine_sd)

