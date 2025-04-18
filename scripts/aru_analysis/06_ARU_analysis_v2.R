#------------------------------
# teamshrub_bowman_honours
# 06_ARU_analysis_v2
# By: Elias Bowman
# Created: 
#
# Description: This script runs statistical analyses on the outputs from the BirdNET models
#------------------------------

# Load required libraries
library(brms)
library(tidyverse)
library(tidybayes)

# Set confidence threshold and temperature palette
confidence_threshold <- 0.5

# Read in data
aru_dataframe <- read_csv(paste0("data/clean/aru/aru_analysis_data_conf",
                                 confidence_threshold, ".csv"))
ARU_average_temps <- read_csv("data/clean/aru/tomst_averages.csv")

# Specify species to analyze
species_list <- c("Lapland Longspur", 
                  "Semipalmated Plover", 
                  "Red-throated Loon")

# Summarize ARU data to the daily level and merge with temperature data
aru_daily <- aru_dataframe %>%
  mutate(date = as.Date(time_interval)) %>%
  group_by(locationID, date, common_name) %>%
  summarize(total_count = sum(species_count), .groups = "drop") %>%
  left_join(ARU_average_temps, by = c("locationID" = "aru_name")) %>%
  mutate(temp_binary = as.factor(temp_binary))

# Define analysis period
period_length <- 30          # Number of days in the period
start <- as.Date("2024-06-25")
total_range <- c(start, start + period_length)


# Filter continuous data by date range and calculate day-from-start
aru_cont <- aru_daily %>%
  filter(date >= total_range[1] & date <= total_range[2]) %>%
  mutate(day_from_start = as.numeric(date - total_range[1]))


# Create a summary dataset for modeling
aru_summary <- aru_cont %>%
  select(locationID, date, day_from_start, common_name, total_count, temp_binary, avg_temp, tomst_num) %>%
  mutate(
    locationID = as.factor(locationID),
    date = as.factor(date),
    log_count = log(total_count)
  ) %>%
  filter(!locationID %in% c("ARUQ18", # died very shortly after deployment
                            "ARUQ8", # microphone broke
                            "ARUQ17" # not deployed until july 2nd, full week late, only covers like 77 percent of study period
                            ))

# Split the dataset by species for separate models
passerine <- aru_summary %>% filter(common_name == "Lapland Longspur")
shorebird <- aru_summary %>% filter(common_name == "Semipalmated Plover")
waterbird <- aru_summary %>% filter(common_name == "Red-throated Loon")

# Define priors for the models
aru_priors <- c(
  set_prior("normal(0, 4.6)", class = "b", coef = "day_from_start"),
  set_prior("normal(0, 4.6)", class = "b", coef = "temp_binarylow"),
  set_prior("normal(0, 4.6)", class = "b", coef = "day_from_start:temp_binarylow"),
  set_prior("normal(2, 6)", class = "Intercept")
)

# Fit all of the models
# Lapland Longspur (Passerine)
passerine_model <- brm(
  total_count ~ day_from_start * temp_binary + (1 | locationID) + (1 | date),
  data = passerine,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

# Semipalmated Plover (Shorebird)
shorebird_model <- brm(
  total_count ~ day_from_start * temp_binary + (1 | locationID) + (1 | date),
  data = shorebird,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

# Red-throated Loon (Waterbird)
waterbird_model <- brm(
  total_count ~ day_from_start * temp_binary + (1 | locationID) + (1 | date),
  data = waterbird,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)


# Optionally, view summaries or perform posterior predictive checks
summary(passerine_model)
summary(shorebird_model)
summary(waterbird_model)
# pp_check(passerine_model, resp = "total_count", ndraws = 1000)
# pp_check(shorebird_model, resp = "total_count", ndraws = 1000)
# pp_check(waterbird_model, resp = "total_count", ndraws = 1000)

# Save model outputs
model_output_path <- "outputs/aru/models/calling/"
dir.create(model_output_path, recursive = TRUE, showWarnings = FALSE)
saveRDS(passerine_model, file = paste0(model_output_path, "passerine.rds"))
saveRDS(shorebird_model, file = paste0(model_output_path, "shorebird.rds"))
saveRDS(waterbird_model, file = paste0(model_output_path, "waterbird_redthroatedloon.rds"))

# Save key datasets used for modeling
data_output_path <- "data/clean/aru/"
dir.create(data_output_path, recursive = TRUE, showWarnings = FALSE)
write_csv(aru_daily, file = paste0(data_output_path, "aru_daily.csv"))
write_csv(aru_summary, file = paste0(data_output_path, "aru_summary.csv"))

