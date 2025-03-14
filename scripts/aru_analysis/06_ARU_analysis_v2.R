

# ARU many species analysis script
# Elias Bowman
# 2025-03-14

# Loading packages ####
library(brms)
library(tidyverse)

# Loading data ####
confidence_threshold <- 0.5 # Confidence threshold used by BirdNET in the output I want

aru_dataframe <- # Read in ARU Data
  read_csv(paste0(
    "data/clean/aru/aru_analysis_data",
    "_conf",
    confidence_threshold,
    ".csv"
  ))

ARU_average_temps <- # Read in in ARU Average Temperatures
  read_csv("data/clean/aru/tomst_averages.csv")

# Summarize ARU data to daily level
aru_daily <- aru_dataframe %>%
  mutate(date = as.Date(time_interval)) %>%
  group_by(locationID, date, common_name) %>%
  summarize(total_count = sum(species_count)) %>%
  ungroup() %>%
  left_join(ARU_average_temps, by = c("locationID" = "aru_name")) %>%
  mutate(temp_binary = as.factor(temp_binary))


# Rejigging aru analaysis dataframe to include guilds and species codes ####
# Expanded species list
species.list.exp <- data.frame(
  common_name = c(
    "Common Eider",
    "Semipalmated Plover",
    "Semipalmated Sandpiper",
    "Baird's Sandpiper",
    #"Red-necked Phalarope",
    "Glaucous Gull",
    "Lapland Longspur",
    "Snow Bunting",
    "Savannah Sparrow",
    "Redpoll",
    "Greater White-fronted Goose",
    "Northern Pintail",
    "Red-throated Loon",
    "Long-tailed Jaeger",
    "Black Guillemot",
    "American Pipit"
  ),
  spec.code = c(
    "COEI",
    "SEPL",
    "SESA",
    "BASA",
    #"RNPL",
    "GLGU",
    "LALO",
    "SNBU",
    "SAVS",
    "REDP",
    "GWFG",
    "NOPI",
    "RTLO",
    "LTJA",
    "BLGU",
    "AMPI"
  )
)

apply_spec_code <- function(data, species.list = species.list.exp) {
  if ("spec.code" %in% colnames(data)) {
    data <- data %>%
      select(-spec.code)  # Remove exisiting spec.code
  }  
  
  
  data <- data %>%
    left_join(species.list, by = "common_name") #%>%
  #mutate(species = coalesce(species.x, species.y)) %>%  # Resolve species conflicts
  #select(-species.x, -species.y)  # Remove the extra columns
  
  return(data)
}



#guild.mapping.path <- "D:/bird_guild_mapping.csv"
guild.mapping.path <- "data/raw/bird_guild_mapping.csv"
guild.mapping <- read.csv(guild.mapping.path)

# Function to left_join guild mapping data into bbs.data
apply_guilds <- function(data, guild.mapping) {
  
  if ("guild" %in% colnames(data) & "guild2" %in% colnames(data)) {
    data <- data %>%
      select(-guild, -guild2)  # Remove exisiting 'guild1' and 'guild2'
  }  
  
  # Check if 'species' column already exists in the data
  if ("species" %in% colnames(data)) {
    # If species exists, just join and resolve any conflicts
    data <- data %>%
      left_join(guild.mapping, by = "spec.code") %>%
      mutate(species = coalesce(species.x, species.y)) %>%  # Resolve species conflicts
      select(-species.x, -species.y)  # Remove the extra columns
  } else {
    # If species doesn't exist, directly add the species column from guild.mapping
    data <- data %>%
      left_join(guild.mapping, by = "spec.code")
  }
  
  return(data)
}



# Adding guilds and species codes
aru_daily <- aru_daily %>%
  apply_spec_code() %>%
  apply_guilds(guild.mapping)


# Restricting to Period of Interest ####
period_length <- 30 # 30 days
start <- as.Date("2024-06-25") # Early time period
total_range <- c(start, start + period_length)

# Continuous data
aru_cont <- aru_daily %>%
  filter(date >= total_range[1] & date <= total_range[2]) %>%
  mutate(
    day_from_start = as.numeric(date - total_range[1]), 
  )

# Creating a summary dataset ready for analysis ####
aru_summary <- aru_cont %>%
  select(
    locationID,
    date,
    day_from_start,
    common_name,
    guild,
    total_count,
    temp_binary,
    avg_temp,
    tomst_num
  ) %>%
  mutate(
    locationID = as.factor(locationID),
    date = as.factor(date),
    scaled_count = scale(total_count, center = TRUE, scale = TRUE),
    log_count = log(total_count) # Log transform total count, choosing not to +1
  )

# Filtering by taxa ####
passerine <- aru_summary %>%
  filter(guild == "passerine")
shorebird <- aru_summary %>%
  filter(guild == "shorebird")
waterbird <- aru_summary %>%
  filter(guild == "waterbird")


# Running models ####
# Setting model priors
aru_priors <- c(
  set_prior("normal(0, 4.6)", class = "b", coef = "day_from_start"),  # Prior for day_from_start
  # max is roughly 1000, over a 16 day inerval about (1000/16) = ~60, say 100 more or less calls a day, log(100) = 4.6
  set_prior("normal(0, 4.6)", class = "b", coef = "temp_binarylow"),  # Prior for temp_binarylow
  set_prior("normal(0, 4.6)", class = "b", coef = "day_from_start:temp_binarylow")  # Prior for temp interaction term
  #set_prior("normal(0, 5)", class = "Intercept")  # Prior for the intercept
)

passerine_model <- brm(
  total_count ~ day_from_start * temp_binary + (1|common_name) + (1|locationID) + (1|date),
  data = passerine,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

waterbird_model <- brm(
  total_count ~ day_from_start * temp_binary + (1|common_name) + (1|locationID) + (1|date),
  data = waterbird,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

shorebird_model <- brm(
  total_count ~ day_from_start * temp_binary + (1|common_name) + (1|locationID) + (1|date),
  data = shorebird,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

model_output_path <- "outputs/aru/calling/"
saveRDS(passerine_model, file = paste0(model_output_path, "passerine_ext.rds"))
saveRDS(waterbird_model, file = paste0(model_output_path, "waterbird_ext.rds"))
saveRDS(shorebird_model, file = paste0(model_output_path, "shorebird_ext.rds"))


