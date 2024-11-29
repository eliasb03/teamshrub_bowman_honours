#------------------------------
# teamshrub_bowman_honours
# 0X_BBS_guilds_v0
# By: Elias Bowman 
# Created: 2024-11-17
#
# Description: This script will summarize the Breeding Bird Data into guild levels
# Should only be run after BBS_tidy
#------------------------------
# Preparing packages and data ####
# Importing data
bbs.summary <- read.csv("data/clean/bbs/bbs_yearly_summary.csv")
species.list <- read.csv("data/clean/bbs/species_list.csv")
species.mapping <- read.csv("data/clean/bbs/species_code_mapping.csv")
guild.mapping <- read.csv("data/raw/bird_guild_mapping.csv")

# Loading Packages
library(tidyverse)  # For dplyr, tidyr, etc.
library(rlang)      # For tidy evaluation tools
library(ggplot2)
library(cowplot)

# Creating relevant functions ####
# Function to match species codes to species names
apply_species_names <- function(data, species.mapping) {
  # Perform a left join to add species names based on species codes
  if ("species" %in% colnames(data)) {
    filled_data <- data %>%
      left_join(species.mapping, by = "spec.code") %>%
      mutate(species = coalesce(species.x, species.y)) %>%  # Resolve species conflicts
      select(-species.x, -species.y)  # Remove the extra columns
  } else {
    filled_data <- data %>%
      left_join(species.mapping, by = "spec.code")
  }
  
  return(filled_data)
}

# Function to left_join guild mapping data into bbs.data
apply_guilds <- function(data, guild.mapping) {
  
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
  
  if ("guild.x" %in% colnames(data)) {
    data <- data %>%
      mutate(guild = coalesce(guild.x, guild.y)) %>%  # Resolve guild conflicts
      select(-guild.x, -guild.y) %>%  # Remove the extra columns 
      mutate(guild2 = coalesce(guild2.x, guild2.y)) %>%  # Resolve guild conflicts
      select(-guild2.x, -guild2.y)
  }
  
  return(data)
}

# Function to reformat applied and filled columns
reformat_columns <- function(data) {
  # Reformat the columns to be more readable
  data <- data %>%
    select(spec.code, species, year, total.count, guild, guild2, observers, effort.multiplier, sampling.effort, sampling.time, num.observers, survey.id, filled.in)
  
  return(data)
}

# Function for filling in missing years with 0s
fill_missing_years <- function(data, species_col, year_col, count_col) {
  # Extract unique species and years
  all_species <- unique(data[[species_col]])
  all_years <- unique(data[[year_col]])
  
  # Create a complete grid of species and years
  complete_data <- expand.grid(
    species = all_species,
    year = all_years
  ) %>%
    rename(
      !!species_col := species, 
      !!year_col := year
    )
  
  # Merge with the original data and fill missing values
  filled_data <- complete_data %>%
    left_join(data, by = c(species_col, year_col)) %>%
    mutate(
      filled.in = is.na(.data[[count_col]]), # TRUE if the count was filled
      !!count_col := ifelse(filled.in, 0, .data[[count_col]]) # Fill with 0 if missing
    )
  
  return(filled_data)
}

# Function to create scaled.total by effort multiplier
create_scaled_total <- function(data) {
  data <- data %>%
    mutate(total.scaled = ifelse(!is.na(effort.multiplier), total.count * effort.multiplier, 0))
  
  return(data)
}

# Filling in missing years with 0s ####
bbs.summary <- fill_missing_years(
  data = bbs.summary, 
  species_col = "spec.code", 
  year_col = "year", 
  count_col = "total.count"
) %>%
  apply_species_names(species.mapping) %>%
  apply_guilds(guild.mapping) %>%
  reformat_columns()
         
####


# Creating Yearly Relative Abundance Metric ####
bbs.summary <- bbs.summary %>%
  group_by(spec.code) %>%
  mutate(species.max = max(total.count)) %>%
  ungroup() %>%
  group_by(year, spec.code) %>%
  mutate(yearly.rel.abundance = total.count / species.max) %>%
  ungroup()


bbs.summary <- bbs.summary %>%
  create_scaled_total() %>%
  group_by(spec.code) %>%
  mutate(species.scaled.max = max(total.scaled)) %>%
  ungroup() %>%
  group_by(year, spec.code) %>%
  mutate(yearly.rel.abundance.scaled = total.scaled / species.scaled.max) %>%
  ungroup()

high.threshold <- 0.5

bbs.summary <- bbs.summary %>%
  mutate(logistic.abundance.scaled = ifelse(yearly.rel.abundance.scaled >= high.threshold, 1, 0)) %>%
  mutate(logistic.abundance = ifelse(yearly.rel.abundance >= high.threshold, 1, 0))


# Summarize to guild level



# Filtering by Cameron-Approved-Species List ####
bbs.summary.f <- filter(bbs.summary, spec.code %in% species.list$spec.code)

#################################################

ggplot(bbs.summary.f, aes(x = year, y = yearly.rel.abundance, color = guild)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = lm) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_half_open(font_size = 14)

ggplot(filter(bbs.summary.f, guild == "shorebird"), aes(x = year, y = yearly.rel.abundance, color = guild)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = lm, linewidth = 1) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_half_open(font_size = 14)

ggplot(filter(bbs.summary, species == "Semipalmated Sandpiper"), aes(x = year, y = yearly.rel.abundance)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = lm, linewidth = 1) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_half_open(font_size = 14)

simple.bbs <- bbs.summary.f %>%
  select(year, spec.code, species, guild, yearly.rel.abundance)

# probably need a species specific relative abundance or guild specific - or after filtering maybe... hmmmmmmm

# Creating Guild Level Summary




# 
# # View the filtered data
# head(filtered_bbs_summary)
# 
# 
# ###############
# # Create top species versions
# top_spec_num <- 15
# bbs.long.spec.top <- select_top_species(bbs.long, top_spec_num)
# 
# 
# 
# # removing unecessary objects from the environment
# rm(top_spec_num)

