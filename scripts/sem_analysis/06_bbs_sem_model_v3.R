#------------------------------
# teamshrub_bowman_honours
# 06_bbs_sem_model_v3
# By: Elias Bowman 
# Created: 2024-10-16
# Last update: 2025-02-09
# 
# Description: This script will construct and run SEM models for the breeding bird survey and the breeding record datasets
#------------------------------

# Importing Packages ####
library(brms)
library(tidybayes)
library(tidyverse)
library(tibble)

# Importing Data ####
sem_data <- read_csv("data/clean/sem/sem_data.csv") %>%
  filter(guild != "birdofprey") # remove bird of prey guild


# Filtering Data based on Guild ####
passerine_data <- sem_data %>%
  filter(guild == "passerine") 

shorebird_data <- sem_data %>%
  filter(guild == "shorebird")

waterbird_data <- sem_data %>%
  filter(guild == "waterbird")


bird_1guild_mod <- bf(bird.abundance ~ ice.melt + temp + snowmelt + budburst
               + (1|species) # Random Intercept
               #+ (ice.melt + temp + snowmelt + budburst|species) # Random Slope and Intercept
               + (1 | year)
               , family = poisson())
bird_3guild_mod <- bf(bird.abundance ~ (ice.melt + temp + snowmelt + budburst) * guild # Interaction
               + (1|species) + (1 | year)
               , family = poisson())
budburst_mod <- bf(budburst ~ snowmelt + temp)
snowmelt_mod <- bf(snowmelt ~ temp)
temp_mod <- bf(temp ~ ice.melt)

# Setting Bird Abundance Model Priors
abundance_priors_scaled <- c(
  # priors tighter when using poisson dist, because of log link function, before log was 0, 20
  set_prior("normal(0, 5)", coef = "ice.melt", resp = "birdabundance"), 
  set_prior("normal(0, 5)", coef = "temp", resp = "birdabundance"),
  set_prior("normal(0, 5)", coef = "snowmelt", resp = "birdabundance"),
  set_prior("normal(0, 5)", coef = "budburst", resp = "birdabundance"),
  
  # Budburst model priors
  set_prior("normal(0, 2)", coef = "snowmelt", resp = "budburst"),
  set_prior("normal(0, 4)", coef = "temp", resp = "budburst"), # 5 degree increase in snowmelt leads to a X week increase in budburst
  
  # Snowmelt model priors
  set_prior("normal(0, 4)", coef = "temp", resp = "snowmelt"),
  
  # Temp model priors
  set_prior("normal(0, 4)", coef = "ice.melt", resp = "temp")
)

passerine_bsem <- brm(
  bird_1guild_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
    set_rescor(FALSE),
  data = passerine_data,
  prior = abundance_priors_scaled,
  cores = 4,
  chains = 3,
  iter = 4000,
  warmup = 1000,
  thin = 2,
  control=list(adapt_delta=0.9999, max_treedepth=15)
)


shorebird_bsem <- brm(
  bird_1guild_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
    set_rescor(FALSE),
  data = shorebird_data,
  prior = abundance_priors_scaled,
  cores = 4,
  chains = 3,
  iter = 4000,
  warmup = 1000,
  thin = 2,
  control=list(adapt_delta=0.9999, max_treedepth=15)
)


waterbird_bsem <- brm(
  bird_1guild_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
    set_rescor(FALSE),
  data = waterbird_data,
  prior = abundance_priors_scaled,
  cores = 4,
  chains = 3,
  iter = 4000,
  warmup = 1000,
  thin = 2,
  control=list(adapt_delta=0.9999, max_treedepth=15)
)

guild_bsem <- brm(
  bird_3guild_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
    set_rescor(FALSE),
  data = sem_data,
  prior = abundance_priors_scaled,
  cores = 4,
  chains = 3,
  iter = 4000,
  warmup = 1000,
  thin = 2,
  control=list(adapt_delta=0.9999, max_treedepth=15)
)
