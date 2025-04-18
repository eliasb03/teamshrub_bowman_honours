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
library(broom)

# Importing Data ####
sem_data <- read_csv("data/clean/sem/sem_data.csv") %>%
  filter(guild != "birdofprey") %>% # remove bird of prey guild
  filter(birdabundance > 0) 


bird_3guild_mod <- bf(birdabundance ~ (icemelt + breedingtemp + snowmelt + budburst) * guild
               + (1|species) + (1 | year), family = poisson()) # Interaction model
budburst_mod <- bf(budburst ~ snowmelt + breedingtemp + regiontemp)
snowmelt_mod <- bf(snowmelt ~ breedingtemp + regiontemp)
breeding_temp_mod <- bf(breedingtemp ~ icemelt + regiontemp)
ice_melt_mod <- bf(icemelt ~ regiontemp)


# Setting Bird Abundance Model Priors
guild_abundance_priors <- c(  # priors tighter when using poisson dist, because of log link function, values of the priors are log(true response)
  set_prior("normal(0, 100)", class = "Intercept", resp = "birdabundance"), # range of bird abundance values
  
  set_prior("normal(0, 4)", coef = "icemelt", resp = "birdabundance"), # 1 week increase in icemelt leads to a max 50 increase (log(50) = 3.9) in bird abundance
  set_prior("normal(0, 4)", coef = "icemelt:guildshorebird", resp = "birdabundance"), 
  set_prior("normal(0, 4)", coef = "icemelt:guildwaterbird", resp = "birdabundance"),
  
  set_prior("normal(0, 4)", coef = "snowmelt", resp = "birdabundance"),
  set_prior("normal(0, 4)", coef = "snowmelt:guildshorebird", resp = "birdabundance"),
  set_prior("normal(0, 4)", coef = "snowmelt:guildwaterbird", resp = "birdabundance"),
  
  set_prior("normal(0, 4)", coef = "budburst", resp = "birdabundance"),
  set_prior("normal(0, 4)", coef = "budburst:guildshorebird", resp = "birdabundance"),
  set_prior("normal(0, 4)", coef = "budburst:guildwaterbird", resp = "birdabundance"),
  
  set_prior("normal(0, 5)", coef = "breedingtemp", resp = "birdabundance"), # 1 degree increase in breeding season temp leads to at max a 100 increase in bird abundance (log(100) = 4.6)
  set_prior("normal(0, 5)", coef = "breedingtemp:guildshorebird", resp = "birdabundance"),
  set_prior("normal(0, 5)", coef = "breedingtemp:guildwaterbird", resp = "birdabundance"),
  
  # Budburst model priors
  set_prior("normal(0, 2)", coef = "snowmelt", resp = "budburst"), # 1 week increase in snowmelt leads to a max 5 (log(5) = 1.6) week increase in budburst
  set_prior("normal(0, 2.5)", coef = "breedingtemp", resp = "budburst"), # 1 degree increase in breeding season temp leads to at max a 10 week increase (log(10)=2.3 week) change in budburst
  set_prior("normal(0, 2.5)", coef = "regiontemp", resp = "budburst"), # 1 degree increase in regional temp leads to at max a 10 week increase (log(10)=2.3 week) change in budburst
  
  # Snowmelt model priors
  set_prior("normal(0, 2.5)", coef = "breedingtemp", resp = "snowmelt"), # 1 degree increase in breeding season temp leads to at max a 10 week increase (log(10)=2.3 week) change in snowmelt
  set_prior("normal(0, 2.5)", coef = "regiontemp", resp = "snowmelt"),
  
  # Breeding temp model priors
  set_prior("normal(0, 3)", coef = "icemelt", resp = "breedingtemp"), # 1 week increase in icemelt leads to at max a 20 degree increase (log(20)=3) in breeding temp
  set_prior("normal(0, 3)", coef = "regiontemp", resp = "breedingtemp"), # 1 degree increase in region temp leads to at max a 20 degree increase in breeding temp
  
  # Ice melt model priors
  set_prior("normal(0, 2.5)", coef = "regiontemp", resp = "icemelt") # 1 degree increase in region temp leads to at max a 10 week increase in ice melt
)


guild_bsem <- brm(
  bird_3guild_mod +
    budburst_mod +
    snowmelt_mod +
    breeding_temp_mod +
    ice_melt_mod +
    set_rescor(FALSE),
  data = sem_data,
  prior = guild_abundance_priors,
  cores = 4,
  chains = 3,
  iter = 6000,
  warmup = 1000,
  thin = 2,
  control=list(adapt_delta=0.9999, max_treedepth=15)
)


# Look at model outputs
get_prior(guild_bsem)
summary(guild_bsem)

# Posterior Predictive Checks
# pp_check(guild_bsem, resp = "birdabundance", ndraws = 1000)
# pp_check(guild_bsem, resp = "budburst", ndraws = 1000)
# pp_check(guild_bsem, resp = "snowmelt", ndraws = 1000)
# pp_check(guild_bsem, resp = "breedingtemp", ndraws = 1000)
# pp_check(guild_bsem, resp = "icemelt", ndraws = 1000)


# Saving model outputs
model_output_path <- "outputs/sem/abundance/"
saveRDS(guild_bsem, file = paste0(model_output_path, "guild.rds"))

# Read model outputs
model_output_path <- "outputs/sem/abundance/"
guild_bsem <- readRDS(file = paste0(model_output_path, "guild.rds"))
