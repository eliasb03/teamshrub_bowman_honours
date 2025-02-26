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
  filter(guild != "birdofprey") # remove bird of prey guild


# Filtering Data based on Guild ####
passerine_data <- sem_data %>%
  filter(guild == "passerine") 

shorebird_data <- sem_data %>%
  filter(guild == "shorebird")

waterbird_data <- sem_data %>%
  filter(guild == "waterbird")


bird_1guild_mod <- bf(birdabundance ~ icemelt + breedingtemp + snowmelt + budburst
               + (1|species) # Random Intercept
               #+ (ice.melt + temp + snowmelt + budburst|species) # Random Slope and Intercept
               + (1 | year)
               , family = poisson())
bird_3guild_mod <- bf(birdabundance ~ (icemelt + breedingtemp + snowmelt + budburst) * guild # Interaction
               + (1|species) + (1 | year)
               , family = poisson())

budburst_mod <- bf(budburst ~ snowmelt + breedingtemp + regiontemp + (1 | year))
snowmelt_mod <- bf(snowmelt ~ breedingtemp + regiontemp + (1 | year))
breeding_temp_mod <- bf(breedingtemp ~ icemelt + regiontemp + (1 | year))
ice_melt_mod <- bf(icemelt ~ regiontemp + (1 | year))

# budburst_mod <- bf(budburst ~ snowmelt + breedingtemp + regiontemp)
# snowmelt_mod <- bf(snowmelt ~ breedingtemp + regiontemp)
# breeding_temp_mod <- bf(breedingtemp ~ icemelt + regiontemp)
# ice_melt_mod <- bf(icemelt ~ regiontemp)

# budburst_mod <- bf(budburst ~ snowmelt + temp)
# snowmelt_mod <- bf(snowmelt ~ temp)
# temp_mod <- bf(temp ~ ice.melt)



# Setting Bird Abundance Model Priors
abundance_priors_scaled <- c(
  # priors tighter when using poisson dist, because of log link function, values of the priors are log(true response)
  set_prior("normal(0, 4)", coef = "icemelt", resp = "birdabundance"), # 1 week increase in icemelt leads to a max 50 increase (log(50) = 3.9) in bird abundance
  set_prior("normal(0, 4)", coef = "snowmelt", resp = "birdabundance"),
  set_prior("normal(0, 4)", coef = "budburst", resp = "birdabundance"),
  set_prior("normal(0, 5)", coef = "breedingtemp", resp = "birdabundance"), # 5 degree increase in breeding season temp leads to at max a 100 increase in bird abundance (log(100) = 4.6)

  # Budburst model priors
  set_prior("normal(0, 2)", coef = "snowmelt", resp = "budburst"), # 1 week increase in snowmelt leads to a max 5 (log(5) = 1.6) week increase in budburst
  set_prior("normal(0, 2.5)", coef = "breedingtemp", resp = "budburst"), # 5 degree increase in breeding season temp leads to at max a 10 week increase (log(10)=2.3 week) change in budburst
  set_prior("normal(0, 2.5)", coef = "regiontemp", resp = "budburst"), # 5 degree increase in regional temp leads to at max a 10 week increase (log(10)=2.3 week) change in budburst
  
  # Snowmelt model priors
  set_prior("normal(0, 2.5)", coef = "breedingtemp", resp = "snowmelt"), # 5 degree increase in breeding season temp leads to at max a 10 week increase (log(10)=2.3 week) change in snowmelt
  set_prior("normal(0, 2.5)", coef = "regiontemp", resp = "snowmelt"),
  
  # Breeding temp model priors
  set_prior("normal(0, 3)", coef = "icemelt", resp = "breedingtemp"), # 1 week increase in icemelt leads to at max a 20 degree increase (log(20)=3) in breeding temp
  set_prior("normal(0, 3)", coef = "regiontemp", resp = "breedingtemp"), # 5 degree increase in region temp leads to at max a 20 degree increase in breeding temp
  
  # Ice melt model priors
  set_prior("normal(0, 2.5)", coef = "regiontemp", resp = "icemelt
            ") # 5 degree increase in region temp leads to at max a 10 week increase in ice melt
)

passerine_bsem <- brm(
  bird_1guild_mod +
    budburst_mod +
    snowmelt_mod +
    breeding_temp_mod +
    ice_melt_mod +
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
    breeding_temp_mod +
    ice_melt_mod +
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
    breeding_temp_mod +
    ice_melt_mod +
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

guild_abundance_priors_scaled <- c(  # priors tighter when using poisson dist, because of log link function, values of the priors are log(true response)
  set_prior("normal(0, 4)", coef = "icemelt", resp = "birdabundance"), # 1 week increase in icemelt leads to a max 50 increase (log(50) = 3.9) in bird abundance
  set_prior("normal(0, 4)", coef = "icemelt:guildshorebird", resp = "birdabundance"), 
  set_prior("normal(0, 4)", coef = "icemelt:guildwaterbird", resp = "birdabundance"),
  
  set_prior("normal(0, 4)", coef = "snowmelt", resp = "birdabundance"),
  set_prior("normal(0, 4)", coef = "snowmelt:guildshorebird", resp = "birdabundance"),
  set_prior("normal(0, 4)", coef = "snowmelt:guildwaterbird", resp = "birdabundance"),
  
  set_prior("normal(0, 4)", coef = "budburst", resp = "birdabundance"),
  set_prior("normal(0, 4)", coef = "budburst:guildshorebird", resp = "birdabundance"),
  set_prior("normal(0, 4)", coef = "budburst:guildwaterbird", resp = "birdabundance"),
  
  set_prior("normal(0, 5)", coef = "breedingtemp", resp = "birdabundance"), # 5 degree increase in breeding season temp leads to at max a 100 increase in bird abundance (log(100) = 4.6)
  set_prior("normal(0, 5)", coef = "breedingtemp:guildshorebird", resp = "birdabundance"),
  set_prior("normal(0, 5)", coef = "breedingtemp:guildwaterbird", resp = "birdabundance"),
  
  # Budburst model priors
  set_prior("normal(0, 2)", coef = "snowmelt", resp = "budburst"), # 1 week increase in snowmelt leads to a max 5 (log(5) = 1.6) week increase in budburst
  set_prior("normal(0, 2.5)", coef = "breedingtemp", resp = "budburst"), # 5 degree increase in breeding season temp leads to at max a 10 week increase (log(10)=2.3 week) change in budburst
  set_prior("normal(0, 2.5)", coef = "regiontemp", resp = "budburst"), # 5 degree increase in regional temp leads to at max a 10 week increase (log(10)=2.3 week) change in budburst
  
  # Snowmelt model priors
  set_prior("normal(0, 2.5)", coef = "breedingtemp", resp = "snowmelt"), # 5 degree increase in breeding season temp leads to at max a 10 week increase (log(10)=2.3 week) change in snowmelt
  set_prior("normal(0, 2.5)", coef = "regiontemp", resp = "snowmelt"),
  
  # Breeding temp model priors
  set_prior("normal(0, 3)", coef = "icemelt", resp = "breedingtemp"), # 1 week increase in icemelt leads to at max a 20 degree increase (log(20)=3) in breeding temp
  set_prior("normal(0, 3)", coef = "regiontemp", resp = "breedingtemp"), # 5 degree increase in region temp leads to at max a 20 degree increase in breeding temp
  
  # Ice melt model priors
  set_prior("normal(0, 2.5)", coef = "regiontemp", resp = "icemelt") # 5 degree increase in region temp leads to at max a 10 week increase in ice melt
)

guild_bsem <- brm(
  bird_3guild_mod +
    budburst_mod +
    snowmelt_mod +
    breeding_temp_mod +
    ice_melt_mod +
    set_rescor(FALSE),
  data = sem_data,
  prior = guild_abundance_priors_scaled,
  cores = 4,
  chains = 3,
  iter = 4000,
  warmup = 1000,
  thin = 2,
  control=list(adapt_delta=0.9999, max_treedepth=15)
)

# Look at model outputs
summary(passerine_bsem)
summary(shorebird_bsem)
summary(waterbird_bsem)
summary(guild_bsem)

# Posterior Predictive Checks
pp_check(guild_bsem, resp = "birdabundance", ndraws = 1000)
pp_check(guild_bsem, resp = "budburst", ndraws = 1000)
pp_check(guild_bsem, resp = "snowmelt", ndraws = 1000)
pp_check(guild_bsem, resp = "breedingtemp", ndraws = 1000)
pp_check(guild_bsem, resp = "icemelt", ndraws = 1000)


# Saving model outputs
model_output_path <- "outputs/sem/abundance/"
saveRDS(passerine_bsem, file = paste0(model_output_path, "passerine.rds"))
saveRDS(shorebird_bsem, file = paste0(model_output_path, "shorebird.rds"))
saveRDS(waterbird_bsem, file = paste0(model_output_path, "waterbird.rds"))
saveRDS(guild_bsem, file = paste0(model_output_path, "guild.rds"))

# Read model outputs
model_output_path <- "outputs/sem/abundance/"
passerine_bsem <- readRDS(file = paste0(model_output_path, "passerine.rds"))
shorebird_bsem <- readRDS(file = paste0(model_output_path, "shorebird.rds"))
waterbird_bsem <- readRDS(file = paste0(model_output_path, "waterbird.rds"))
guild_bsem <- readRDS(file = paste0(model_output_path, "guild.rds"))

# Create model outputs, scaled by link functions
process_fixed_effects <- function(model, delay_invlink = FALSE) {
  summary(model)$fixed %>%
    tibble::rownames_to_column(var = "Parameter") %>%
    mutate(parameter = Parameter) %>%  # Keep a copy of the original column
    separate(Parameter, into = c("response", "predictor"), sep = "_", extra = "merge", fill = "right") %>%
    mutate(
      # Extract interaction values
      interaction = case_when(
        grepl(":guild", predictor) ~ sub(".*:guild", "", predictor),  # Extract after ":guild"
        grepl("^guild", predictor) ~ sub("^guild", "", predictor),    # Extract after "guild" (for interaction intercepts)
        TRUE ~ NA_character_
      ),
      # Remove :guildX from predictor but keep the main term
      predictor = case_when(
        grepl(":guild", predictor) ~ sub(":guild.*", "", predictor),  # Remove everything after ":guild"
        grepl("^guild", predictor) ~ "Intercept",  # Replace predictor for interaction intercepts
        TRUE ~ predictor
      ),
      # Apply exp() only to rows where response is "birdabundance" and delay_invlink is FALSE
      across(c(estimate = Estimate, est.error = Est.Error, l95.ci = `l-95% CI`, u95.ci = `u-95% CI`),
             ~ ifelse((response == "birdabundance" & !delay_invlink), exp(.x), .x))
    ) %>%
    rename(rhat = Rhat, bulk.ess = Bulk_ESS, tail.ess = Tail_ESS) %>%
    select(response, predictor, interaction, estimate, est.error, l95.ci, u95.ci, parameter, rhat, bulk.ess, tail.ess)
}

passerine_bsem_effect <- process_fixed_effects(passerine_bsem)
shorebird_bsem_effect <- process_fixed_effects(shorebird_bsem)
waterbird_bsem_effect <- process_fixed_effects(waterbird_bsem)
guild_bsem_effect <- process_fixed_effects(guild_bsem, delay_invlink = TRUE)

guildsummed_bsem_effect<- guild_bsem_effect %>%
  group_by(response, predictor) %>%
  mutate(
    # Create a temporary column for updating
    new.estimate = case_when(
      is.na(interaction) ~ estimate,
      interaction == "shorebird" ~ estimate + estimate[is.na(interaction)],
      interaction == "waterbird" ~ estimate + estimate[is.na(interaction)]
    ),
    new.est.error = case_when(
      is.na(interaction) ~ est.error,
      interaction == "shorebird" ~ est.error + est.error[is.na(interaction)],
      interaction == "waterbird" ~ est.error + est.error[is.na(interaction)]
    ),
    new.l95.ci = case_when(
      is.na(interaction) ~ l95.ci,
      interaction == "shorebird" ~ l95.ci + l95.ci[is.na(interaction)],
      interaction == "waterbird" ~ l95.ci + l95.ci[is.na(interaction)]
    ),
    new.u95.ci = case_when(
      is.na(interaction) ~ u95.ci,
      interaction == "shorebird" ~ u95.ci + u95.ci[is.na(interaction)],
      interaction == "waterbird" ~ u95.ci + u95.ci[is.na(interaction)]
    ),
    interaction = case_when(
      is.na(interaction) & response == "birdabundance" ~ "passerine",
      interaction == "shorebird" ~ "shorebird",
      interaction == "waterbird" ~ "waterbird",
      TRUE ~ NA_character_  # Default case to handle unmatched conditions
    )
  ) %>%
  ungroup()


