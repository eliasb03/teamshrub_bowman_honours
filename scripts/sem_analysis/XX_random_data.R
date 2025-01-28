#####
# Script to make random dataset, copy of final_data to build sem model
# Elias Bowman, 2025-01-18

library(brms)
library(tidyverse)

# Generate a new random dataset with flexible length
set.seed(123)  # Optional: For reproducibility

# Mapping of original column names to model-friendly keywords
column_mapping <- c(
  "Tave_sm" = "temp",
  "budburst_mean" = "budburst",
  "snowmelt_mean" = "snowmelt",
  "spring_drop_doy" = "ice.melt",
  "rel.abundance.scaled" = "bird.abundance"
)

# Generate random dataset ####
# structured like final_data
n_rows <- 1000  # Replace this with the desired number of rows

random_data <- tibble(
  year = sample(1990:2024, n_rows, replace = TRUE),  # Random years between 1990 and 2024
  spec.code = sample(letters, n_rows, replace = TRUE),  # Random species codes (using letters)
  species = sample(
    c("Species A", "Species B", "Species C", "Species D"),
    n_rows,
    replace = TRUE
  ),  # Random species names
  guild = case_when(
    species %in% c("Species A", "Species B") ~ "Guild 1",  # Guilds based on species
    species %in% c("Species C", "Species D") ~ "Guild 2",
    TRUE ~ "Other"
  ),
  rel.abundance.total = runif(n_rows, 0, 1),  # Random relative abundance between 0 and 1
  rel.abundance.scaled = runif(n_rows, 0, 1),  # Random scaled abundance
  logistic.id.total = sample(c("low", "high"), n_rows, replace = TRUE),  # Random logistic categories
  spring_drop_doy = sample(100:250, n_rows, replace = TRUE),  # Random spring drop day of year
  snowmelt_mean = sample(100:250, n_rows, replace = TRUE),  # Random snowmelt mean values
  budburst_mean = sample(100:250, n_rows, replace = TRUE),  # Random budburst mean values
  Tave_sm = runif(n_rows, 0, 15)  # Random temperature values
)

random_subset <- random_data %>%
  select(year, species, guild, rel.abundance.scaled, budburst_mean, snowmelt_mean, spring_drop_doy, Tave_sm) %>%
  rename_with(~ ifelse(. %in% names(column_mapping), column_mapping[.], .), everything())

# Building model structure ####
# hierarchy - guild fixed year and species random effect

# bird_mod <- bf(bird.abundance ~ ice.melt + temp + snowmelt + budburst)
# budburst_mod <- bf(budburst ~ snowmelt + temp)
# snowmelt_mod <- bf(snowmelt ~ temp)
# temp_mod <- bf(temp ~ ice.melt)

bird_mod <- bf(bird.abundance ~ ice.melt + temp + snowmelt + budburst + guild + (1|species) + (1|year))
budburst_mod <- bf(budburst ~ snowmelt + temp + (1|year))
snowmelt_mod <- bf(snowmelt ~ temp + (1|year))
temp_mod <- bf(temp ~ ice.melt + (1|year))

# Defining Priors ######
# Priors for bird.abundance model ####
prior_bird_abundance <- c(
  # Intercept prior for bird.abundance
  set_prior("uniform(0, 1000)", class = "b", coef = "(Intercept)", resp = "birdabundance"),
  
  # Fixed effect priors for bird.abundance predictors
  set_prior("normal(0, 10)", class = "b", coef = "ice.melt", resp = "birdabundance"),
  set_prior("normal(0, 10)", class = "b", coef = "temp", resp = "birdabundance"),
  set_prior("normal(0, 10)", class = "b", coef = "snowmelt", resp = "birdabundance"),
  set_prior("normal(0, 10)", class = "b", coef = "budburst", resp = "birdabundance"),
  set_prior("normal(0, 100)", class = "b", coef = "guild", resp = "birdabundance"),
  
  # Priors for group-level standard deviations
  set_prior("student_t(3, 0, 2.5)", class = "sd", group = "species", resp = "birdabundance"),
  set_prior("student_t(3, 0, 2.5)", class = "sd", group = "year", resp = "birdabundance"),
  
  # Prior for residual standard deviation
  set_prior("student_t(3, 0, 2.5)", class = "sigma", resp = "birdabundance")
  
)

# Priors for budburst model ####
prior_budburst <- c(
  # Intercept prior for budburst
  set_prior("normal(172.5, 54.1)", class = "b", coef = "(Intercept)", resp = "budburst"),
  
  # Fixed effect priors for budburst predictors
  set_prior("normal(0, 1)", class = "b", coef = "snowmelt", resp = "budburst"),
  set_prior("normal(0, 1)", class = "b", coef = "temp", resp = "budburst"),
  set_prior("normal(0, 1)", class = "b", coef = "guild", resp = "budburst"),
  
  # Priors for group-level standard deviations
  set_prior("student_t(3, 0, 54.1)", class = "sd", group = "species", resp = "budburst"),
  set_prior("student_t(3, 0, 54.1)", class = "sd", group = "year", resp = "budburst"),
  
  # Prior for residual standard deviation
  set_prior("student_t(3, 0, 54.1)", class = "sigma", resp = "budburst")
)

# Priors for snowmelt model ####
prior_snowmelt <- c(
  # Intercept prior for snowmelt
  set_prior("normal(173, 59.3)", class = "b", coef = "(Intercept)", resp = "snowmelt"),
  
  # Fixed effect priors for snowmelt predictors
  set_prior("normal(0, 1)", class = "b", coef = "temp", resp = "snowmelt"),
  set_prior("normal(0, 1)", class = "b", coef = "guild", resp = "snowmelt"),
  
  # Priors for group-level standard deviations
  set_prior("student_t(3, 0, 59.3)", class = "sd", group = "species", resp = "snowmelt"),
  set_prior("student_t(3, 0, 59.3)", class = "sd", group = "year", resp = "snowmelt"),
  
  # Prior for residual standard deviation
  set_prior("student_t(3, 0, 59.3)", class = "sigma", resp = "snowmelt")
)

# Priors for temp model ####
prior_temp <- c(
  # Intercept prior for temp
  set_prior("normal(7.4, 5.9)", class = "b", coef = "(Intercept)", resp = "temp"),
  
  # Fixed effect priors for temp predictors
  set_prior("normal(0, 1)", class = "b", coef = "ice.melt", resp = "temp"),
  set_prior("normal(0, 1)", class = "b", coef = "guild", resp = "temp"),
  
  # Priors for group-level standard deviations
  set_prior("student_t(3, 0, 5.9)", class = "sd", group = "species", resp = "temp"),
  set_prior("student_t(3, 0, 5.9)", class = "sd", group = "year", resp = "temp"),
  
  # Prior for residual standard deviation
  set_prior("student_t(3, 0, 5.9)", class = "sigma", resp = "temp")
)

# Combine all priors
all_priors <- c(prior_bird_abundance, prior_budburst, prior_snowmelt, prior_temp)


# Model ####
brms_sem <- brm(
  bird_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
    set_rescor(FALSE),
  data = random_subset,
  prior = all_priors,
  cores = 4,
  chains = 2
)

summary(brms_sem)
plot(brms_sem)

get_prior(
  bird_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
    set_rescor(FALSE),
  data = random_subset
)
