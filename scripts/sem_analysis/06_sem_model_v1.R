#------------------------------
# teamshrub_bowman_honours
# 06_sem_model_v1
# By: Elias Bowman 
# Created: 2024-10-16
# Last update: 2025-02-08
# 
# Description: This script will construct and run SEM models for my analysis
# At this time (16Oct2024) I am not ready to run my model at all, but I think it is valuable to make a code framework to do so 
#------------------------------

# Importing Packages ####
library(piecewiseSEM)
library(brms)
library(semPlot)
library(DiagrammeR)

# Importing Data ####

column_mapping <- c(
  "Tave_sm" = "temp",
  "budburst_mean" = "budburst",
  "snowmelt_mean" = "snowmelt",
  "spring_drop_doy" = "ice.melt",
  "rel.abundance.scaled" = "bird.abundance"
)

models_table <- data.frame(
  model = c("birdabundance", "budburst", "snowmelt", "temp"),
  intercept = c(TRUE, TRUE, TRUE, TRUE),
  ice.melt = c(TRUE, FALSE, FALSE, TRUE),
  temp = c(TRUE, TRUE, TRUE, FALSE),
  snowmelt = c(TRUE, TRUE, FALSE, FALSE),
  budburst = c(TRUE, FALSE, FALSE, FALSE),
  #guild = c(TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

# Generating Random Data ####
n_rows <- 1000
data_points <- 350 # number of datapoints to use in model
# Random Dataset ####
random_data <- tibble(
  # Random year between 1990 and 2024 (as a factor)
  year = as.factor(sample(1990:2024, n_rows, replace = TRUE)),
  
  # Random species code (single letter)
  spec.code = sample(letters, n_rows, replace = TRUE),
  
  # Random species name from four possible options
  species = sample(c("Species A", "Species B", "Species C", "Species D"), n_rows, replace = TRUE),
  
  # Assign guild based on species
  guild = case_when(
    species %in% c("Species A", "Species B") ~ "Guild 1",
    species %in% c("Species C", "Species D") ~ "Guild 2",
    TRUE ~ "Other"
  ),
  
  # Random relative abundance values
  rel.abundance.total = rnorm(n_rows, 0, 1),  # Normal distribution, mean = 0, sd = 1
  rel.abundance.scaled = rnorm(n_rows, 0, 1), # Scaled abundance values
  
  # Random logistic categories (low or high)
  logistic.id.total = sample(c("low", "high"), n_rows, replace = TRUE),
  
  # Random day of year values for spring drop, snowmelt, and budburst
  spring_drop_doy = sample(100:250, n_rows, replace = TRUE),
  snowmelt_mean = sample(100:250, n_rows, replace = TRUE),
  budburst_mean = sample(100:250, n_rows, replace = TRUE),
  
  # Random mean summer temperature between 0 and 15°C
  Tave_sm = runif(n_rows, 0, 15)
)

# Select a subset of columns and rename them if they exist in column_mapping
random_subset <- random_data %>%
  select(
    year,
    species,
    guild,
    rel.abundance.scaled,
    budburst_mean,
    snowmelt_mean,
    spring_drop_doy,
    Tave_sm
  ) %>%
  rename_with(~ ifelse(. %in% names(column_mapping), column_mapping[.], .), everything()) %>%  # Conditionally rename columns
  sample_n(data_points)  # Randomly sample the specified number of rows

# Define bounds for key parameters
bounds_data <- tibble(
  parameter = c("bird.abundance", "budburst", "snowmelt", "ice.melt", "temp"),
  upper = c(1, 250, 250, 250, 25),   # Upper bounds for each parameter
  lower = c(0, 100, 100, 100, -15)  # Lower bounds for each parameter
)

# Constructing SEM Model ####
bird_mod <- bf(bird.abundance ~ ice.melt + temp + snowmelt + budburst + guild +
                 (1|species) + (1|year))
budburst_mod <- bf(budburst ~ snowmelt + temp + (1 | year))
snowmelt_mod <- bf(snowmelt ~ temp + (1 | year))
temp_mod <- bf(temp ~ ice.melt + (1 | year))

# Running prior SEM Model ####

brms_sem_prior <- brm(
  bird_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
    set_rescor(FALSE),
  data = random_subset,
  #prior = priors, # Use uninformative priors
  cores = 4,
  iter = 5000,
  warmup = 1000,
  thin = 2,
  chains = 2,
  control=list(adapt_delta=0.9999)
)

summary(brms_sem_prior)
# Creating Final Priors ####
priors <- c(
  # Bird abundance model priors
  set_prior("normal(-50,100)", coef = "ice.melt", resp = "birdabundance"),
  set_prior("normal(-50,100)", coef = "temp", resp = "birdabundance"),
  set_prior("normal(-50,100)", coef = "snowmelt", resp = "birdabundance"),
  set_prior("normal(-50,100)", coef = "budburst", resp = "birdabundance"),
  
  # Budburst model priors
  set_prior("normal(-50,100)", coef = "snowmelt", resp = "budburst"),
  set_prior("normal(-50,100)", coef = "temp", resp = "budburst"),
  
  # Snowmelt model priors
  set_prior("normal(-50,100)", coef = "temp", resp = "snowmelt"),
  
  # Temp model priors
  set_prior("normal(-50,100)", coef = "ice.melt", resp = "temp")
  
)

# Running SEM Model ####
brms_sem <- brm(
  bird_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
    set_rescor(FALSE),
  data = random_subset,
  prior = priors,
  cores = 4,
  iter = 5000,
  warmup = 1000,
  thin = 2,
  chains = 2,
  control=list(adapt_delta=0.9999)
)

