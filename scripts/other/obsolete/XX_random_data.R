#####
# Script to make random dataset, copy of final_data to build sem model
# Elias Bowman, 2025-01-18

library(brms)
library(tidyverse)

set.seed(123)  # For reproducibility

# Mapping of original column names to model-friendly keywords
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

# Generate a new random dataset and bounds dataset
### with flexible length structured like final_data
n_rows <- 1000
data_points <- 350 # number of datapoints to use in model
# Random Dataset ####
random_data <- tibble(
  year = as.factor(sample(1990:2024, n_rows, replace = TRUE)),
  # Random years between 1990 and 2024
  spec.code = sample(letters, n_rows, replace = TRUE),
  # Random species codes (using letters)
  species = sample(
    c("Species A", "Species B", "Species C", "Species D"),
    n_rows,
    replace = TRUE
  ),
  # Random species names
  guild = case_when(
    species %in% c("Species A", "Species B") ~ "Guild 1",
    # Guilds based on species
    species %in% c("Species C", "Species D") ~ "Guild 2",
    TRUE ~ "Other"
  ),
  rel.abundance.total = rnorm(n_rows, 0, 1), #runif(n_rows, 0, 1)
  # Random relative abundance between 0 and 1
  rel.abundance.scaled = rnorm(n_rows, 0, 1), #runif(n_rows, 0, 1)
  # Random scaled abundance
  logistic.id.total = sample(c("low", "high"), n_rows, replace = TRUE),
  # Random logistic categories
  spring_drop_doy = sample(100:250, n_rows, replace = TRUE),
  # Random spring drop day of year
  snowmelt_mean = sample(100:250, n_rows, replace = TRUE),
  # Random snowmelt mean values
  budburst_mean = sample(100:250, n_rows, replace = TRUE),
  # Random budburst mean values
  Tave_sm = runif(n_rows, 0, 15)  # Random temperature values
)

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
  rename_with( ~ ifelse(. %in% names(column_mapping), column_mapping[.], .), everything()) %>%
  sample_n(data_points)
# Bounds Dataset ####
bounds_data <- tibble(
  parameter = c("bird.abundance", "budburst", "snowmelt", "ice.melt", "temp"),
  upper = c(1, 250, 250, 250, 25),
  lower = c(0, 100, 100, 100, -15)
)

# Defining Scaling Functions ####
# Function: compute scaling parameters for each numeric column 
compute_scaling_params <- function(df) {
  # Determine which columns are numeric
  numeric_cols <- sapply(df, is.numeric)
  
  # Create an empty data frame to store results
  params_df <- data.frame(
    parameter = character(),
    mean = numeric(),
    sd = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop over each numeric column
  for (col in names(df)[numeric_cols]) {
    # Compute the mean and standard deviation (using the data)
    mean_val <- mean(df[[col]], na.rm = TRUE)
    sd_val <- sd(df[[col]], na.rm = TRUE)
    
    # Append the results to our summary data frame.
    params_df <- rbind(params_df,
                       data.frame(parameter = col,
                                  mean = mean_val,
                                  sd = sd_val,
                                  stringsAsFactors = FALSE))
  }
  
  return(params_df)
}

# Function: unscale a vector based on sd and mean from compute_scaling_params
unscale <- function(scaled_vector, sd, mean) {
  original <- (y * sd) + mean
  return(original)
}

# Center (subtract the mean) and scale (divide by the standard deviation) each column
subset_scaled <- random_subset %>%
  mutate(across(4:8, ~ scale(.)[, 1]))

scaling_params <- compute_scaling_params(random_subset)

scaled_bounds <- bounds_data %>%
  left_join(scaling_params, by = c("parameter" = "parameter")) %>%
  mutate(upper_scaled = (upper - mean) / sd,
         lower_scaled = (lower - mean) / sd)

# Building model structure ####
bird_mod <- bf(bird.abundance ~ ice.melt + temp + snowmelt + budburst + 
                 (1|species) + (1|year))
budburst_mod <- bf(budburst ~ snowmelt + temp + (1 | year))
snowmelt_mod <- bf(snowmelt ~ temp + (1 | year))
temp_mod <- bf(temp ~ ice.melt + (1 | year))

# Defining Priors ######
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


# Print the priors to check
priors

summary(brms_sem)

stancode(brms_sem)

pp_check(brms_sem, resp = "birdabundance")
pp_check(brms_sem, resp = "birdabundance")

get_prior(brms_sem)
# TO DO: 
# - Declare Priors - give some mildly informative ones
# - Try normalizing and centering the data (all between -1 and 1 sort of thing)
# - Figure out how to unormalize the results
# - Think about how to simplify the model


plot(brms_sem)

get_prior(bird_mod +
            budburst_mod +
            snowmelt_mod +
            temp_mod +
            set_rescor(FALSE),
          data = random_subset)



 
# # Create priors dynamically
# priors <- lapply(1:nrow(scaled_bounds), function(i) {
#   var <- scaled_bounds$parameter[i]
#   lower <- scaled_bounds$lower_scaled[i]
#   upper <- scaled_bounds$upper_scaled[i]
#   set_prior(paste0("uniform(", lower, ", ", upper, ")"), coef = var, resp = "birdabundance")
# })
# #set_prior(paste0("normal(0, 1) T[", lower, ", ", upper, "]"), coef = var, resp = "birdabundance")
# 
# # Function to generate priors based on the model and predictors
# generate_priors <- function(models_table, bounds_table) {
#   priors <- list()
#   
#   # Loop over the models
#   for (model in models_table$model) {
#     # Get columns for this model
#     model_vars <- colnames(models_table)[2:ncol(models_table)]  # Skip the model name column
#     model_vars <- model_vars[models_table[model == models_table$model, model_vars] == TRUE]  # Keep only TRUE entries
#     
#     # Loop over the variables for this model
#     for (var in model_vars) {
#       # Check if bounds exist for this variable
#       bounds <- bounds_table[bounds_table$parameter == var, ]
#       lower_bound <- bounds$lower_scaled
#       upper_bound <- bounds$upper_scaled
#       
#       # Add prior for this variable
#       prior_str <- paste0("uniform(", lower_bound, ",", upper_bound, ")")  # Example truncated prior
#       priors[[paste0(model, "_", var)]] <- set_prior(prior_str, coef = var, resp = model)
#     }
#   }
#   
#   # Return the generated priors
#   return(priors)
# }
# 
# # Generate the priors
# priors <- generate_priors(models_table, scaled_bounds)

priors <- c(
  # Bird abundance model priors
  #set_prior("uniform(-2, 2)", coef = "(Intercept)", resp = "birdabundance"),
  set_prior("uniform(-1.81735931275449, 1.6796119495095)", coef = "ice.melt", resp = "birdabundance"),
  set_prior("uniform(-5.10716094045919, 3.95723016679574)", coef = "temp", resp = "birdabundance"),
  set_prior("uniform(-1.65327414887036, 1.67457236656917)", coef = "snowmelt", resp = "birdabundance"),
  set_prior("uniform(-1.64463105310479, 1.79931335629259)", coef = "budburst", resp = "birdabundance"),
  #set_prior("uniform(,)", coef = "guild", resp = "birdabundance"),
  
  # Budburst model priors
  #set_prior("uniform(-2, 2)", coef = "(Intercept)", resp = "budburst"),
  set_prior("uniform(-1.65327414887036, 1.67457236656917)", coef = "snowmelt", resp = "budburst"),
  set_prior("uniform(-5.10716094045919, 3.95723016679574)", coef = "temp", resp = "budburst"),
  #set_prior("uniform(,)", coef = "guild", resp = "budburst"),
  
  # Snowmelt model priors
  #set_prior("uniform(-2, 2)", coef = "(Intercept)", resp = "snowmelt"),
  set_prior("uniform(-5.10716094045919, 3.95723016679574)", coef = "temp", resp = "snowmelt"),
  #, set_prior("uniform(,)", coef = "guild", resp = "snowmelt"),
  
  # Temp model priors
  #set_prior("uniform(-2, 2)", coef = "(Intercept)", resp = "temp"),
  set_prior("uniform(-1.81735931275449, 1.6796119495095)", coef = "ice.melt", resp = "temp")
  #set_prior("uniform(,)", coef = "guild", resp = "temp")
)

priors <- c(
  # Bird abundance model priors
  set_prior("uniform(-1.81735931275449, 1.6796119495095)", coef = "ice.melt", resp = "birdabundance"),
  set_prior("uniform(-5.10716094045919, 3.95723016679574)", coef = "temp", resp = "birdabundance"),
  set_prior("uniform(-1.65327414887036, 1.67457236656917)", coef = "snowmelt", resp = "birdabundance"),
  set_prior("uniform(-1.64463105310479, 1.79931335629259)", coef = "budburst", resp = "birdabundance"),
  
  # Budburst model priors
  set_prior("uniform(-1.65327414887036, 1.67457236656917)", coef = "snowmelt", resp = "budburst"),
  set_prior("uniform(-5.10716094045919, 3.95723016679574)", coef = "temp", resp = "budburst"),
  
  # Snowmelt model priors
  set_prior("uniform(-5.10716094045919, 3.95723016679574)", coef = "temp", resp = "snowmelt"),
  
  # Temp model priors
  set_prior("uniform(-1.81735931275449, 1.6796119495095)", coef = "ice.melt", resp = "temp")
  
)


priors <- c(
  # Bird abundance model priors
  set_prior("uniform(-7, 7)", #coef = "ice.melt", 
            resp = "birdabundance", lb = -5, ub = 5),
  #set_prior("uniform(-Inf, Inf)", coef = "temp", resp = "birdabundance", lb = -5, ub = 5),
  #set_prior("uniform(-Inf, Inf)", coef = "snowmelt", resp = "birdabundance", lb = -5, ub = 5),
  #set_prior("uniform(-Inf, Inf)", coef = "budburst", resp = "birdabundance", lb = -5, ub = 5),
  
  # Budburst model priors
  set_prior("uniform(-7, 7)", #coef = "snowmelt", 
            resp = "budburst", lb = -5, ub = 5),
  #set_prior("uniform(-Inf, Inf)", coef = "temp", resp = "budburst", lb = -5, ub = 5),
  
  # Snowmelt model priors
  set_prior("uniform(-7, 7)", #coef = "temp", 
            resp = "snowmelt", lb = -5, ub = 5),
  
  # Temp model priors
  set_prior("uniform(-7, 7)", #coef = "ice.melt", 
            resp = "temp", lb = -5, ub = 5)
  
)


##############

brms_sem <- brm(
  bird_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
    set_rescor(FALSE),
  data = random_subset,
  #prior = priors,
  cores = 4,
  iter = 5000,
  warmup = 1000,
  thin = 2,
  chains = 2,
  control=list(adapt_delta=0.9999)
)
