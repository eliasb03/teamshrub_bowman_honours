#------------------------------
# teamshrub_bowman_honours
# 06_sem_model_v1
# By: Elias Bowman 
# Created: 2024-10-16
# Last update: 2025-02-09
# 
# Description: This script will construct and run SEM models for the breeding bird survey and the breeding record datasets
#------------------------------

# Importing Packages ####
library(brms)
# library(semPlot)
# library(DiagrammeR)
library(tidybayes)
library(tidyverse)
library(ggplot2)
library(ggdag)
library(grid)
library(tibble)

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
data_points <- 300 # number of datapoints to use in model
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
bird_mod <- bf(bird.abundance ~ guild * (ice.melt + temp + snowmelt + budburst) + (1|species) + (1 | year))
# bird_mod <- bf(bird.abundance ~ ice.melt + temp + snowmelt + budburst + 
#                  (1| guild) + (1|year))
# bird_mod <- bf(bird.abundance ~ ice.melt + temp + snowmelt + budburst + 
#                  (1 + ice.melt + temp + snowmelt + budburst | guild) + (1|year))
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
plot(brms_sem_prior)


# Creating Final Priors ####
priors <- c(
  # Bird abundance model priors
  set_prior("normal()", coef = "ice.melt", resp = "birdabundance"),
  set_prior("normal()", coef = "temp", resp = "birdabundance"),
  set_prior("normal()", coef = "snowmelt", resp = "birdabundance"),
  set_prior("normal()", coef = "budburst", resp = "birdabundance"),
  
  # Budburst model priors
  set_prior("normal()", coef = "snowmelt", resp = "budburst"),
  set_prior("normal()", coef = "temp", resp = "budburst"),
  
  # Snowmelt model priors
  set_prior("normal()", coef = "temp", resp = "snowmelt"),
  
  # Temp model priors
  set_prior("normal()", coef = "ice.melt", resp = "temp")
  
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



#

# Extract model estimates (posterior means and credible intervals)
model_summary <- summary(brms_sem_prior)

fixed_effects <- model_summary$fixed %>%
  mutate(coef = rownames(.)) %>%
  select(coef, Estimate, Est.Error, `l-95% CI`, `u-95% CI`)

# Define the coefficients corresponding to each desired path.
target_coefs <- c("temp_ice.melt", "snowmelt_temp", "budburst_snowmelt", 
                  "birdabundance_snowmelt", "birdabundance_budburst", 
                  "birdabundance_ice.melt", "birdabundance_temp")

target_paths <- c("Ice Melt → Temperature", 
                  "Temperature → Snowmelt", 
                  "Snowmelt → Budburst", 
                  "Snowmelt → Bird Abundance", 
                  "Budburst → Bird Abundance", 
                  "Ice Melt → Bird Abundance", 
                  "Temperature → Bird Abundance")

# Create a tibble with the estimates for only these coefficients.
estimates <- fixed_effects %>%
  filter(coef %in% target_coefs) %>%
  # Ensure the correct ordering by matching names:
  mutate(path = target_paths[match(coef, target_coefs)]) %>%
  rename(estimate = Estimate, ci_low = `l-95% CI`, ci_high = `u-95% CI`, est.error = Est.Error) %>%
  mutate(across(c(estimate, ci_low, ci_high, est.error), ~ round(., 3)))

# Print estimates for reference
print(estimates)

# Plot the path diagram using ggplot2


# Define node positions for the plot.
node_coords <- tibble(
  name = c("Ice Melt", "Temperature", "Snowmelt", "Budburst", "Bird Abundance"),
  x = c(0, 0, 0, 0, 3.5),
  y = c(6, 2, -2, -6, 0)
)

# Define the edges between nodes.
edges <- tibble(
  from = c("Ice Melt", "Temperature", "Snowmelt", "Snowmelt", "Budburst", "Ice Melt", "Temperature"),
  to   = c("Temperature", "Snowmelt", "Budburst", "Bird Abundance", "Bird Abundance", "Bird Abundance", "Bird Abundance")
) %>%
  mutate(path = paste(from, "→", to))

# Join the node coordinates to get starting and ending positions:
edges <- edges %>%
  left_join(node_coords, by = c("from" = "name")) %>%
  rename(x_from = x, y_from = y) %>%
  left_join(node_coords, by = c("to" = "name")) %>%
  rename(x_to = x, y_to = y) %>%
  # Compute midpoint for label placement (with a slight vertical offset)
  mutate(x_mid = (x_from + x_to) / 2,
         y_mid = (y_from + y_to) / 2) #+ 0.15)

# Join the coefficient estimates to the corresponding edge using the path label.
edges <- edges %>%
  left_join(estimates, by = "path")

ggplot() +
  # Draw the edges (arrows) with width proportional to the absolute estimate
  geom_segment(
    data = edges,
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to,
      size = abs(estimate)
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    lineend = "round"
  ) +
  # Adjust the range of line widths to your preference
  scale_size_continuous(range = c(0.5, 3)) +
  # Add coefficient labels on the edges (if available)
  geom_label(
    data = edges,
    aes(
      x = x_mid,
      y = y_mid,
      label = ifelse(
        is.na(estimate),
        "",
        paste0(#"β = ", 
               estimate, 
               "\n[", ci_low, ", ", ci_high, "]")
      )
    ),
    size = 4,
    color = "darkred",
    fill = "white",
    label.size = 0,
    vjust = .75
  ) +
  # Draw the nodes
  # Square points using shape = 15 (filled square)
  # geom_point(data = node_coords, aes(x = x, y = y), shape = 15, size = 27, color = "grey") + 
  # # Adding text labels
  # geom_text(data = node_coords, aes(x = x, y = y ,label = str_wrap(name, width = 10)), vjust = 0.4, color = "black", size = 4) +
  # Draw the nodes using geom_labels (note that these are not squares)
  geom_label(data = node_coords,
             aes(x = x, y = y, label = name),
             fill = "grey",      # background color of the label
             color = "black",       # text color
             size = 5,              # text size
             label.size = 1,
             label.padding = unit(c(0.4, 0.1, 0.4, 0.1), "cm"),
             label.r = unit(0, "pt")) +
  theme_void() +
  theme(legend.position = "none") +
  xlim(-1, 4.2) +
  ylim(-6.5, 6.5)
