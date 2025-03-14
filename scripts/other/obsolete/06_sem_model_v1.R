#------------------------------
# teamshrub_bowman_honours
# 06_bbs_sem_model_v1
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
  
  # Random species name from 16 possible options
  species = sample(
    c("Species A", "Species B", "Species C", "Species D", "Species E", "Species F",  # Guild 1 (6 species)
      "Species G", "Species H", "Species I", "Species J", "Species K", "Species L",  # Guild 2 (6 species)
      "Species M", "Species N", "Species O", "Species P"),                          # Guild 3 (4 species)
    n_rows, replace = TRUE
  ),
  
  # Assign guild based on species
  guild = case_when(
    species %in% c("Species A", "Species B", "Species C", "Species D", "Species E", "Species F") ~ "Guild 1",
    species %in% c("Species G", "Species H", "Species I", "Species J", "Species K", "Species L") ~ "Guild 2",
    species %in% c("Species M", "Species N", "Species O", "Species P") ~ "Guild 3",
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

model_summary <- summary(guild_bsem)

fixed_effects <- model_summary$fixed %>%
  mutate(coef = rownames(.)) %>%
  select(coef, Estimate, Est.Error, `l-95% CI`, `u-95% CI`) %>%
  filter(!grepl(":guild", coef))  # Remove interaction

# Define the coefficients corresponding to each desired path.
target_coefs <- c("breedingtemp_icemelt", "snowmelt_breedingtemp", "budburst_snowmelt", 
                  "birdabundance_snowmelt", "birdabundance_budburst", 
                  "birdabundance_icemelt", "birdabundance_breedingtemp")

target_paths <- c("Ice Melt → Spring Temperature", 
                  "Spring Temperature → Snowmelt", 
                  "Snowmelt → Budburst", 
                  "Snowmelt → Bird Abundance", 
                  "Budburst → Bird Abundance", 
                  "Ice Melt → Bird Abundance", 
                  "Spring Temperature → Bird Abundance")

# Create a tibble with the estimates for only these coefficients.
estimates <- fixed_effects %>%
  filter(coef %in% target_coefs) %>%
  # Ensure the correct ordering by matching names:
  mutate(path = target_paths[match(coef, target_coefs)]) %>%
  rename(estimate = Estimate, ci_low = `l-95% CI`, ci_high = `u-95% CI`, est.error = Est.Error) %>%
  mutate(across(c(estimate, ci_low, ci_high, est.error), ~ round(., 3)))

# Define node positions for the plot.
node_coords <- tibble(
  name = c("Ice Melt", "Spring Temperature", "Snowmelt", "Budburst", "Bird Abundance"),
  x = c(0, 0, 0, 0, 3.5),
  y = c(6, 2, -2, -6, 0)
)

# Define the edges between nodes.
edges <- tibble(
  from = c("Ice Melt", "Spring Temperature", "Snowmelt", "Snowmelt", "Budburst", "Ice Melt", "Spring Temperature"),
  to   = c("Spring Temperature", "Snowmelt", "Budburst", "Bird Abundance", "Bird Abundance", "Bird Abundance", "Bird Abundance")
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
  geom_segment(
    data = edges,
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to,
      size = abs(estimate),
      color = ifelse(estimate > 0, "positive", "negative")  # Color by sign
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    lineend = "round"
  ) +
  scale_size_continuous(range = c(0.5, 3)) +
  scale_color_manual(values = c("positive" = "blue", "negative" = "red")) +  # Define colors
  geom_label(
    data = edges,
    aes(
      x = x_mid,
      y = y_mid,
      label = ifelse(
        is.na(estimate),
        "",
        paste0(estimate, "\n[", ci_low, ", ", ci_high, "]")
      )
    ),
    size = 4,
    color = "darkred",
    fill = "white",
    label.size = 0,
    vjust = .75
  ) +
  geom_label(data = node_coords,
             aes(x = x, y = y, label = name),
             fill = "grey",
             color = "black",
             size = 5,
             label.size = 1,
             label.padding = unit(c(0.4, 0.1, 0.4, 0.1), "cm"),
             label.r = unit(0, "pt")) +
  theme_void() +
  theme(legend.position = "none") +
  xlim(-1, 4.2) +
  ylim(-6.5, 6.5)

##############################
node_coords <- tibble(
  name = c("Ice Melt", "Spring Temperature", "Snowmelt", "Budburst", "Bird Abundance", "Region Temp"),
  x = c(0, 0, 0, 0, 4.5, -4.5),
  y = c(6, 2, -2, -6, 0, 0)
) %>%
  mutate(name = case_when(
    name == "Ice Melt" ~ "Ice Melt",
    name == "Spring Temperature" ~ "Spring\nTemperature",
    name == "Snowmelt" ~ "Snowmelt",
    name == "Budburst" ~ "Budburst",
    name == "Bird Abundance" ~ "Bird\nAbundance",
    name == "Region Temp" ~ "Regional\nTemperature",
    TRUE ~ name
  ))

# Plot the graph with modified labels
ggplot() +
  geom_segment(
    data = edges,
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to,
      size = abs(estimate),
      color = ifelse(estimate > 0, "positive", "negative")
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    lineend = "round"
  ) +
  scale_size_continuous(range = c(0.5, 3)) +
  scale_color_manual(values = c("positive" = "blue", "negative" = "red")) +
  geom_label(
    data = edges,
    aes(
      x = x_mid,
      y = y_mid,
      label = ifelse(
        is.na(estimate),
        "",
        paste0(estimate, "\n[", ci_low, ", ", ci_high, "]")
      )
    ),
    size = 4,
    color = "darkred",
    fill = "white",
    label.size = 0,
    vjust = .75
  ) +
  geom_label(data = node_coords,
             aes(x = x, y = y, label = name),
             fill = "white",#"grey",
             color = "black",
             size = 5,
             label.size = 1,
             label.padding = unit(c(0.4, 0.1, 0.4, 0.1), "cm"),
             label.r = unit(0, "pt")) +
  theme_void() +
  theme(legend.position = "none") +
  xlim(-6, 6) +
  ylim(-6.5, 6.5)














###################
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


##############################


library(lavaan)
library(semPlot)

# Define your SEM model
lavaan_model <- "
  birdabundance ~ icemelt + breedingtemp + snowmelt + budburst
  budburst ~ snowmelt + breedingtemp + regiontemp
  snowmelt ~ breedingtemp + regiontemp
  breedingtemp ~ icemelt + regiontemp
  icemelt ~ regiontemp
"

# Create a labeled covariance matrix for the variables
cov_matrix <- diag(6)
colnames(cov_matrix) <- rownames(cov_matrix) <- c("birdabundance", "icemelt", "breedingtemp", "snowmelt", "budburst", "regiontemp")

# Create a fake model with specified path coefficients using lavaanify
fit <- lavaan(model = lavaan_model, sample.cov = cov_matrix, sample.nobs = 100, fixed.x = TRUE, do.fit = FALSE)

# Plot the model without variances
semPaths(fit, 
         what = "path",          # Show path coefficients
         whatLabels = "est",     # Display the exact coefficient values
         layout = "tree",        # Tree layout for clarity
         edge.label.cex = 1.2,   # Size of path coefficient labels
         sizeMan = 10,           # Size of manifest variables
         sizeLat = 8,            # Size of latent variables (if any)
         color = "lightblue",    # Node color
         edge.color = "black",   # Path color
         style = "lisrel",       # Classic SEM style
         curve = 0.5,            # Curve for covariances
         residuals = FALSE)      # Hides variances



###########
# # Define your SEM model
# lavaan_model <- "
#   passerineabundance ~ icemelt + breedingtemp + snowmelt + budburst
#   shorebirdabundance ~ icemelt + breedingtemp + snowmelt + budburst
#   waterbirdabundance ~ icemelt + breedingtemp + snowmelt + budburst
#   budburst ~ snowmelt + breedingtemp + regiontemp
#   snowmelt ~ breedingtemp + regiontemp
#   breedingtemp ~ icemelt + regiontemp
#   icemelt ~ regiontemp
# "
# 
# # Create a labeled covariance matrix for the variables
# cov_matrix <- diag(8)
# colnames(cov_matrix) <- rownames(cov_matrix) <- c("passerineabundance", "shorebirdabundance", "waterbirdabundance", "icemelt", "breedingtemp", "snowmelt", "budburst", "regiontemp")
# 
# # Create a fake model with specified path coefficients using lavaanify
# fit <- lavaan(model = lavaan_model, sample.cov = cov_matrix, sample.nobs = 100, fixed.x = TRUE, do.fit = FALSE)

# Define your SEM model with specific path coefficients
lavaan_model <- "
  passerineabundance ~ -1*icemelt + 0.3*breedingtemp + 0.2*snowmelt + 0.4*budburst
  shorebirdabundance ~ 0.6*icemelt + -0.2*breedingtemp + 0.1*snowmelt + 0.3*budburst
  waterbirdabundance ~ 0.4*icemelt + 0.5*breedingtemp + 0.2*snowmelt + 0.3*budburst
  budburst ~ 0.6*snowmelt + 0.5*breedingtemp + 0.7*regiontemp
  snowmelt ~ 0.5*breedingtemp + 0.6*regiontemp
  breedingtemp ~ 0.4*icemelt + 0.3*regiontemp
  icemelt ~ 0.3*regiontemp
"

# Create a labeled covariance matrix for the variables
cov_matrix <- diag(8)
colnames(cov_matrix) <- rownames(cov_matrix) <- c("passerineabundance", "shorebirdabundance", "waterbirdabundance", "icemelt", "breedingtemp", "snowmelt", "budburst", "regiontemp")

# Create a fake model with specified path coefficients using lavaanify
fit <- lavaan(model = lavaan_model, sample.cov = cov_matrix, sample.nobs = 100, fixed.x = TRUE, do.fit = FALSE)


# Define a custom layout for the SEM plot
custom_layout <- matrix(c(
  # # option 1
  # 5.5, 4.5,     # passerineabundance
  # 7, 9,   # shorebirdabundance
  # 4, 0,     # waterbirdabundance
  # # option 2
  # 5, 9,     # passerineabundance
  # 6.5, 4.5,   # shorebirdabundance
  # 5, 0,     # waterbirdabundance
  # # option 3
  5, 9,     # passerineabundance
  6.5, 4.5,   # shorebirdabundance
  4, 0,     # waterbirdabundance
  2, 0,     # budburst
  2, 3,     # snowmelt
  2, 6,     # breedingtemp
  2, 9,     # icemelt
  0, 4.5    # regiontemp 
), ncol = 2, byrow = TRUE)

# Define full names for each variable
full_names <- c(
  "Passerine\nAbundance" = "passerineabundance",
  "Shorebird\nAbundance" = "shorebirdabundance",
  "Waterbird\nAbundance" = "waterbirdabundance",
#  "Bird\nAbundance" = "birdabundance",
  "Budburst" = "budburst",
  "Snowmelt" = "snowmelt",
  "Breeding\nTemp" = "breedingtemp",
  "Ice Melt" = "icemelt",
  "Region\nTemp" = "regiontemp"
)

# Scale the edge widths by the absolute value of the effect sizes (coefficients)
path_coefs <- parameterEstimates(fit)
edge_colors <- ifelse(path_coefs$est > 0, "blue", "red")  # Blue for positive, red for negative

# Plot the model with custom colors for positive and negative effects
test <- semPaths(
  fit,
  what = "par",
  whatLabels = "est",
  layout = custom_layout,
  edge.label.cex = 1,
  nodeLabels = names(full_names),
  sizeMan = 7,
  color = "white",
  edge.color = edge_colors,  # Use custom edge colors
  style = "lisrel",
  curve = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, 0, 0),
  fade = FALSE,
  residuals = FALSE,
  fixedStyle = 1
)



# test$graphAttributes$Edges$lty <- c(rep(1, 20)) # setting all lines solid
# plot(test)
# library(qgraph)
# qgraph(test, lty = 1)
