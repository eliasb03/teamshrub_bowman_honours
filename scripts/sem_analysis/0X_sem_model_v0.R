#------------------------------
# teamshrub_bowman_honours
# 0X_sem_model_v0
# By: Elias Bowman 
# Created: 2024-10-16
# Last update: 2024-10-16
# 
# Description: This script will construct and run SEM models for my analysis
# At this time (16Oct2024) I am not ready to run my model at all, but I think it is valuable to make a code framework to do so 
#------------------------------

library(piecewiseSEM)
library(brms)
library(semPlot)
library(DiagrammeR)

# Create a dummy dataframe ####
set.seed(123)  # For reproducibility
dummy_num_years <- 25
dummy_sem_data <- data.frame(
  ice_melt_date = rnorm(dummy_num_years, mean = 120, sd = 10),  # e.g., Julian date of ice melt
  bird_abundance = rnorm(dummy_num_years, mean = 30, sd = 5),   # e.g., count of birds observed
  spring_temp = rnorm(dummy_num_years, mean = 5, sd = 2),       # e.g., spring temperature
  snowmelt_date = rnorm(dummy_num_years, mean = 135, sd = 10),  # e.g., Julian date of snowmelt
  salix_budburst = rnorm(dummy_num_years, mean = 150, sd = 8)   # e.g., Julian date of budburst for Salix arctica
)


# SEM Proposed Structure ####
# bird_abundance ~ ice_melt_date + spring_temp + snowmelt_date + salix_budburst
# salix_budburst ~ snowmelt_date + spring_temp
# snowmelt_date ~ spring_temp
# spring_temp ~ ice_melt_date

# Define model formulas
model_formulas <- list(
    bird_abundance = bird_abundance ~ ice_melt_date + spring_temp + snowmelt_date + salix_budburst,
    salix_budburst = salix_budburst ~ snowmelt_date + spring_temp,
    snowmelt_date = snowmelt_date ~ spring_temp,
    spring_temp = spring_temp ~ ice_melt_date
  )

# Building Piecewise SEM Model ####
bird_abundance_psem <- psem(
  lm(bird_abundance ~ ice_melt_date + spring_temp + snowmelt_date + salix_budburst, data = dummy_sem_data),
  lm(salix_budburst ~ snowmelt_date + spring_temp, data = dummy_sem_data),
  lm(snowmelt_date ~ spring_temp, data = dummy_sem_data),
  lm(spring_temp ~ ice_melt_date, data = dummy_sem_data),
  data = dummy_sem_data
)

# Building BRMS Bayseian SEM
bird_abundance_mod <- bf(bird_abundance ~ ice_melt_date + spring_temp + snowmelt_date + salix_budburst)
salix_budburst_mod <- bf(salix_budburst ~ snowmelt_date + spring_temp)
snowmelt_date_mod <- bf(snowmelt_date ~ spring_temp)
spring_temp_mod <- bf(spring_temp ~ ice_melt_date)

abundance_fit_brms <- brm(bird_abundance_mod +
                    salix_budburst_mod + 
                    snowmelt_date_mod + 
                    spring_temp_mod,
                    set_rescor(FALSE), 
                  data = dummy_sem_data,
                  cores=4, chains =2) # orignal code used 4 cores, 2 chains


pairs(abundance_fit_brms)

# Test Plotting #########
# summary(bird_abundance_psem)
# cov(dummy_sem_data)
plot(
  bird_abundance_psem,
  return = FALSE,
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "white"),
  edge_attrs = data.frame(style = "solid", color = "black"),
  ns_dashed = T,
  alpha = 0.05,
  show = "std",
  digits = 3,
  add_edge_label_spaces = TRUE
)

# plot(bird_abundance_psem,
# node_attrs = list(
#   shape = "rectangle", color = "black",
#   fillcolor = "orange", x = 3, y=1:5)
# )
# 
# 
# plot(
#   bird_abundance_psem,
#   node_attrs = list(
#     shape = "rectangle", # All nodes as rectangles
#     color = "black",     # Black borders
#     fillcolor = "orange" # Orange fill
#   ),
#   edge_attrs = list(
#     color = "black",     # Black edges
#     penwidth = 2         # Thicker lines
#   ),
#   layout = "neato",        # Layout style (e.g., dot, neato, circo)
# )
 
# grViz("
# digraph SEM {
#   # Define node styles and positions
#   node [shape = rectangle, style = filled, fillcolor = orange, color = black]
#   X1 [label = 'X1', pos = '-1,1!']
#   X2 [label = 'X2', pos = '-1,0!']
#   X3 [label = 'X3', pos = '-1,-1!']
#   
#   node [shape = ellipse, style = filled, fillcolor = lightblue]
#   Response [label = 'Response\nVariable', pos = '0,0!']
# 
#   # Edges
#   X1 -> Response
#   X2 -> Response
#   X3 -> Response
# 
#   # Graph settings
#   graph [rankdir = LR, splines = true]
# }
# ")

#############
# Import Packages 
library(lavaan)

# Define Data Set
#model_dataframe <- NAME OF DATAFRAME MADE FOR SEM MODEL
# 
# # Define the Model
# # Path analysis model (no latent variables)
path_model <- '
  # Regressions (relationships between observed variables)
  ice_melt_date ~ bird_abundance
  ice_melt_date ~ spring_temp
  spring_temp ~ bird_abundance
  spring_temp ~ snowmelt_date
  spring_temp ~ salix_budburst
  snowmelt_date ~ bird_abundance
  snowmelt_date ~ salix_budburst
  salix_budburst ~ bird_abundance
'


# Create a dummy dataframe
set.seed(123)  # For reproducibility
dummy_sem_data <- data.frame(
  ice_melt_date = rnorm(10, mean = 150, sd = 10),  # e.g., Julian date of ice melt
  bird_abundance = rnorm(10, mean = 30, sd = 5),   # e.g., count of birds observed
  spring_temp = rnorm(10, mean = 5, sd = 2),       # e.g., spring temperature
  snowmelt_date = rnorm(10, mean = 140, sd = 10),  # e.g., Julian date of snowmelt
  salix_budburst = rnorm(10, mean = 135, sd = 8)   # e.g., Julian date of budburst for Salix arctica
)

# # Define the SEM model
# sem_model <- '
#   ice_melt_date ~ bird_abundance + spring_temp
#   spring_temp ~ bird_abundance + snowmelt_date + salix_budburst
#   snowmelt_date ~ bird_abundance + salix_budburst
# '

# Fit the model
fit <- sem(path_model, data = dummy_sem_data)

# Plot the SEM structure
semPaths(
  fit,
  what = "paths",
  layout = "tree",
  edge.width = 3,
  edge.color = "black",
  edge.label.cex = 1.2,
  sizeMan = 10,
  color = "darkgreen",
  title = FALSE,
  curvePivot = TRUE,
  label.cex = 4,
  fade = FALSE,
  label.prop = 0.3,
  rescale = FALSE,
  style = "RAM"
)

#
# # SEM model (includes latent variables)
# sem_model <- '
#   # Measurement model
#   latent_var1 =~ indicator1 + indicator2 + indicator3
#   latent_var2 =~ indicator4 + indicator5
#
#   # Regressions
#   latent_var2 ~ latent_var1
#   outcome_var ~ latent_var2
#
#   # Covariances (if applicable)
#   latent_var1 ~~ latent_var2
# '
# 
# # Fit the model to the data
# fit <- sem(path_model, data = model_dataframe)
# 
# # Summary of the model results
# summary(fit, fit.measures = TRUE, standardized = TRUE)

