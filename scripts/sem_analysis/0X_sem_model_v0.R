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

# Import Packages 
library(lavaan)
library(semPlot)

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

