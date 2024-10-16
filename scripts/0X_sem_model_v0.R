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

# Define the Model
# Path analysis model (no latent variables)
path_model <- '
  # Regressions (relationships between observed variables)
  var2 ~ var1      # var1 predicts var2
  var3 ~ var2      # var2 predicts var3
  var3 ~ var1      # var1 predicts var3
'

# SEM model (includes latent variables)
sem_model <- '
  # Measurement model
  latent_var1 =~ indicator1 + indicator2 + indicator3
  latent_var2 =~ indicator4 + indicator5

  # Regressions
  latent_var2 ~ latent_var1
  outcome_var ~ latent_var2

  # Covariances (if applicable)
  latent_var1 ~~ latent_var2
'

# Fit the model to the data
fit <- sem(path_model, data = model_dataframe)

# Summary of the model results
summary(fit, fit.measures = TRUE, standardized = TRUE)

