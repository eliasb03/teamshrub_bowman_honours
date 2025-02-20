


time_series_priors

time_series_model <- brm( 
  formula = calling frequency ~ temperature + wind speed + time + (1 | site),  
  data = times_series_data,
  family = response distribution ,  
  prior = time_series_priors,
  chains = 4, 
  iter = 2000,  
  warmup = 1000 
)


autocor = cor_ar(~ time | site),  # Account for temporal autocorrelation within sites 