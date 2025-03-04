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
library(sjPlot)

# Importing Data ####
sem_data <- read_csv("data/clean/sem/sem_data.csv") %>%
  filter(guild != "birdofprey") %>%
  filter(bird.abundance > 0)


# Filtering Data based on Guild ####
passerine_data <- sem_data %>%
  filter(guild == "passerine") %>%  
  filter(bird.abundance > 0)

shorebird_data <- sem_data %>%
  filter(guild == "shorebird")

waterbird_data <- sem_data %>%
  filter(guild == "waterbird")

# Constructing Model ####
bird_mod <- bf(bird.abundance ~ ice.melt + temp + snowmelt + budburst
               + (1|species) + (1 | year)
               , family = poisson())#zero_one_inflated_beta()) 
budburst_mod <- bf(budburst ~ snowmelt + temp) #+ (1 | year))
snowmelt_mod <- bf(snowmelt ~ temp) #+ (1 | year))
temp_mod <- bf(temp ~ ice.melt) #+ (1 | year))


# Setting priors
abundance_priors_scaled <- c(
  # Bird abundance model priors
    # priors tighter when using poisson dist, because of log link function, prior to log was 0, 20
  set_prior("normal(0, 5)", coef = "ice.melt", resp = "birdabundance"), 
  set_prior("normal(0, 5)", coef = "temp", resp = "birdabundance"),
  set_prior("normal(0, 5)", coef = "snowmelt", resp = "birdabundance"),
  set_prior("normal(0, 5)", coef = "budburst", resp = "birdabundance"),
  
  # Budburst model priors
  set_prior("normal(0, 2)", coef = "snowmelt", resp = "budburst"),
  set_prior("normal(0, 4)", coef = "temp", resp = "budburst"), # 5 degree increase in snowmelt leads to a X week increase in budburst
  
  # Snowmelt model priors
  set_prior("normal(0, 4)", coef = "temp", resp = "snowmelt"),
  
  # Temp model priors
  set_prior("normal(0, 4)", coef = "ice.melt", resp = "temp")
)

passerine_bsem <- brm(
  bird_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
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
  bird_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
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




summary(passerine_bsem)
summary(shorebird_bsem)
plot(passerine_bsem)

plot_model(passerine_bsem, type = "pred", terms = "ice.melt")
exp(3.42 + (-0.05*-5))


bird_mod <- bf(bird.abundance ~ (ice.melt + temp + snowmelt + budburst) * guild
               + (1|species) 
               + (1 | year) # decide if guild observations are dependent between years, wouldn't be doing this if i did 3 seperate models anyway
               , family = poisson())
               #, family = zero_one_inflated_beta())
budburst_mod <- bf(budburst ~ snowmelt + temp) #+ (1 | year))
snowmelt_mod <- bf(snowmelt ~ temp) #+ (1 | year))
temp_mod <- bf(temp ~ ice.melt) #+ (1 | year))

guild_bsem <- brm(
  bird_mod +
    budburst_mod +
    snowmelt_mod +
    temp_mod +
    set_rescor(FALSE),
  data = sem_data,
  prior = abundance_priors_scaled,
  cores = 4,
  chains = 3,
  iter = 4000,
  warmup = 1000,
  thin = 2,
  control=list(adapt_delta=0.9999, max_treedepth=15)
)

summary(guild_bsem)
plot(guild_bsem)
plot_model(guild_bsem, type = "pred", terms = c("snowmelt", "guild"))
plot_model(guild_bsem, type = "pred", terms = c("ice.melt", "guild"))
plot_model(guild_bsem, type = "pred", terms = c("temp", "guild"))
plot_model(guild_bsem, type = "pred", terms = c("budburst", "guild"))


plot_model(shorebird_bsem, type = "pred", terms = "temp")



# TO DO:
# Confirm final interaction model
# 1. Develop a new dataframe with values for each guild summed based on different of guilds
# 2. Create a figure from the interaction model





# Run a model and confirm that time is changing the predictors 
# they are not randomly distributed within the study period









# # Extract model estimates (posterior means and credible intervals)
# model_summary <- summary(passerine_bsem)
# 
# fixed_effects <- model_summary$fixed %>%
#   mutate(coef = rownames(.)) %>%
#   select(coef, Estimate, Est.Error, `l-95% CI`, `u-95% CI`)
# 
# # Define the coefficients corresponding to each desired path.
# target_coefs <- c("temp_ice.melt", "snowmelt_temp", "budburst_snowmelt", 
#                   "birdabundance_snowmelt", "birdabundance_budburst", 
#                   "birdabundance_ice.melt", "birdabundance_temp")
# 
# target_paths <- c("Ice Melt → Temperature", 
#                   "Temperature → Snowmelt", 
#                   "Snowmelt → Budburst", 
#                   "Snowmelt → Bird Abundance", 
#                   "Budburst → Bird Abundance", 
#                   "Ice Melt → Bird Abundance", 
#                   "Temperature → Bird Abundance")
# 
# # Create a tibble with the estimates for only these coefficients.
# estimates <- fixed_effects %>%
#   filter(coef %in% target_coefs) %>%
#   # Ensure the correct ordering by matching names:
#   mutate(path = target_paths[match(coef, target_coefs)]) %>%
#   rename(estimate = Estimate, ci_low = `l-95% CI`, ci_high = `u-95% CI`, est.error = Est.Error) %>%
#   mutate(across(c(estimate, ci_low, ci_high, est.error), ~ round(., 3)))
# 
# # Print estimates for reference
# print(estimates)
# 
# # Plot the path diagram using ggplot2
# 
# 
# # Define node positions for the plot.
# node_coords <- tibble(
#   name = c("Ice Melt", "Temperature", "Snowmelt", "Budburst", "Bird Abundance"),
#   x = c(0, 0, 0, 0, 3.5),
#   y = c(6, 2, -2, -6, 0)
# )
# 
# # Define the edges between nodes.
# edges <- tibble(
#   from = c("Ice Melt", "Temperature", "Snowmelt", "Snowmelt", "Budburst", "Ice Melt", "Temperature"),
#   to   = c("Temperature", "Snowmelt", "Budburst", "Bird Abundance", "Bird Abundance", "Bird Abundance", "Bird Abundance")
# ) %>%
#   mutate(path = paste(from, "→", to))
# 
# # Join the node coordinates to get starting and ending positions:
# edges <- edges %>%
#   left_join(node_coords, by = c("from" = "name")) %>%
#   rename(x_from = x, y_from = y) %>%
#   left_join(node_coords, by = c("to" = "name")) %>%
#   rename(x_to = x, y_to = y) %>%
#   # Compute midpoint for label placement (with a slight vertical offset)
#   mutate(x_mid = (x_from + x_to) / 2,
#          y_mid = (y_from + y_to) / 2) #+ 0.15)
# 
# # Join the coefficient estimates to the corresponding edge using the path label.
# edges <- edges %>%
#   left_join(estimates, by = "path")
# 
# ggplot() +
#   # Draw the edges (arrows) with width proportional to the absolute estimate
#   geom_segment(
#     data = edges,
#     aes(
#       x = x_from,
#       y = y_from,
#       xend = x_to,
#       yend = y_to,
#       size = abs(estimate)
#     ),
#     arrow = arrow(length = unit(0.2, "cm")),
#     lineend = "round"
#   ) +
#   # Adjust the range of line widths to your preference
#   scale_size_continuous(range = c(0.5, 3)) +
#   # Add coefficient labels on the edges (if available)
#   geom_label(
#     data = edges,
#     aes(
#       x = x_mid,
#       y = y_mid,
#       label = ifelse(
#         is.na(estimate),
#         "",
#         paste0(#"β = ", 
#           estimate, 
#           "\n[", ci_low, ", ", ci_high, "]")
#       )
#     ),
#     size = 4,
#     color = "darkred",
#     fill = "white",
#     label.size = 0,
#     vjust = .75
#   ) +
#   # Draw the nodes
#   # Square points using shape = 15 (filled square)
#   # geom_point(data = node_coords, aes(x = x, y = y), shape = 15, size = 27, color = "grey") + 
#   # # Adding text labels
#   # geom_text(data = node_coords, aes(x = x, y = y ,label = str_wrap(name, width = 10)), vjust = 0.4, color = "black", size = 4) +
#   # Draw the nodes using geom_labels (note that these are not squares)
#   geom_label(data = node_coords,
#              aes(x = x, y = y, label = name),
#              fill = "grey",      # background color of the label
#              color = "black",       # text color
#              size = 5,              # text size
#              label.size = 1,
#              label.padding = unit(c(0.4, 0.1, 0.4, 0.1), "cm"),
#              label.r = unit(0, "pt")) +
#   theme_void() +
#   theme(legend.position = "none") +
#   xlim(-1, 4.2) +
#   ylim(-6.5, 6.5)



plot_bsem_paths <- function(
    model,
    target_coefs = c(
      "temp_ice.melt",
      "snowmelt_temp",
      "budburst_snowmelt",
      "birdabundance_snowmelt",
      "birdabundance_budburst",
      "birdabundance_ice.melt",
      "birdabundance_temp"
    )
    ,
    target_paths = c(
      "Ice Melt → Temperature",
      "Temperature → Snowmelt",
      "Snowmelt → Budburst",
      "Snowmelt → Bird Abundance",
      "Budburst → Bird Abundance",
      "Ice Melt → Bird Abundance",
      "Temperature → Bird Abundance"
    ),
    node_coords = tibble(
      name = c(
        "Ice Melt",
        "Temperature",
        "Snowmelt",
        "Budburst",
        "Bird Abundance"
      ),
      x = c(0, 0, 0, 0, 3.5),
      y = c(6, 2, -2, -6, 0)
    )
) {
  # Extract model estimates (posterior means and credible intervals)
  model_summary <- summary(model)
  
  fixed_effects <- model_summary$fixed %>%
    mutate(coef = rownames(.)) %>%
    select(coef, Estimate, Est.Error, `l-95% CI`, `u-95% CI`)
  
  # Create a tibble with the estimates for only these coefficients.
  estimates <- fixed_effects %>%
    filter(coef %in% target_coefs) %>%
    mutate(path = target_paths[match(coef, target_coefs)]) %>%
    rename(estimate = Estimate, ci_low = `l-95% CI`, ci_high = `u-95% CI`, est.error = Est.Error) %>%
    mutate(across(c(estimate, ci_low, ci_high, est.error), ~ round(., 3)))
  
  # Define the edges between nodes.
  edges <- tibble(
    from = c("Ice Melt", "Temperature", "Snowmelt", "Snowmelt", "Budburst", "Ice Melt", "Temperature"),
    to   = c("Temperature", "Snowmelt", "Budburst", "Bird Abundance", "Bird Abundance", "Bird Abundance", "Bird Abundance")
  ) %>%
    mutate(path = paste(from, "→", to))
  
  # Join the node coordinates to get starting and ending positions
  edges <- edges %>%
    left_join(node_coords, by = c("from" = "name")) %>%
    rename(x_from = x, y_from = y) %>%
    left_join(node_coords, by = c("to" = "name")) %>%
    rename(x_to = x, y_to = y) %>%
    mutate(x_mid = (x_from + x_to) / 2, y_mid = (y_from + y_to) / 2)
  
  # Join the coefficient estimates to the corresponding edge using the path label
  edges <- edges %>%
    left_join(estimates, by = "path")
  
  # Create the plot
  ggplot() +
    geom_segment(
      data = edges,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to, size = abs(estimate)),
      arrow = arrow(length = unit(0.2, "cm")),
      lineend = "round"
    ) +
    scale_size_continuous(range = c(0.5, 3)) +
    geom_label(
      data = edges,
      aes(x = x_mid, y = y_mid, label = ifelse(is.na(estimate), "", paste0(estimate, "\n[", ci_low, ", ", ci_high, "]"))),
      size = 4,
      color = "darkred",
      fill = "white",
      label.size = 0,
      vjust = .75
    ) +
    geom_label(
      data = node_coords,
      aes(x = x, y = y, label = name),
      fill = "grey",
      color = "black",
      size = 5,
      label.size = 1,
      label.padding = unit(c(0.4, 0.1, 0.4, 0.1), "cm"),
      label.r = unit(0, "pt")
    ) +
    theme_void() +
    theme(legend.position = "none") +
    xlim(-1, 4.2) +
    ylim(-6.5, 6.5)
}
