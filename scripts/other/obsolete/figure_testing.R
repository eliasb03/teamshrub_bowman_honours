






lavaan_model <- "
  passerineabundance ~ 0.5*icemelt + 0.3*breedingtemp + 0.2*snowmelt + 0.4*budburst
  shorebirdabundance ~ 0.6*icemelt + 0.2*breedingtemp + 0.1*snowmelt + 0.3*budburst
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



# # # guildsummed_bsem_effect
# # # 
# # # 
# # # passerine_bsem_effect_test <- passerine_bsem_effect %>%
# # #   mutate(
# # #     predictor = case_when(
# # #       predictor == "Intercept" ~ response,
# # #       TRUE ~ predictor
# # #     )
# # #   )
# # # 
# # # post_summ <- fixef(passerine_bsem)  
# # # View(post_summ)
# # # View(parameterEstimates(lavaan_fit))
# # # View(parameterEstimates(lavaan_fit)$est)
# # # parameterEstimates(lavaan_fit)$est <- post_summ[, "Estimate"]
# # # test <- parameterEstimates(lavaan_fit)
# # # test <- parameterEstimates(lavaan_fit) %>%
# # #   left_join(passerine_bsem_effect_test, by = c("lhs" = "response", "rhs" = "predictor")) %>%
# # #   select(-est) %>%
# # #   rename("est" = "estimate")
# # # 
# # # parameterEstimates(lavaan_fit)$est <- test$est
# # # 
# # # # Get variable order from lavaan model
# # # lavaan_fit_vars <- lavNames(lavaan_fit, type = "ov")
# # # 
# # # # Print the variable names in order
# # # print(lavaan_fit_vars)
# # # 
# # # #########
# # # # Assuming you have already fitted your lavaan model
# # # # lavaan_fit <- sem(your_model, data = your_data)
# # # 
# # # library(lavaan)
# # # library(semPlot)
# # # 
# # # # Assuming lavaan_fit is your fitted lavaan model
# # # # Extract parameter estimates and their labels
# # # # Check the parameter labels from the fitted model
# # # param_estimates <- parameterEstimates(lavaan_fit)
# # # print(param_estimates$label)  # Check the labels
# # # 
# # # # Create the new estimates vector again and print to verify
# # # new_estimates <- setNames(test$est, paste(test$lhs, test$op, test$rhs, sep = "_"))
# # # print(names(new_estimates))  # Check the names in new_estimates
# # # 
# # # # Match and assign new estimates, check for NAs in matches
# # # matched_values <- new_estimates[match(param_estimates$label, names(new_estimates))]
# # # print(matched_values)  # Check the matched values before assignment
# # # 
# # # # Assign the matched values back to param_estimates
# # # param_estimates$est <- matched_values
# # # 
# # # # If there are NA values, you may want to investigate further
# # # if (any(is.na(param_estimates$est))) {
# # #   warning("Some estimates could not be matched and are NA.")
# # # }
# # #########
# # 
# # # custom_layout <- matrix(c(
# # #   3, 3.5,   # birdabundance
# # #   1.9, 2,   # budburst
# # #   2.1, 3,   # snowmelt
# # #   1.9, 4,   # breedingtemp
# # #   2.1, 5,   # icemelt
# # #   1, 3.5    # regiontemp 
# # # ), ncol = 2, byrow = TRUE)
# # # Function to fit the lavaan model with specified estimates from passerine_bsem_effect
# # # Function to fit the lavaan model with specified estimates from passerine_bsem_effect, ignoring intercepts
# # fit_lavaan_with_passed_estimates <- function(estimates_df, data) {
# #   # Base model components
# #   model_components <- list(
# #     "birdabundance ~ icemelt + breedingtemp + snowmelt + budburst",
# #     "budburst ~ snowmelt + breedingtemp + regiontemp",
# #     "snowmelt ~ breedingtemp + regiontemp",
# #     "breedingtemp ~ icemelt + regiontemp",
# #     "icemelt ~ regiontemp"
# #   )
# #   
# #   # Initialize a list to hold modified model equations
# #   modified_components <- model_components
# #   
# #   # Loop through each row of the estimates_df
# #   for (i in 1:nrow(estimates_df)) {
# #     response <- estimates_df$response[i]
# #     predictor <- estimates_df$predictor[i]
# #     est <- estimates_df$estimate[i]
# #     
# #     # Find the correct component to modify, ignoring intercepts
# #     component_index <- which(grepl(paste0(response, " ~"), modified_components))
# #     
# #     if (length(component_index) == 0) {
# #       next  # Skip if no component found
# #     }
# #     
# #     # Update the model for the predictor with the specified estimate
# #     if (predictor != "Intercept") {
# #       modified_components[[component_index]] <- 
# #         gsub(
# #           paste0("(", predictor, ")(\\s*\\+)?"), 
# #           paste0(est, " * ", predictor, " + "), 
# #           modified_components[[component_index]]
# #         )
# #     }
# #   }
# #   
# #   # Combine the modified model components into a single model string
# #   lavaan_model <- paste(modified_components, collapse = "\n")
# #   
# #   # Fit the model using the modified model syntax
# #   fit <- sem(lavaan_model, data = data)
# #   
# #   return(fit)
# # }
# # 
# # # Example usage with passerine_bsem_effect dataset
# # # Assuming sem_data is your dataset to fit the model
# # fit_result <- fit_lavaan_with_passed_estimates(passerine_bsem_effect, sem_data)
# 
# posterior_samples <- posterior_samples(passerine_bsem)
# 
# # Calculate the mean estimates for paths (you can also use other summaries like median)
# mean_estimates <- posterior_samples %>%
#   summarize(across(everything(), mean))
# 
# # Create a data frame to represent paths
# paths <- data.frame()
# 
# # Add paths for the bird abundance model using mean estimates
# paths <- rbind(paths,
#                data.frame(from = "icemelt", to = "birdabundance", estimate = mean_estimates[["b_birdabundance_icemelt"]]),
#                data.frame(from = "breedingtemp", to = "birdabundance", estimate = mean_estimates[["b_birdabundance_breedingtemp"]]),
#                data.frame(from = "snowmelt", to = "birdabundance", estimate = mean_estimates[["b_birdabundance_snowmelt"]]),
#                data.frame(from = "budburst", to = "birdabundance", estimate = mean_estimates[["b_birdabundance_budburst"]]),
#                data.frame(from = "regiontemp", to = "icemelt", estimate = mean_estimates[["b_icemelt_regiontemp"]]),
#                data.frame(from = "regiontemp", to = "breedingtemp", estimate = mean_estimates[["b_breedingtemp_regiontemp"]]),
#                data.frame(from = "breedingtemp", to = "snowmelt", estimate = mean_estimates[["b_snowmelt_breedingtemp"]]),
#                data.frame(from = "snowmelt", to = "budburst", estimate = mean_estimates[["b_budburst_snowmelt"]]),
#                data.frame(from = "breedingtemp", to = "budburst", estimate = mean_estimates[["b_budburst_breedingtemp"]])
# )
# 
# # Now define the lavaan model syntax string
# model_string <- paste0(
#   "birdabundance ~ icemelt + breedingtemp + snowmelt + budburst;\n",
#   "budburst ~ snowmelt + breedingtemp + regiontemp;\n",
#   "snowmelt ~ breedingtemp + regiontemp;\n",
#   "breedingtemp ~ icemelt + regiontemp;\n",
#   "icemelt ~ regiontemp;"
# )
# 
# # Create a SEM model object using lavaan
# library(lavaan)
# sem_model <- sem(model_string, data = passerine_data)
# 
# beta_matrix <- as.matrix(xtabs(estimate ~ from + to, data = paths))
# sem_model <- setBeta(sem_model, beta_matrix)
# 
# # Manually set the parameter estimates in the SEM model
# sem_model@Model$beta <- as.matrix(paths)
# 
# # Now plot the model using semPlot
# semPaths(sem_model, whatLabels = "est")
# library(DiagrammeR)
# 
# # Define the nodes and edges for the SEM
# nodes <- data.frame(id = c("birdabundance", "budburst", "snowmelt", "breedingtemp", "icemelt", "regiontemp"),
#                     label = c("Bird Abundance", "Budburst", "Snowmelt", "Breeding Temperature", "Ice Melt", "Regional Temperature"),
#                     shape = c("ellipse", "ellipse", "ellipse", "ellipse", "ellipse", "ellipse"))
# 
# edges <- data.frame(from = c("icemelt", "breedingtemp", "snowmelt", "budburst", "breedingtemp", "regiontemp", "icemelt", "snowmelt"),
#                     to = c("birdabundance", "birdabundance", "birdabundance", "budburst", "snowmelt", "icemelt", "regiontemp", "breedingtemp"),
#                     rel = c("regression", "regression", "regression", "regression", "regression", "regression", "regression", "regression"))
# 
# # Create the graph
# graph <- create_graph() %>%
#   add_nodes_from_table(nodes) %>%
#   add_edges_from_table(edges, from_col = from, to_col = to)

# Render the graph
render_graph(graph)
######
library(lavaan)
library(semPlot)
lavaan_model <- "
  birdabundance ~ icemelt + breedingtemp + snowmelt + budburst
  budburst ~ snowmelt + breedingtemp + regiontemp
  snowmelt ~ breedingtemp + regiontemp
  breedingtemp ~ icemelt + regiontemp
  icemelt ~ regiontemp
"
lavaan_fit <- sem(lavaan_model, data = sem_data)
summary(lavaan_fit, standardized = TRUE)

params <- parameterEstimates(lavaan_fit)
str(params)
params[params$label == "your_parameter_label", "est"] <- new_value


custom_layout <- matrix(c(
  4, 3.5,   # birdabundance
  2, 2,   # budburst
  2, 3,   # snowmelt
  2, 4,   # breedingtemp
  2, 5,   # icemelt
  0, 3.5    # regiontemp 
), ncol = 2, byrow = TRUE)


# Define full names for each variable
full_names <- c(
  "Bird\nAbundance" = "birdabundance",
  "Budburst" = "budburst",
  "Snowmelt" = "snowmelt",
  "Breeding\nTemp" = "breedingtemp",
  "Ice Melt" = "icemelt",
  "Region\nTemp" = "regiontemp"
)


semPaths(
  lavaan_fit,
  what = "std",
  layout = custom_layout, 
  edge.label.cex = 1.5,
  nodeLabels = names(full_names),
  curve = c(0, 0, 0, 0, 0, -2.5, 0, 0, 0, 0, 0),
  fade = FALSE
)


#guild_bsem
test <- summary(passerine_bsem)$fixed
passerine_bsem_effect

paths_df <- data.frame(target_coefs = c(
  "birdabundance_snowmelt",
  "birdabundance_budburst",
  "birdabundance_icemelt",
  "birdabundance_breedingtemp",
  
  "icemelt_regiontemp",
  
  "breedingtemp_regiontemp",
  "breedingtemp_icemelt", 
  
  "snowmelt_regiontemp",
  "snowmelt_breedingtemp",
  
  "budburst_regiontemp",
  "budburst_breedingtemp",
  "budburst_snowmelt"
), target_paths = c(
  # bird abundance paths
  "Snowmelt → Bird Abundance",        
  "Budburst → Bird Abundance",        
  "Ice Melt → Bird Abundance",        
  "Breeding Temperature → Bird Abundance",
  
  # ice melt paths
  "Region Temperature → Ice Melt",
  
  # breeding temperature paths
  "Region Temperature → Breeding Temperature",
  "Ice Melt → Breeding Temperature",
  
  # snowmelt paths
  "Region Temperature → Snowmelt",
  "Breeding Temperature → Snowmelt",
  
  # budbust paths
  "Region Temperature → Budburst",
  "Breeding Temperature → Budburst",
  "Snowmelt → Budburst"
))
node_coords = tibble(
  name = c(
    "Region Temperature",
    "Ice Melt",
    "Breeding Temperature",
    "Snowmelt",
    "Budburst",
    "Bird Abundance"
  ),
  x = c(0, 2, 2, 2, 2, 4),  # Adjust x-coordinate for breeding temperature
  y = c(0, 6, 2, -2, -6, 0)   # Adjust y-coordinate for breeding temperature
)

paths_df <- paths_df %>%
  mutate(
    from = sub(" → .*", "", paths$target_paths),  # Extract the part before the arrow
    to = sub(".* → ", "", paths$target_paths)     # Extract the part after the arrow
  )

edges <- paths_df %>%
  left_join(node_coords, by = c("from" = "name")) %>%  # Join to get coordinates for "from" nodes
  rename(x_from = x, y_from = y) %>%
  left_join(node_coords, by = c("to" = "name")) %>%    # Join to get coordinates for "to" nodes
  rename(x_to = x, y_to = y) %>%
  mutate(x_mid = (x_from + x_to) / 2,                   # Calculate midpoints
         y_mid = (y_from + y_to) / 2) %>%               # for edges
  mutate(x_mid = case_when(
    target_paths == "Breeding Temperature → Budburst" ~ x_mid - 0.75,
    TRUE ~ x_mid
  )) %>%
  mutate(
    curve_value = as.numeric(case_when(
      target_paths == "Breeding Temperature → Budburst" ~ 0.7,
      TRUE ~ 0
    )))

edges <- edges %>%
  left_join(passerine_bsem_effect, by = c("target_coefs" = "parameter"))

shortening_factor = 0.75
edges <- edges %>%
  mutate(
    xend_shortened = x_from + (x_to - x_from) * shortening_factor,
    yend_shortened = y_from + (y_to - y_from) * shortening_factor,
    
    xstart_shortened = x_to + (x_from - x_to) * shortening_factor,
    ystart_shortened = y_to + (y_from - y_to) * shortening_factor
  )



ggplot() +
  ggforce::geom_arc(
    data = edges,
    aes(x0 = x_mid, y0 = y_mid, 
        xend = xstart_shortened, 
        yend = ystart_shortened, 
        r = 1,
        size = abs(estimate), 
        curvature = curve_value),  # Map curvature here
    lineend = "butt"
  )

geom_curve(
  data = edges,
  #aes(x = x_from, y = y_from, xend = xend_shortened, yend = yend_shortened, size = abs(estimate)),
  aes(x = xstart_shortened, y = ystart_shortened, xend = x_mid, yend = y_mid, size = abs(estimate)),
  #arrow = arrow(length = unit(0.25, "cm")),
  lineend = "butt", 
  curvature = edges$curve_value
) 

+
  geom_curve(
    data = edges,
    #aes(x = x_from, y = y_from, xend = xend_shortened, yend = yend_shortened, size = abs(estimate)),
    aes(x = x_mid, y = y_mid, xend = xend_shortened, yend = yend_shortened, size = abs(estimate)),
    arrow = arrow(length = unit(0.25, "cm")),
    lineend = "butt",    
    curvature = curve
  ) 

+ geom_label(
  data = node_coords,
  aes(x = x, y = y, label = name),
  fill = "grey",
  color = "black",
  size = 5,
  label.size = 1,
  label.padding = unit(c(0.4, 0.1, 0.4, 0.1), "cm"),
  label.r = unit(0, "pt")
)


+ scale_size_continuous(range = c(0.5, 3)) +
  geom_label(
    data = edges,
    aes(x = x_mid, y = y_mid, label = ifelse(is.na(estimate), "", paste0(estimate, "\n[", l95.ci, ", ", u95.ci, "]"))),
    size = 4,
    color = "darkred",
    fill = "white",
    label.size = 0,
    vjust = .75
  ) 





+ geom_label(
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







#######################
plot_bsem_paths <- function(
    model,
    target_coefs = c(
      "birdabundance_snowmelt",
      "birdabundance_budburst",
      "birdabundance_icemelt",
      "birdabundance_breedingtemp",
      
      "icemelt_regiontemp",
      
      "breedingtemp_regiontemp",
      "breedingtemp_icemelt", 
      
      "snowmelt_regiontemp",
      "snowmelt_breedingtemp",
      
      "budburst_regiontemp",
      "budburst_breedingtemp",
      "budburst_snowmelt"
    ),
    target_paths = c(
      # bird abundance paths
      "Snowmelt → Bird Abundance",        
      "Budburst → Bird Abundance",        
      "Ice Melt → Bird Abundance",        
      "Breeding Temperature → Bird Abundance",
      
      # ice melt paths
      "Region Temperature → Ice Melt",
      
      # breeding temperature paths
      "Region Temperature → Breeding Temperature",
      "Ice Melt → Breeding Temperature",
      
      # snowmelt paths
      "Region Temperature → Snowmelt",
      "Breeding Temperature → Snowmelt",
      
      # budbust paths
      "Region Temperature → Budburst",
      "Breeding Temperature → Budburst",
      "Snowmelt → Budburst"
    ),
    node_coords = tibble(
      name = c(
        "Region Temperature",
        "Ice Melt",
        "Breeding Temperature",
        "Snowmelt",
        "Budburst",
        "Bird Abundance"
      ),
      x = c(0, 2, 2, 2, 2, 4),  # Adjust x-coordinate for breeding temperature
      y = c(0, 6, 2, -2, -6, 0)   # Adjust y-coordinate for breeding temperature
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



###############

model_summary <- summary(guild_bsem)

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
