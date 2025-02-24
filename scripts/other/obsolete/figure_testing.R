

guild_bsem



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
