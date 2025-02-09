# # BBS Investigations
# # Looking into the BBS data for anomalies and distributions
# 
# library(cowplot)
# 

library(ggplot2)
library(dplyr)
library(ggthemes)
library(cowplot)
library(patchwork)
library(patchwork)

# View(bbs.summary)

bbs.only.obs <- bbs.summary %>%
  filter(total.count != 0)

species.list <- read.csv("data/clean/bbs/species_list.csv")

bbs.species.summary <- bbs.only.obs %>%
  group_by(species) %>%
  summarise(mean.count = mean(total.count),
            sd.count = sd(total.count),
            observance.frequency = n()) %>%
  mutate(in.list = ifelse(species %in% species.list$species, TRUE, FALSE))

# Define custom theme function
custom_theme <- function() {
  theme_half_open(font_size = 12) +
    theme(
      axis.text.x = element_text(angle = 75, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      legend.position = "top"
    )
}

# Preprocess data: reorder and create color labels
bbs.species.summary <- bbs.species.summary %>%
  mutate(
    species = reorder(species, -observance.frequency), # Reorder species
    in_list_label = ifelse(in.list, "In List", "Not in List") # Create a readable label
  )

# Define color palette
color_palette <- c("In List" = "#4CAF50", "Not in List" = "#F44336") # Green and Red

# Function to create bar plots
create_bar_plot <- function(data, title) {
  ggplot(data, aes(x = species, y = observance.frequency, fill = in_list_label)) +
    geom_bar(stat = "identity", show.legend = TRUE) +
    scale_fill_manual(values = color_palette) +
    labs(
      title = title,
      x = "Species",
      y = "Number of Years Observed",
      fill = "Species Status"
    ) +
    custom_theme()
}

# Create full dataset plot
full_plot <- create_bar_plot(
  bbs.species.summary,
  "Species Observance Frequency in the Breeding Bird Survey"
)

# Create top 30 species plot
top30 <- bbs.species.summary %>%
  slice_max(order_by = observance.frequency, n = 30) # Select top 30

top30_plot <- create_bar_plot(
  top30,
  "Top 30 Species Observance Frequency in the Breeding Bird Survey"
)

# Display plots
full_plot
top30_plot

###########

# Extract the top 30 species names
top30_data <- bbs.only.obs %>%
  filter(species %in% top30$species) %>%
  group_by(year, species) %>%
  summarise(total.count = sum(total.count), .groups = "drop")

top30_species <- unique(top30_data$species)

# Define a function to create yearly observation count plots for a single species
plot_species_observation <- function(data, species_name, fill_colors = c("red", "green")) {
  species_data <- data %>%
    filter(species == species_name)
  
  ggplot(species_data, aes(x = year, y = total.count, fill = in.list)) +
    geom_bar(stat = "identity", color = "black") +
    scale_fill_manual(
      values = setNames(fill_colors, c(FALSE, TRUE)),
      name = "In List",
      labels = c("No", "Yes")
    ) +
    labs(
      title = species_name,
      x = "Year",
      y = "Observation Count"
    ) +
    theme_half_open() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

# Create a list of plots for the top 30 species
species_plots <- top30_species %>%
  set_names() %>% # Use species names as list names
  map(plot_species_observation(top30_data, .))

#species_plots[["Black Guillemot"]]


# Combine all plots into a single grid
combined_plot <- wrap_plots(species_plots, ncol = 5) +
  plot_annotation(
    title = "Yearly Observation Counts for Top 30 Species",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  )

# Display the combined plot
print(combined_plot)

# 
# Extract the top 30 species names
top30_data <- bbs.only.obs %>%
  filter(species %in% top30$species) %>%
  group_by(year, species) %>%
  summarise(total.count = sum(total.count), .groups = "drop") %>%
  mutate(in.list = ifelse(species %in% species.list$species, TRUE, FALSE))

top30_species <- unique(top30_data$species)

# Define a function to create yearly observation count plots for a single species
plot_species_observation <- function(data, species_name, fill_colors = c("red", "green")) {
  species_data <- data %>%
    filter(species == species_name)
  
  ggplot(species_data, aes(x = year, y = total.count, fill = in.list)) +
    geom_bar(stat = "identity", color = "black") +
    scale_fill_manual(
      values = setNames(fill_colors, c(FALSE, TRUE)),
      name = "In List",
      labels = c("No", "Yes")
    ) +
    labs(
      title = species_name,
      x = "Year",
      y = "Observation Count"
    ) +
    theme_half_open() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

# Create a list of plots for the top 30 species
species_plots <- top30_species %>%
  set_names() %>% # Use species names as list names
  map(~ plot_species_observation(top30_data, .x)) # Apply the function with species name

# Combine all plots into a single grid
combined_plot <- wrap_plots(species_plots, ncol = 5) +
  plot_annotation(
    title = "Yearly Observation Counts for Top 30 Species",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  )

# Display the combined plot
print(combined_plot)
# 
# unsampled <- bbs.species.summary %>% 
#        filter(!(species %in% species.list$species))
# 
# top20 <- bbs.species.summary %>%
#   filter(rank(desc(observance.frequency))<=20)
# 
# # plot bar of all species counts, species id on x axis,n counts on y axis
# bbs.species.summary %>%
#   mutate(
#     species = reorder(species, -observance.frequency), # Reorder species
#     bar_color = ifelse(in.list, "green", "red")        # Define colors based on `in.list`
#   ) %>%
#   ggplot(aes(x = species, y = observance.frequency, fill = bar_color)) +  # Use `fill` for bar colors
#   geom_bar(stat = "identity") +
#   scale_fill_identity() +                              # Use the colors as is
#   theme_half_open() +
#   theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
#   labs(title = "Species Observance Frequency in the Breeding Bird Survey",
#        x = "Species",
#        y = "Number of Years Observed") 
# 
# # plot bar of top 20 species counts, species id on x axis,n counts on y axis
# bbs.species.summary %>%
#   mutate(
#     species = reorder(species, -observance.frequency), # Reorder species
#     bar_color = ifelse(in.list, "green", "red")        # Define colors based on `in.list`
#   ) %>%
#   filter(rank(desc(observance.frequency))<=30) %>%
#   ggplot(aes(x = species, y = observance.frequency, fill = bar_color)) +  # Use `fill` for bar colors
#   geom_bar(stat = "identity") +
#   scale_fill_identity() +                              # Use the colors as is
#   theme_half_open() +
#   theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
#   labs(title = "Top 30 Species Observance Frequency in the Breeding Bird Survey",
#        x = "Species",
#        y = "Number of Years Observed") 
