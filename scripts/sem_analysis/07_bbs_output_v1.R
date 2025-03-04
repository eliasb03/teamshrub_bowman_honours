#------------------------------
# teamshrub_bowman_honours
# 07_bbs_output_v1.R
# By: Elias Bowman 
# Created: 2025-03-03
# 
# Description: This script will construct bbs_sem model outputs
#------------------------------

library(tidyverse)
library(ggeffects)
library(cowplot)
library(dplyr)

# Read model outputs
model_output_path <- "outputs/sem/abundance/"
#passerine_bsem <- readRDS(file = paste0(model_output_path, "passerine.rds"))
guild_bsem <- readRDS(file = paste0(model_output_path, "guild.rds"))

process_fixed_effects <- function(model) {
  summary(model)$fixed %>%
    tibble::rownames_to_column(var = "Parameter") %>%
    mutate(parameter = Parameter) %>%  # Keep a copy of the original column
    separate(Parameter, into = c("response", "predictor"), sep = "_", extra = "merge", fill = "right") %>%
    mutate(
      # Extract interaction values
      interaction = case_when(
        grepl(":guild", predictor) ~ sub(".*:guild", "", predictor),  # Extract after ":guild"
        grepl("^guild", predictor) ~ sub("^guild", "", predictor),    # Extract after "guild" (for interaction intercepts)
        TRUE ~ NA_character_
      ),
      # Remove :guildX from predictor but keep the main term
      predictor = case_when(
        grepl(":guild", predictor) ~ sub(":guild.*", "", predictor),  # Remove everything after ":guild"
        grepl("^guild", predictor) ~ "Intercept",  # Replace predictor for interaction intercepts
        TRUE ~ predictor
      ),
      # Apply exp() only to rows where response is "birdabundance" and delay_invlink is FALSE
      # across(c(estimate = Estimate, est.error = Est.Error, l95.ci = `l-95% CI`, u95.ci = `u-95% CI`),
      #        ~ ifelse((response == "birdabundance" & !delay_invlink), exp(.x), .x))
    ) %>%
    rename(rhat = Rhat, bulk.ess = Bulk_ESS, tail.ess = Tail_ESS, estimate = Estimate, est.error = Est.Error, l95.ci = `l-95% CI`, u95.ci = `u-95% CI`) %>%
    select(response, predictor, interaction, estimate, est.error, l95.ci, u95.ci, parameter, rhat, bulk.ess, tail.ess)
}

add_interaction_title <- function(data) {
  data %>%
    group_by(response, predictor) %>%
    mutate(
      interaction = case_when(
        is.na(interaction) & response == "birdabundance" ~ "passerine",
        interaction == "shorebird" ~ "shorebird",
        interaction == "waterbird" ~ "waterbird",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      solved = case_when(
        interaction == "passerine" ~ TRUE,
        is.na(interaction) ~ TRUE,
        TRUE ~ FALSE
      )
    )
}

create_intercepts <- function(data) {
  data %>%
    filter(predictor == "Intercept") %>%
    select(-solved, -rhat, -bulk.ess, -tail.ess) %>%
    rename(
      intercept = estimate,
      intercept_error = est.error,
      intercept_l95 = l95.ci,
      intercept_u95 = u95.ci,
      intercept_parameter = parameter
    ) %>%
    mutate(intercept = case_when(
      interaction %in% c("shorebird", "waterbird") ~ intercept + intercept[interaction == "passerine"],
      TRUE ~ intercept
    )) %>%
    ungroup() %>%
    select(-predictor)
}

add_intercepts <- function(data, intercepts) {
  data %>% 
    filter(predictor != "Intercept") %>%
    left_join(intercepts, by = c("response", "interaction"))
}

address_interaction <- function(data) {
  data %>%
    group_by(response, predictor) %>%
    mutate(
      new.estimate = case_when(
        solved ~ estimate,
        interaction %in% c("shorebird", "waterbird") ~ estimate + estimate[interaction == "passerine"]
      ),
      new.est.error = case_when(
        solved ~ est.error,
        interaction %in% c("shorebird", "waterbird") ~ est.error + est.error[interaction == "passerine"]
      ),
      new.l95.ci = case_when(
        solved ~ l95.ci,
        interaction %in% c("shorebird", "waterbird") ~ l95.ci + l95.ci[interaction == "passerine"]
      ),
      new.u95.ci = case_when(
        solved ~ u95.ci,
        interaction %in% c("shorebird", "waterbird") ~ u95.ci + u95.ci[interaction == "passerine"]
      )
    ) %>%
    ungroup()
}

#### BIG NOTE: NO WAY THE 95% CI ARE CORRECT RN. NEED TO FIX THIS
guild_bsem_effect <- guild_bsem %>%
  process_fixed_effects() %>%
  add_interaction_title()

intercepts <- guild_bsem_effect %>%
  create_intercepts()

guild_bsem_effect <- guild_bsem_effect %>%
  add_intercepts(intercepts)

guild_bsem_interacted <- guild_bsem_effect %>%
  address_interaction() %>%
  select(response, predictor, interaction,
         new.estimate, new.est.error, new.l95.ci, new.u95.ci, parameter, 
         intercept, intercept_parameter, intercept_error, intercept_l95, intercept_u95,
         estimate, est.error, l95.ci, u95.ci, rhat, bulk.ess, tail.ess) %>%
  mutate(response_single = if_else(
    is.na(interaction), 
    response, 
    paste0(interaction, str_sub(response, 5, nchar(response)))
  ))

#---------------------------
# SEM Figure
#---------------------------
# Group by the response_single variable and paste the predictors together
model_lines <- guild_bsem_interacted %>%
  group_by(response_single) %>%
  summarize(line = paste0(response_single, " ~ ", 
                          paste(paste0(new.estimate, "*", predictor), collapse = " + "))) %>%
  pull(line)

# Remove duplicate lines
model_lines <- unique(model_lines)

response_order <- c("passerineabundance", "shorebirdabundance", "waterbirdabundance", "budburst", "snowmelt", "breedingtemp", "icemelt")

# Extract the response variable from each line (before the "~")
responses <- sapply(model_lines, function(x) strsplit(x, " ~ ")[[1]][1])

# Create a factor to reorder the responses based on the specified order
responses <- factor(responses, levels = response_order)

# Reorder the model lines according to the response order
model_lines <- model_lines[order(responses)]

# Combine all lines into a single model specification string
lavaan_model <- paste(model_lines, collapse = "\n")

# Print your model specification to check
cat(lavaan_model)

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
  sizeMan = 5,
  color = "white",
  edge.color = edge_colors,  # Use custom edge colors
  style = "lisrel",
  curve = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, 0, 0),
  fade = FALSE,
  residuals = FALSE,
  fixedStyle = 1
)



#---------------------------
# Bird Abundance Specific Variable Outputs
#---------------------------
# List of variable names
variables <- c("icemelt", "breedingtemp", "snowmelt", "budburst")

# Get the responses using lapply, including the "guild" term
responses <- lapply(variables, function(var) predict_response(guild_bsem, terms = c(var, "guild")))

# Name the responses with the variable names directly
names(responses) <- variables

# Process and combine data frames using imap_dfr
resp_data <- imap_dfr(responses, ~ as.data.frame(.x) %>%
                        filter(response.level == "birdabundance") %>%
                        mutate(predictor = .y))  # .y is the name of the list element

# Plot icemelt_resp
sem_data_plotting <- sem_data %>%
  mutate(group = guild) %>%
  filter(complete.cases(.)) %>%
  pivot_longer(cols = c("icemelt", "breedingtemp", "snowmelt", "budburst"), names_to = "predictor", values_to = "value")

# set a ylimit
ylim_threshold <- 40

# Print the number of excluded data points
excluded_data <- sem_data %>% 
  filter(birdabundance > ylim_threshold)
cat("Number of data points excluded: ", nrow(excluded_data), "\n")

# Recalibrating Data
scaling_params <- read_csv("data/clean/sem/scaling_params.csv") %>%
  rename("predictor" = "variable") #%>%

resp_data <- resp_data %>%
  left_join(scaling_params, by = "predictor")%>%   # Join based on predictor
  mutate(x_cal = (x * scaling_value) + mean)          # Revert centering and scaling 

sem_data_plotting <- sem_data_plotting %>%
  left_join(scaling_params, by = "predictor") %>%   # Join based on predictor
  mutate(value_cal = (value*scaling_value) + mean)  # Add the calibration value to value

# Plot the calibrated data
ggplot(data = resp_data, aes(x = x_cal, y = predicted, colour = group)) +
  geom_line(size = 1) + 
  geom_point(data = sem_data_plotting, aes(x = value_cal, y = birdabundance, colour = group), 
             size = 1, alpha = 0.5, position = "jitter") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.13, colour = NA) +  # Fill ribbons
  scale_colour_viridis_d(name = "Guild",  
                         labels = c("Waterbird", "Passerine", "Shorebird"), 
                         guide = guide_legend(nrow = 1)) +  # Combined legend for colour
  scale_fill_viridis_d(name = "Guild",  
                       labels = c("Waterbird", "Passerine", "Shorebird"), 
                       guide = guide_legend(nrow = 1)) +  # Combined legend for fill
  labs(x = NULL,#"Predictor Variable", 
       y = "Predicted Bird Abundance") +
  theme_half_open(font_size = 12) +  
  theme(
    legend.position = c(0.5, -0.12),         # Bottom center position for legend
    legend.justification = "center",         # Center the legend horizontally
    legend.title.align = 0.5,                # Center the legend title
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 10),
    panel.spacing = unit(0.02, "npc"),
    plot.margin = margin(t = 10, r = 10, b = 24, l = 10)
  ) + 
  facet_wrap(~predictor, nrow = 2, scale = "free_x",
             strip.position = "bottom",     # Move facet labels to the bottom
             labeller = as_labeller(c("icemelt" = "Ice Melt\n(Day of Year)", 
                                      "budburst" = "Budburst\n(Day of Year)",
                                      "breedingtemp" = "Spring Temperature\n(Degrees C)",
                                      "snowmelt" = "Snowmelt\n(Day of Year)"))) +
  coord_cartesian(ylim = c(0, ylim_threshold))



#---------------------------
# Old SEM Attempt
#---------------------------


model_summary <- summary(guild_bsem)

fixed_effects <- model_summary$fixed %>%
  mutate(coef = rownames(.)) %>%
  select(coef, Estimate, Est.Error, `l-95% CI`, `u-95% CI`) %>%
  filter(!grepl(":guild", coef))  # Remove interaction

target_coefs <- c(
  "birdabundance_icemelt",
  "birdabundance_breedingtemp",
  "birdabundance_snowmelt",
  "birdabundance_budburst",
  
  "icemelt_regiontemp",
  
  "breedingtemp_icemelt",
  "breedingtemp_regiontemp",
  
  "snowmelt_breedingtemp",
  "snowmelt_regiontemp",
  
  "budburst_snowmelt",
  "budburst_breedingtemp",
  "budburst_regiontemp")

target_paths <- c(
  "Ice Melt → Bird Abundance", 
  "Spring Temperature → Bird Abundance", 
  "Snowmelt → Bird Abundance", 
  "Budburst → Bird Abundance", 
  
  "Region Temperature → Ice Melt",
  
  "Ice Melt → Spring Temperature", 
  "Region Temperature → Spring Temperature",
  
  "Spring Temperature → Snowmelt",
  "Region Temperature → Snowmelt",
  
  "Snowmelt → Budburst", 
  "Spring Temperature → Budburst", 
  "Region Temperature → Budburst"
)

# Create a tibble with the estimates for only these coefficients.
estimates <- fixed_effects %>%
  filter(coef %in% target_coefs) %>%
  # Ensure the correct ordering by matching names:
  mutate(path = target_paths[match(coef, target_coefs)]) %>%
  rename(estimate = Estimate, ci_low = `l-95% CI`, ci_high = `u-95% CI`, est.error = Est.Error) %>%
  mutate(across(c(estimate, ci_low, ci_high, est.error), ~ round(., 3))) %>%
  separate(path, into = c("from", "to"), sep = " → ")

# Define node positions for the plot.
node_coords <- tibble(
  name = c("Ice Melt", "Spring Temperature", "Snowmelt", "Budburst", "Bird Abundance", "Region Temperature"),
  x = c(0, 0, 0, 0, 4.5, -4.5),
  y = c(6, 2, -2, -6, 0, 0)
) 
# Join the node coordinates to get starting and ending positions:
edges <- estimates %>%
  left_join(node_coords, by = c("from" = "name")) %>%
  rename(x_from = x, y_from = y) %>%
  left_join(node_coords, by = c("to" = "name")) %>%
  rename(x_to = x, y_to = y) %>%
  # Compute midpoint for label placement (with a slight vertical offset)
  mutate(x_mid = (x_from + x_to) / 2,
         y_mid = (y_from + y_to) / 2) #+ 0.15) 

node_coords <- node_coords %>% 
  mutate(clean = case_when(
    name == "Ice Melt" ~ "Ice Melt",
    name == "Spring Temperature" ~ "Spring\nTemperature",
    name == "Snowmelt" ~ "Snowmelt",
    name == "Budburst" ~ "Budburst",
    name == "Bird Abundance" ~ "Bird\nAbundance",
    name == "Region Temperature" ~ "Regional\nTemperature",
    TRUE ~ name),
  )

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
             aes(x = x, y = y, label = clean),
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

