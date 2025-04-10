#------------------------------
# teamshrub_bowman_honours
# 06_ARU_analysis_v1
# By: Elias Bowman
# Created: 
#
# Description: This script runs statistical analyses on the outputs from the BirdNET models
#------------------------------

library(brms)
library(tidyverse)
library(tidybayes)
library(ggplot2)
library(ggeffects)
library(glmmTMB)
library(cowplot)
library(ggridges)
library(gtsummary)
library(gt)
library(broom.mixed)
library(knitr)
library(webshot2)

confidence_threshold <- 0.5 # Confidence threshold used by BirdNET in the output I want

temp_palette <- c("high" = "#E63946", "low" = "#457B9D")

aru_dataframe <- # Read in ARU Data
  read_csv(paste0(
    "data/clean/aru/aru_analysis_data",
    "_conf",
    confidence_threshold,
    ".csv"
  ))

ARU_average_temps <- # Read in in ARU Average Temperatures
  read_csv("data/clean/aru/tomst_averages.csv")

species_list <- # Species to do comparison between
  c("Lapland Longspur", 
    "Semipalmated Plover", 
    #"Common Eider",
    "Red-throated Loon"
  )

# Summarize ARU data to daily level
aru_daily <- aru_dataframe %>%
  mutate(date = as.Date(time_interval)) %>%
  group_by(locationID, date, common_name) %>%
  summarize(total_count = sum(species_count)) %>%
  ungroup() %>%
  left_join(ARU_average_temps, by = c("locationID" = "aru_name")) %>%
  mutate(temp_binary = as.factor(temp_binary)) #%>% filter(common_name %in% species_list)


period_length <- 30 # 20 days
start <- as.Date("2024-06-25") # Early time period
total_range <- c(start, start + period_length)

# Continuous data
aru_cont <- aru_daily %>%
  filter(date >= total_range[1] & date <= total_range[2]) %>%
  mutate(
    day_from_start = as.numeric(date - total_range[1]), 
  )

# Creating a summary dataset ready for analysis
aru_summary <- aru_cont %>%
  select(
    locationID,
    date,
    day_from_start,
    common_name,
    total_count,
    temp_binary,
    avg_temp,
    tomst_num
  ) %>%
  mutate(
    locationID = as.factor(locationID),
    date = as.factor(date),
    scaled_count = scale(total_count, center = TRUE, scale = TRUE),
    log_count = log(total_count) # Log transform total count, choosing not to +1
  )

aru_summary <- aru_summary[aru_summary$locationID!="ARUQ18",] # remove aruq 18 which rarely worked
aru_summary <- aru_summary[aru_summary$locationID!="ARUQ8",] # remove aruq 8 which died early worked
aru_summary <- aru_summary[aru_summary$locationID!="ARUQ17",] # remove aruq 17 which was deployed quite late


# Filtering by taxa
passerine <- aru_summary %>%
  filter(common_name == "Lapland Longspur")
shorebird <- aru_summary %>%
  filter(common_name == "Semipalmated Plover")
  #filter(common_name == "Semipalmated Sandpiper")
waterbird <- aru_summary %>%
#  filter(common_name == "Common Eider")
  filter(common_name == "Red-throated Loon")

aru_priors <- c(
  set_prior("normal(0, 4.6)", class = "b", coef = "day_from_start"),  # Prior for day_from_start
  # max is roughly 1000, over a 16 day inerval about (1000/16) = ~60, say 100 more or less calls a day, log(100) = 4.6
  set_prior("normal(0, 4.6)", class = "b", coef = "temp_binarylow"),  # Prior for temp_binarylow
  set_prior("normal(0, 4.6)", class = "b", coef = "day_from_start:temp_binarylow"),  # Prior for temp interaction term
  set_prior("normal(2, 6)", class = "Intercept")  # Prior for the intercept
)



passerine_model <- brm(
  total_count ~ day_from_start * temp_binary + (1|locationID) + (1|date),
  data = passerine,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

waterbird_model <- brm(
  total_count ~ day_from_start * temp_binary + (1|locationID) + (1|date),
  data = waterbird,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)


shorebird_model <- brm(
  total_count ~ day_from_start * temp_binary + (1|locationID) + (1|date),
  data = shorebird,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

# pp_check(passerine_model, resp = "total_count", ndraws = 1000)
summary(passerine_model)


model_output_path <- "outputs/aru/models/calling/"
saveRDS(passerine_model, file = paste0(model_output_path, "passerine.rds"))
saveRDS(waterbird_model, file = paste0(model_output_path, "waterbird_redthroatedloon.rds"))
#saveRDS(waterbird_model, file = paste0(model_output_path, "waterbird.rds"))
saveRDS(shorebird_model, file = paste0(model_output_path, "shorebird.rds"))

passerine_model <- readRDS(file = paste0(model_output_path, "passerine.rds"))
waterbird_model <- readRDS(file = paste0(model_output_path, "waterbird_redthroatedloon.rds"))
#waterbird_model <- readRDS(file = paste0(model_output_path, "waterbird.rds"))
shorebird_model <- readRDS(file = paste0(model_output_path, "shorebird.rds"))

# 
# 
# # Generate predicted values for day_from_start by temp_binary
# pass_pred <- ggpredict(passerine_model, terms = c("day_from_start[0:30]", "temp_binary")) %>%
#   as.data.frame() %>%
#   mutate(species = "Lapland Longspur")
# 
# shor_pred <- ggpredict(shorebird_model, terms = c("day_from_start[0:30]", "temp_binary")) %>%
#   as.data.frame() %>%
#   mutate(species = "Semipalmated Plover")
# 
# watr_pred <- ggpredict(waterbird_model, terms = c("day_from_start[0:30]", "temp_binary")) %>%
#   as.data.frame() %>%
#   mutate(species = "Red-throated Loon")
#   #mutate(species = "Common Eider")
# 
# all_pred <- bind_rows(
#   pass_pred,
#   shor_pred,
#   watr_pred
# ) %>% mutate(Temperature = group) %>%
#   mutate(Temperature = fct_recode(Temperature, 
#                                   "Cool" = "low",
#                                   "Warm" = "high"),
#          Date = as.Date(x + start))
# 
# aru_plotting_data <- aru_summary %>%
#   filter(common_name %in% c("Lapland Longspur", "Semipalmated Plover", "Red-throated Loon")) %>%
#   mutate(Temperature = temp_binary) %>%
#   filter(complete.cases(.)) %>%
#   mutate(Temperature = fct_recode(Temperature, 
#                                   "Cool" = "low",
#                                   "Warm" = "high"),
#          Date = as.Date(date))
# 
# 
# # calling_temp_resp <- 
# #   ggplot(data = all_pred, aes(x = Date, y = predicted, colour = group)) +
# #   geom_line(linewidth = 1) + 
# #   #geom_point(data = aru_plotting_data, aes(x = Date, y = total_count, colour = Temperature), size = 1, alpha = 0.1, position = "jitter") +
# #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
# #               alpha = 0.13, colour = NA) +  # Fill ribbons
# #   scale_colour_viridis_d(name = "Temperature",
# #                          #option = "viridis",  # Change palette here
# #                          labels = c("Warm", "Cold"),
# #                          guide = guide_legend(nrow = 1)) +  # Combined legend for colour
# #   scale_fill_viridis_d(name = "Temperature",
# #                        #option = "viridis",  # Change palette here
# #                        labels = c("Warm", "Cold"),
# #                        guide = guide_legend(nrow = 1)) +  # Combined legend for fill
# #   labs(x = NULL,#"Predictor Variable", 
# #        y = "Predicted Bird Call Detections") +
# #   theme_half_open(font_size = 12) +
# #   theme(
# #     strip.background = element_blank(),
# #     strip.placement = "outside",
# #     strip.text = element_text(size = 10, face = "bold"),
# #     panel.spacing = unit(0.02, "npc"),
# #     plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
# #     legend.position = "bottom"
# #   ) +
# #   facet_wrap(~species, nrow = 1, scale = "free",
# #              strip.position = "top") +
# #   scale_y_continuous(limits = c(0, NA))
# # Define the warm (reddish) and cold (bluish) colors
# #temp_palette <- c("high" = "#D73027", "low" = "#4575B4")
# 
# calling_temp_resp <- 
#   ggplot(data = all_pred, aes(x = Date, y = predicted, colour = group)) +
#   geom_line(linewidth = 1) + 
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
#               alpha = 0.13, colour = NA) +  # Fill ribbons
#   scale_colour_manual(name = "Temperature",
#                       values = temp_palette,
#                       labels = c("Warm", "Cold"),
#                       guide = guide_legend(nrow = 1)) +  # Consistent legend
#   scale_fill_manual(name = "Temperature",
#                     values = temp_palette,
#                     labels = c("Warm", "Cold"),
#                     guide = guide_legend(nrow = 1)) +  # Matching fill colors
#   labs(x = NULL, 
#        y = "Predicted Bird Call Detections") +
#   theme_minimal(base_family = "Helvetica") +
#   theme_half_open(font_size = 12) +
#   theme(
#     strip.background = element_blank(),
#     strip.placement = "outside",
#     strip.text = element_text(size = 10, face = "bold"),
#     panel.spacing = unit(0.02, "npc"),
#     plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
#     legend.position = "bottom"
#   ) +
#   facet_wrap(~species, nrow = 1, scale = "free",
#              strip.position = "top") +
#   scale_y_continuous(limits = c(0, NA))
# 
# calling_temp_resp
# 
# ggsave("outputs/aru/model_figs/calling_temp_model_17mar2025.pdf", plot = calling_temp_resp, width = 10, height = 4, units = "in")
# 
# ggsave("outputs/aru/model_figs/calling_temp_model_17mar2025.png", plot = calling_temp_resp, width = 12, height = 4, units = "in")

#-----------------------------
# Create Calling Ridgeline
#-----------------------------
create_faceted_ridgeline_plot <- function(species_list, scaling = 0.4, log = TRUE, temp_palette = c("high" = "#E63946", "low" = "#457B9D")) {
  
  # Helper function to prepare ridgeline data for a given species
  create_ridgeline_data <- function(species_of_interest) {
    
    # Filter data for the selected species and exclude ARUQ18 and ARUQ17
    filtered_species <- aru_daily %>%
      filter(common_name == species_of_interest) #& !locationID %in% c("ARUQ18", "ARUQ17"))
    
    # Apply 3-day rolling mean to smooth detections
    if (log) {
      filtered_species <- filtered_species %>%
        mutate(smoothed_total_count = zoo::rollmean(log(total_count + 1), k = 3, fill = NA))
    } else {
      filtered_species <- filtered_species %>%
        mutate(smoothed_total_count = zoo::rollmean(total_count, k = 3, fill = NA))
    }
    
    # Assign numeric ARU values and order locations by temperature
    aru_daily <- aru_daily %>%
      mutate(aru_num = as.numeric(gsub("ARUQ", "", locationID)))
    
    ordered_locations <- aru_daily %>%
      arrange(desc(temp_binary), desc(aru_num)) %>%
      pull(locationID) %>%
      unique()
    
    # Return processed data with species column
    filtered_species %>%
      mutate(species = species_of_interest,
             locationID = factor(locationID, levels = ordered_locations))
  }
  
  # Process data for all species in the list
  all_ridgeline_data <- bind_rows(lapply(species_list, create_ridgeline_data))
  
  # Generate the faceted ridgeline plot
  ggplot(all_ridgeline_data, aes(x = date, y = locationID, height = smoothed_total_count, fill = temp_binary)) +
    geom_ridgeline(stat = "identity", scale = scaling, alpha = 0.7) +
    geom_vline(xintercept = as.numeric(total_range), linetype = "dashed", color = "black", size = 1) + # Add vertical lines
    scale_fill_manual(values = temp_palette) +
    labs(#title = "Ridgeline Plots of Bird Call Detections",
         x = "Date",
         y = "Smoothed and Log-Transformed Detections",
         fill = "Temperature") +
    theme_minimal(base_family = "Helvetica") +
    theme_half_open(font_size = 12) + 
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.x = element_text(),#angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(),#face = "bold"),
          plot.title = element_text(),#face = "bold"),
          panel.grid.major.y = element_line(colour = "grey80"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    scale_x_date(date_labels = "%b %d", date_breaks = "10 days") +
    coord_cartesian(xlim = c(as.Date("2024-06-20"), as.Date("2024-08-20"))) +
    facet_wrap(~species, nrow = 1, strip.position = "top")  # Facet by species
}

# Example Usage:
species_list <- c("Semipalmated Plover", "Lapland Longspur", "Red-throated Loon")
faceted_plot <- create_faceted_ridgeline_plot(species_list)
faceted_plot

output_folder <- "outputs/aru/model_figs"
ggsave(file.path(output_folder, "Species_Ridgeline.png"), 
       faceted_plot,
       width = 14, height = 6)

#------------------------
# Output Table
#------------------------

aru_model_summary_table <- function(model, title = "Model Summary", plot_name = "model_table") {
  # Recode variables for cleaner labels
  recode_vars <- c(
    "intercept" = "Intercept",
    "Intercept" = "Intercept",
    "(Intercept)" = "Intercept",
    "(intercept)" = "Intercept",
    "day_from_start" = "Date",
    "temp_binarylow" = "Low Temperature",
    "day_from_start:temp_binarylow" = "Date:Low Temperature"
  )
  
  # Extract and format model summary
  model_output <- summary(model)$fixed %>%
    tibble::rownames_to_column(var = "Parameter") %>%
    mutate(across(c(Estimate, Est.Error, `l-95% CI`, `u-95% CI`), round, 3)) %>%
    mutate(across(c(Bulk_ESS, Tail_ESS), round, 0)) %>%
    mutate(across(c(Rhat), round, 3)) %>%
    rename(
      "Estimate" = Estimate,
      "Est.Error" = Est.Error,
      "Lower 95% CI" = `l-95% CI`,
      "Upper 95% CI" = `u-95% CI`,
      "R-hat" = Rhat,
      "Bulk ESS" = Bulk_ESS,
      "Tail ESS" = Tail_ESS
    ) %>%
    mutate(
      Parameter = recode(Parameter, !!!recode_vars)
    ) %>%
    mutate(Parameter = factor(Parameter, levels = c("Intercept", "Date", "Low Temperature", "Date:Low Temperature"))) %>%
    arrange(Parameter)
  
  # Create the GT table
  model_table <- model_output %>%
    gt() %>%
    tab_header(
      title = title
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "gainsboro"),
        cell_text(style = "italic")
      ),
      locations = cells_body(rows = `Lower 95% CI` > 0 & `Upper 95% CI` > 0)
    ) %>%
    tab_style(
      style = list(
        cell_text(style = "italic"),
        cell_fill(color = "gainsboro")
      ),
      locations = cells_body(rows = `Lower 95% CI` < 0 & `Upper 95% CI` < 0)
    ) %>%
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = Parameter == "Intercept")
    ) %>%
    tab_style(
      style = cell_borders(sides = "top", color = "grey", weight = px(1)),
      locations = cells_body(rows = Parameter == "Date")
    )
  
  output_html <- paste0("outputs/aru/model_figs/", plot_name, ".html")
  output_img = paste0("outputs/aru/model_figs/", plot_name, ".png")
  # Save as HTML
  gtsave(model_table, output_html)
  
  # Save as PNG
  webshot(output_html, file = output_img, vwidth = 200, vheight = 160, zoom = 2)
  
  cat("Table saved to:", output_html, "\nImage saved to:", output_img, "\n")
}


aru_model_summary_table(model = passerine_model,
                        title = "Passerine Calling Response",
                        plot_name = "passerine_table_06feb2025")
aru_model_summary_table(model = shorebird_model,
                        title = "Shorebird Calling Response",
                        plot_name = "shorebird_table_06feb2025")
aru_model_summary_table(model = waterbird_model,
                        title = "Waterfowl Calling Response",
                        plot_name = "waterbird_table_06feb2025")

# -----------------------
# Create Single Ridgeline
# -----------------------
aru_daily <- aru_daily %>%
  filter(locationID != "ARUQ18",  # Remove ARUQ18
         locationID != "ARUQ8",
         locationID != "ARUQ17")  # Remove ARUQ8

# Define the function for creating the ridgeline plot
create_ridgeline_plot <- function(species_of_interest, scaling = 0.02, log = FALSE) {
  
  # Filter data for the selected species and exclude ARUQ18 and ARUQ17
  filtered_single_species <- aru_daily %>%
    filter(common_name == species_of_interest & !locationID %in% c("ARUQ18", "ARUQ8"))
  
  # Apply 7-day rolling mean to the 'total_count' column
  if (log) {
    filtered_single_species$smoothed_total_count <- zoo::rollmean(log(filtered_single_species$total_count), k = 3, fill = NA)
    y_axis_label <- "Log Smoothed Detections"
    plot_title <- paste("Log Smoothed Detections for", species_of_interest)
  } else {
    filtered_single_species$smoothed_total_count <- zoo::rollmean(filtered_single_species$total_count, k = 3, fill = NA)
    y_axis_label <- "Smoothed Detections"
    plot_title <- paste("Smoothed Detections for", species_of_interest)
  }
  
  # Reorder the locationID factor to ensure 'ARUQ0' is first, and others follow in order
  filtered_single_species <- filtered_single_species %>%
    mutate(locationID = factor(locationID, levels = c("ARUQ0", "ARUQ1", "ARUQ2", "ARUQ3", "ARUQ4", "ARUQ5", "ARUQ6", "ARUQ7", "ARUQ8", "ARUQ9", 
                                                      "ARUQ10", "ARUQ11", "ARUQ12", "ARUQ13", "ARUQ14", "ARUQ15", "ARUQ16", "ARUQ17", "ARUQ19")))
  
  # Create the ridgeline plot
  # ggplot(filtered_single_species, aes(x = date, y = locationID, group = locationID, height = smoothed_total_count, fill = locationID)) +
  #   geom_ridgeline(stat = "identity", scale = scaling, alpha = 0.7) +  # Increased scale for more separation
  #   labs(title = plot_title,
  #        x = "Date",
  #        y = y_axis_label) +
  #   theme_minimal() +
  #   theme(legend.position = "none",
  #         axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Bold x-axis text
  #         axis.text.y = element_text(face = "bold"),  # Bold y-axis text
  #         plot.title = element_text(face = "bold"),  # Bold plot title
  #         panel.grid.major = element_blank(),  # Remove major grid lines
  #         panel.grid.minor = element_blank()) +  # Remove minor grid lines 
  #   scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
  #   coord_cartesian(xlim = c(as.Date("2024-06-20"), as.Date("2024-08-20"))) +
  #   scale_y_discrete(limits = rev(levels(filtered_single_species$locationID)))  # Reverse the y-axis order
  aru_daily <- aru_daily %>%
    mutate(aru_num = as.numeric(gsub("ARUQ", "", locationID)))
  
  # Order locations: "high" temp first, "low" temp last
  ordered_locations <- aru_daily %>%
    arrange(desc(temp_binary), desc(aru_num)) %>%
    pull(locationID) %>%
    unique()
  
  # Create color palette for temperature groups
  temp_palette <- c("high" = "#D73027", "low" = "#4575B4")  # Red for warm, blue for cold
  
  ggplot(filtered_single_species, aes(x = date, y = locationID, group = locationID, height = smoothed_total_count, fill = #locationID
                                        temp_binary)) +
    geom_ridgeline(stat = "identity", scale = scaling, alpha = 0.7) +
    geom_vline(xintercept = as.numeric(total_range), linetype = "dashed", color = "black", size = 1) + # Add vertical lines
    labs(title = plot_title,
         x = "Date",
         y = y_axis_label) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title = element_text(face = "bold"),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank()) +
          panel.grid.major.y = element_line(colour = "grey80"),  # Remove major grid lines on y-axis
          panel.grid.minor.y = element_blank(),  # Remove minor grid lines on y-axis
          panel.grid.major.x = element_blank(),  # Keep x-axis grid lines
          panel.grid.minor.x = element_blank()) +
    scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
    coord_cartesian(xlim = c(as.Date("2024-06-20"), as.Date("2024-08-20"))) +
    scale_y_discrete(limits = ordered_locations) +  # Apply the new y-axis order
    scale_fill_manual(values = temp_palette)
  
}

plover_plot <- create_ridgeline_plot("Semipalmated Plover", scaling = 0.4, log = TRUE)

longspur_plot <- create_ridgeline_plot("Lapland Longspur", scaling = 0.4, log = TRUE)

loon_plot <- create_ridgeline_plot("Red-throated Loon", scaling = 0.4, log = TRUE)


output_folder <- "outputs/aru/model_figs"
ggsave(file.path(output_folder, "SemipalmPlover_Ridgeline.png"), 
       plover_plot,
       width = 10, height = 6)

ggsave(file.path(output_folder, "LapLong_Ridgeline.png"), 
       longspur_plot,
       width = 10, height = 6)

ggsave(file.path(output_folder, "RedThrLoon_Ridgeline.png"), 
       loon_plot,
       width = 10, height = 6)