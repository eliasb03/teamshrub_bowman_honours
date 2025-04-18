##############################
# Full Visualization Script for ARU Analysis
# Author: Elias Bowman (teamshrub_bowman_honours)
# Description: This script reads in ARU data and model outputs, creates visualization plots,
#              outputs summary tables, and saves all figures.
##############################

# Load Required Libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(cowplot)
library(brms)
library(gtools)
library(ggridges)
library(zoo)
library(ggeffects)
library(gt)
library(webshot2)
library(patchwork)
library(stringr)
library(readr)

# Set Parameters and Color Palettes
confidence_threshold <- 0.5  # Confidence threshold used by BirdNET
temp_palette <- c("high" = "#E63946", "low" = "#457B9D")
species_list <- c("Lapland Longspur", "Semipalmated Plover", "Red-throated Loon")
period_length <- 30
start <- as.Date("2024-06-25")
total_range <- c(start, start + period_length)

# Read In Data and Prepare ARU Datasets 
aru_dataframe <- read_csv(paste0("data/clean/aru/aru_analysis_data_conf",
                                 confidence_threshold, ".csv"))
ARU_average_temps <- read_csv("data/clean/aru/tomst_averages.csv")
aru_daily <- read_csv("data/clean/aru/aru_daily.csv")
aru_summary <- read_csv("data/clean/aru/aru_summary.csv")

# Subset data by species (for plotting predictions)
passerine <- aru_summary %>% filter(common_name == "Lapland Longspur")
shorebird  <- aru_summary %>% filter(common_name == "Semipalmated Plover")
waterbird  <- aru_summary %>% filter(common_name == "Red-throated Loon")

# Read in Model Outputs 
model_output_path <- "outputs/aru/models/calling/"
passerine_model <- readRDS(file = paste0(model_output_path, "passerine.rds"))
waterbird_model <- readRDS(file = paste0(model_output_path, "waterbird_redthroatedloon.rds"))
shorebird_model <- readRDS(file = paste0(model_output_path, "shorebird.rds"))

# Generate Predicted Values with ggpredict()
pass_pred <- ggpredict(passerine_model, terms = c("day_from_start[0:30]", "temp_binary")) %>%
  as.data.frame() %>% mutate(species = "Lapland Longspur")

shor_pred <- ggpredict(shorebird_model, terms = c("day_from_start[0:30]", "temp_binary")) %>%
  as.data.frame() %>% mutate(species = "Semipalmated Plover")

watr_pred <- ggpredict(waterbird_model, terms = c("day_from_start[0:30]", "temp_binary")) %>%
  as.data.frame() %>% mutate(species = "Red-throated Loon")

all_pred <- bind_rows(pass_pred, shor_pred, watr_pred) %>%
  mutate(Temperature = group,
         Temperature = fct_recode(Temperature, "Cool" = "low", "Warm" = "high"),
         Date = as.Date(x + start),
         species = factor(species, levels = c("Lapland Longspur", "Semipalmated Plover", "Red-throated Loon")))

# ----------------------------
# Create Calling Response to Temperature Plot  
# ----------------------------

# Create Predicted Calling Response Plot ####
calling_temp_resp <-
  ggplot(data = all_pred, aes(x = Date, y = predicted, colour = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.13, colour = NA) +
  scale_colour_manual(name = "Temperature", values = temp_palette,
                      labels = c("Warm", "Cold"), guide = guide_legend(nrow = 1)) +
  # Remove the fill legend since it is redundant
  scale_fill_manual(name = "Temperature", values = temp_palette,
                    labels = c("Warm", "Cold"), guide = "none") +
  labs(x = NULL, y = "Predicted Bird Call Detections") +
  theme_minimal(base_family = "Helvetica") +
  theme_half_open(font_size = 12) +
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        strip.text = element_blank(),
        strip.placement = "outside",
        #strip.text = element_text(size = 10, face = "bold"),
        panel.spacing = unit(0.02, "npc"),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        legend.position = "bottom"
        #legend.position = "none"
        ) +
  facet_wrap(~species, nrow = 1, scale = "free", strip.position = "top") +
  scale_y_continuous(limits = c(0, NA))


# Build Faceted Ridgeline Plot ####
species_facet <- c("Lapland Longspur", "Semipalmated Plover", "Red-throated Loon")

all_ridgeline_data <- bind_rows(lapply(species_facet, function(species_of_interest) {
  # Filter data for the species and remove unwanted ARU locations
  tmp <- aru_daily %>% 
    filter(common_name == species_of_interest,
           !locationID %in% c("ARUQ18", "ARUQ17", "ARUQ8"))
  # Compute smoothed detection counts using a 3-day rolling mean with a log transform
  tmp <- tmp %>% mutate(smoothed_total_count = zoo::rollmean(log(total_count + 1), k = 3, fill = NA))
  # Create a numeric version of the ARU identifier for ordering
  tmp <- tmp %>% mutate(aru_num = as.numeric(gsub("ARUQ", "", locationID)))
  # Order locations: higher temperature (or higher ARU number) first
  ordered_locations <- tmp %>% arrange(desc(temp_binary), desc(aru_num)) %>% pull(locationID) %>% unique()
  # Convert species to a factor with the desired order and set new factor level for locationID
  tmp %>% mutate(species = factor(species_of_interest, levels = species_facet),
                 locationID = factor(locationID, levels = ordered_locations))
}))

# Create the ridgeline plot with modified y-axis labels
faceted_plot <- ggplot(all_ridgeline_data, aes(x = date, y = locationID, height = smoothed_total_count, fill = temp_binary)) +
  geom_ridgeline(stat = "identity", scale = 0.4, alpha = 0.7) +
  geom_vline(xintercept = as.numeric(total_range), linetype = "dashed", color = "black", size = 1) +
  scale_fill_manual(name = "Temperature", values = temp_palette) +
  labs(x = "Date", y = "Smoothed and Log-Transformed Detections", fill = "Temperature") +
  theme_minimal(base_family = "Helvetica") +
  theme_half_open(font_size = 12) +
  theme(#legend.position = "bottom",
        legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside",
        #strip.text = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(),
        axis.text.y = element_text(),
        plot.title = element_blank(),
        #plot.title = element_text(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_x_date(date_labels = "%b %d", date_breaks = "10 days") +
  coord_cartesian(xlim = c(as.Date("2024-06-20"), as.Date("2024-08-20"))) +
  facet_wrap(~species, nrow = 1, 
             strip.position = "top") +
  # Transform y axis labels to remove "ARUQ" prefix
  scale_y_discrete(labels = function(x) { gsub("ARUQ", "", x) })

# Combine the Two Plots Using Patchwork ####
# combined_plot <- calling_temp_resp / faceted_plot #+ plot_layout(guides = "collect")
combined_plot <- faceted_plot / calling_temp_resp  #+ plot_layout(guides = "collect")
combined_plot <- combined_plot & theme(plot.background = element_rect(fill = "transparent", color = NA))
combined_plot

output_summary_folder <- "outputs/aru/model_figs"
dir.create(output_summary_folder, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_summary_folder, "calling_model.png"), 
       combined_plot,
       width = 10, height = 8, bg = "transparent")



# ----------------------------
# Create Model Summary Tables
# ----------------------------
aru_model_summary_table <- function(model, title = "Model Summary", plot_name = "model_table") {
  recode_vars <- c(
    "intercept" = "Intercept",
    "Intercept" = "Intercept",
    "(Intercept)" = "Intercept",
    "(intercept)" = "Intercept",
    "day_from_start" = "Date",
    "temp_binarylow" = "Low Temperature",
    "day_from_start:temp_binarylow" = "Date:Low Temperature"
  )
  model_output <- summary(model)$fixed %>%
    tibble::rownames_to_column(var = "Parameter") %>%
    mutate(across(c(Estimate, Est.Error, `l-95% CI`, `u-95% CI`), round, 3)) %>%
    mutate(across(c(Bulk_ESS, Tail_ESS), round, 0)) %>%
    mutate(across(c(Rhat), round, 3)) %>%
    rename("Estimate" = Estimate,
           "Est.Error" = Est.Error,
           "Lower 95% CI" = `l-95% CI`,
           "Upper 95% CI" = `u-95% CI`,
           "R-hat" = Rhat,
           "Bulk ESS" = Bulk_ESS,
           "Tail ESS" = Tail_ESS) %>%
    mutate(Parameter = recode(Parameter, !!!recode_vars)) %>%
    mutate(Parameter = factor(Parameter, levels = c("Intercept", "Date", "Low Temperature", "Date:Low Temperature"))) %>%
    arrange(Parameter)
  
  model_table <- model_output %>% gt() %>%
    tab_header(title = title) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels(everything())) %>%
    tab_style(style = list(cell_fill(color = "gainsboro"),
                           cell_text(style = "italic")),
              locations = cells_body(rows = `Lower 95% CI` > 0 & `Upper 95% CI` > 0)) %>%
    tab_style(style = list(cell_text(style = "italic"),
                           cell_fill(color = "gainsboro")),
              locations = cells_body(rows = `Lower 95% CI` < 0 & `Upper 95% CI` < 0)) %>%
    tab_style(style = cell_borders(sides = "top", color = "black", weight = px(2)),
              locations = cells_body(rows = Parameter == "Intercept")) %>%
    tab_style(style = cell_borders(sides = "top", color = "grey", weight = px(1)),
              locations = cells_body(rows = Parameter == "Date"))
  
  output_html <- paste0(output_fig_folder, "/", plot_name, ".html")
  #output_img  <- paste0(output_fig_folder, "/", plot_name, ".png")
  gtsave(model_table, output_html)
  #webshot(output_html, file = output_img, vwidth = 200, vheight = 160, zoom = 2)
  cat("Table saved to:", output_html)
  #cat("Image saved to:", output_img)
}

# Save summary tables for each model
aru_model_summary_table(model = passerine_model, title = "Lapland Longspur Calling Response", plot_name = "passerine_table_06feb2025")
aru_model_summary_table(model = shorebird_model, title = "Semipalmated Plover Calling Response", plot_name = "shorebird_table_06feb2025")
aru_model_summary_table(model = waterbird_model, title = "Red-Throated Loon Calling Response", plot_name = "waterbird_table_06feb2025")

# ----------------------------
# ARU Active Dates Visualization
# ----------------------------
# Define directories and columns to remove
base_dir <- "D:/ARU_QHI_2024"
aru_all_output_folder <- "D:/ARU_QHI_2024/output_total"
columns_to_remove <- c("lat", "lon", "week", "overlap", "sensitivity",
                       "species_list", "dateTimeRecorder", "min_conf")

# Function to extract locationID from recordingID
extract_locationID <- function(dt) {
  dt[, locationID := sub("^(ARUQ\\d+)B?\\D.*", "\\1", recordingID)]
  return(dt)
}

# Read the to-ignore list
toignore <- read_csv(paste0(base_dir, "/full_toignore.csv"))

# Function to process all CSV files in the ARU folder
process_aru_folder <- function(folder_path, cols_to_remove, toignore) {
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  lapply(csv_files, function(file_path) {
    dt <- fread(file_path)
    dt <- dt[, !cols_to_remove, with = FALSE]
    dt <- dt[!recordingID %in% toignore$File]
    dt <- dt[str_detect(recordingID, "^ARU") & str_detect(recordingID, "(0000|3000)\\.wav$")]
    dt <- extract_locationID(dt)
    return(dt)
  })
}

# Process the ARU activity data
aru_activity <- process_aru_folder(aru_all_output_folder, columns_to_remove, toignore)

aru_activity[[7]][, dateTimeLocal := as.POSIXct(ifelse(
  grepl("^\\d{4}-\\d{2}-\\d{2}$", dateTimeLocal),
  paste0(dateTimeLocal, " 00:00:00"),
  dateTimeLocal
))]

# Combine the dates from all ARU activity data.tables
unique_dates <- unique(rbindlist(lapply(aru_activity, function(dt) {
  dt[, .(dateTimeLocal = as.POSIXct(dateTimeLocal, tz = "UTC"), locationID)]
})))

# Order the data and filter to include only ARUQ related IDs
setorder(unique_dates, locationID, dateTimeLocal)
unique_dates <- unique_dates[grepl("^ARUQ", locationID)]

# Calculate the time differences (in minutes) between records within each location
unique_dates[, time_diff := difftime(dateTimeLocal, shift(dateTimeLocal, type = "lag"), units = "mins"), by = locationID]

# Create segment IDs whenever the time gap is > 30 minutes or NA
unique_dates[, segment_id := cumsum(is.na(time_diff) | time_diff > 30), by = locationID]

# Define the start and end time for each segment
segments <- unique_dates[, .(start = min(dateTimeLocal), end = max(dateTimeLocal)), 
                         by = .(locationID, segment_id)]
segments[, locationNum := as.numeric(sub("ARUQ", "", locationID))]

# Define the highlight time for the non-working ARU segment.
highlight_time <- as.POSIXct("2024-06-28 16:30:00")

# Mark segments as "not_working" if they start after highlight_time for ARUQ8,
# otherwise "working". (Existing assignment; will be updated after potential splits.)
segments[, working := ifelse(locationID == "ARUQ8" & start > highlight_time, "not_working", "working")]

# For ARUQ8, find segments that span the highlight time
to_split <- segments[locationID == "ARUQ8" & start < highlight_time & end > highlight_time]

if (nrow(to_split) > 0) {
  # Create a segment that ends at highlight_time (pre-highlight)
  seg_before <- copy(to_split)
  seg_before[, end := highlight_time]
  seg_before[, working := "working"]
  
  # Create a segment that begins at highlight_time (post-highlight)
  seg_after <- copy(to_split)
  seg_after[, start := highlight_time]
  seg_after[, working := "not_working"]
  
  # Remove the original segments that spanned the highlight time
  segments <- segments[!(locationID == "ARUQ8" & start < highlight_time & end > highlight_time)]
  
  # Combine the original segments with the two new split segments
  segments <- rbind(segments, seg_before, seg_after)
  
  # Ensure the segments are ordered by location and time
  setorder(segments, locationID, start)
}
# For ARUQ8, reassign segment_id according to the order of start times
segments[locationID == "ARUQ8", segment_id := seq_len(.N), by = locationID]

# # Define the highlight time for the non-working ARU segment.
# highlight_time <- as.POSIXct("2024-06-28 16:30:00")
# 
# # Mark segments as "not_working" (previously colored red) if they meet the condition
# segments[, working := ifelse(locationID == "ARUQ8" & start > highlight_time, "not_working", "working")]

# Create a cleaned version of the locationID that removes the "ARUQ" text for y-axis labels
#segments[, locationNum := factor(locationNum, levels = mixedsort(unique(locationNum)))]
segments$locationNum <- factor(segments$locationNum, levels = mixedsort(unique(segments$locationNum)))

# Incorporate ARU_average_temps for the temperature-based colour scale
# Define the temperature palette
temp_palette <- c("high" = "#E63946", "low" = "#457B9D")

segments <- merge(segments, ARU_average_temps, by.x = "locationID", by.y = "aru_name", all.x = TRUE)

# Plot the segments using the temperature-based colour scheme
aru_runtime <- ggplot(segments) +
  geom_segment(aes(x = start, xend = end,
                   y = locationNum, yend = locationNum,
                   color = temp_binary, alpha = working),
               size = 3) +
  scale_color_manual(values = temp_palette) +
  # Set solid lines for working segments and dashed for not working segments
  scale_alpha_manual(values = c("working" = 1, "not_working" = 0)) +
  scale_x_datetime(date_breaks = "7 days", date_labels = "%b %d") +
  labs(x = "Date", y = "ARU #") +
  scale_y_discrete(limits = rev(levels(segments$locationNum))) +
  theme_minimal() + 
  theme_minimal_hgrid() +
  theme(legend.position = "none")

output_summary_folder <- "outputs/aru/summary"
dir.create(output_summary_folder, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_summary_folder, "aru_runtime.png"), 
       aru_runtime,
       width = 10, height = 4, bg = "transparent")


# ----------------------------
# ARU Total Hours Active
# ----------------------------
unique_dates
aru_total_hours <- unique_dates %>%
  group_by(locationID) %>%
  summarise(total_recording = length(unique(dateTimeLocal))) %>%
  mutate(
    min_recording = total_recording * 10,
    hour_recording = min_recording/60
  ) 

sum(aru_total_hours$hour_recording)
#7268.16 hours

#########################3

# === Species Detection Bar Graph ===
# (Assumes aru_analysis_dataframe is available; adjust accordingly)
# top_species_counts <- aru_analysis_dataframe %>%
#   group_by(common_name) %>%
#   summarise(observations = n()) %>%
#   arrange(desc(observations)) %>%
#   slice_head(n = 30)
# ggplot(top_species_counts, aes(x = reorder(common_name, -observations), y = observations)) +
#   geom_bar(stat = "identity", fill = "black") +
#   theme_minimal() +
#   labs(x = "Species", y = "Number of Detections") +
#   theme_half_open(font_size = 12) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# === Species Detection Over Time: Ridgeline Plots ===
create_ridgeline_plot <- function(species_of_interest, scaling = 0.02, log = FALSE) {
  filtered_single_species <- aru_daily %>%
    filter(common_name == species_of_interest & !locationID %in% c("ARUQ18", "ARUQ17"))
  if (log) {
    filtered_single_species$smoothed_total_count <- zoo::rollmean(log(filtered_single_species$total_count), k = 3, fill = NA)
    y_axis_label <- "Log Smoothed Detections"
    plot_title <- paste("Log Smoothed Detections for", species_of_interest)
  } else {
    filtered_single_species$smoothed_total_count <- zoo::rollmean(filtered_single_species$total_count, k = 3, fill = NA)
    y_axis_label <- "Smoothed Detections"
    plot_title <- paste("Smoothed Detections for", species_of_interest)
  }
  # Reorder locationID for consistency
  filtered_single_species <- filtered_single_species %>%
    mutate(locationID = factor(locationID, levels = c("ARUQ0", "ARUQ1", "ARUQ2", "ARUQ3",
                                                      "ARUQ4", "ARUQ5", "ARUQ6", "ARUQ7",
                                                      "ARUQ8", "ARUQ9", "ARUQ10", "ARUQ11",
                                                      "ARUQ12", "ARUQ13", "ARUQ14", "ARUQ15",
                                                      "ARUQ16", "ARUQ19")))
  aru_daily_mod <- aru_daily %>% mutate(aru_num = as.numeric(gsub("ARUQ", "", locationID)))
  ordered_locations <- aru_daily_mod %>% arrange(desc(temp_binary), aru_num) %>% pull(locationID) %>% unique()
  ggplot(filtered_single_species, aes(x = date, y = locationID, group = locationID, height = smoothed_total_count, fill = locationID)) +
    geom_ridgeline(stat = "identity", scale = scaling, alpha = 0.7) +
    labs(title = plot_title, x = "Date", y = y_axis_label) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title = element_text(face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
    coord_cartesian(xlim = c(as.Date("2024-06-20"), as.Date("2024-08-20"))) +
    scale_y_discrete(limits = ordered_locations)
}

# Save ridgeline plots for several species
output_summary_folder <- "outputs/aru/summary"
dir.create(output_summary_folder, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_summary_folder, "Savannah_Sparrow_log_ridgeline_plot.png"), 
       create_ridgeline_plot("Savannah Sparrow", scaling = 0.7, log = TRUE),
       width = 10, height = 6)
ggsave(file.path(output_summary_folder, "Lapland_Longspur_log_ridgeline_plot.png"), 
       create_ridgeline_plot("Lapland Longspur", scaling = 0.7, log = TRUE),
       width = 10, height = 6)
ggsave(file.path(output_summary_folder, "Common_Eider_log_ridgeline_plot.png"), 
       create_ridgeline_plot("Common Eider", scaling = 1, log = TRUE),
       width = 10, height = 6)
ggsave(file.path(output_summary_folder, "Common_Redpoll_log_ridgeline_plot.png"), 
       create_ridgeline_plot("Common Redpoll", scaling = 0.7, log = TRUE),
       width = 10, height = 6)
ggsave(file.path(output_summary_folder, "Semipalmated_Plover_log_ridgeline_plot.png"), 
       create_ridgeline_plot("Semipalmated Plover", scaling = 0.7, log = TRUE),
       width = 10, height = 6)

(exp((1.130-1.174) + (0.003-0.026)*30)-exp(1.130-1.174))/30
