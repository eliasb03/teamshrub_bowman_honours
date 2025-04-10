# ARU visualization script
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(cowplot)
library(brms)
library(gtools) 
library(ggridges)
library(zoo)

##############

passerine_model <- readRDS(file = paste0(model_output_path, "passerine.rds"))
waterbird_model <- readRDS(file = paste0(model_output_path, "waterbird_redthroatedloon.rds"))
shorebird_model <- readRDS(file = paste0(model_output_path, "shorebird.rds"))


# Generate predicted values for day_from_start by temp_binary
pass_pred <- ggpredict(passerine_model, terms = c("day_from_start[0:30]", "temp_binary")) %>%
  as.data.frame() %>%
  mutate(species = "Lapland Longspur")

shor_pred <- ggpredict(shorebird_model, terms = c("day_from_start[0:30]", "temp_binary")) %>%
  as.data.frame() %>%
  mutate(species = "Semipalmated Plover")

watr_pred <- ggpredict(waterbird_model, terms = c("day_from_start[0:30]", "temp_binary")) %>%
  as.data.frame() %>%
  mutate(species = "Red-throated Loon")
#mutate(species = "Common Eider")

all_pred <- bind_rows(
  pass_pred,
  shor_pred,
  watr_pred
) %>% mutate(Temperature = group) %>%
  mutate(Temperature = fct_recode(Temperature, 
                                  "Cool" = "low",
                                  "Warm" = "high"),
         Date = as.Date(x + start))

aru_plotting_data <- aru_summary %>%
  filter(common_name %in% c("Lapland Longspur", "Semipalmated Plover", "Red-throated Loon")) %>%
  mutate(Temperature = temp_binary) %>%
  filter(complete.cases(.)) %>%
  mutate(Temperature = fct_recode(Temperature, 
                                  "Cool" = "low",
                                  "Warm" = "high"),
         Date = as.Date(date))


calling_temp_resp <- 
  ggplot(data = all_pred, aes(x = Date, y = predicted, colour = group)) +
  geom_line(linewidth = 1) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.13, colour = NA) +  # Fill ribbons
  scale_colour_manual(name = "Temperature",
                      values = temp_palette,
                      labels = c("Warm", "Cold"),
                      guide = guide_legend(nrow = 1)) +  # Consistent legend
  scale_fill_manual(name = "Temperature",
                    values = temp_palette,
                    labels = c("Warm", "Cold"),
                    guide = guide_legend(nrow = 1)) +  # Matching fill colors
  labs(x = NULL, 
       y = "Predicted Bird Call Detections") +
  theme_minimal(base_family = "Helvetica") +
  theme_half_open(font_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 10, face = "bold"),
    panel.spacing = unit(0.02, "npc"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    legend.position = "bottom"
  ) +
  facet_wrap(~species, nrow = 1, scale = "free",
             strip.position = "top") +
  scale_y_continuous(limits = c(0, NA))

calling_temp_resp

ggsave("outputs/aru/model_figs/calling_temp_model_17mar2025.pdf", plot = calling_temp_resp, width = 10, height = 4, units = "in")

ggsave("outputs/aru/model_figs/calling_temp_model_17mar2025.png", plot = calling_temp_resp, width = 12, height = 4, units = "in")



##############


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

#--------------------------
#
#--------------------------


# Plotting ARU Active Dates ####
base_dir <- "D:/ARU_QHI_2024"
aru_all_output_folder <- "D:/ARU_QHI_2024/output_total"

columns_to_remove <- c(
  "lat",
  "lon",
  "week",
  "overlap",
  "sensitivity",
  "species_list",
  "dateTimeRecorder",
  "min_conf"
)


# Define a function to extract locationID from recordingID
extract_locationID <- function(dt) {
  dt[, locationID := sub("^(ARUQ\\d+)B?\\D.*", "\\1", recordingID)]
  return(dt)
}

# Remove faulty files
toignore <- read_csv(paste0(base_dir, "/full_toignore.csv"))

# Define a function to process each file
process_aru_folder <- function(folder_path,
                               cols_to_remove,
                               toignore) {
  # Get a list of CSV files in the folder
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Process each CSV file
  lapply(csv_files, function(file_path) {
    # Read the CSV using fread
    dt <- fread(file_path)
    
    # Remove unwanted columns
    dt <- dt[, !cols_to_remove, with = FALSE]
    
    # Remove rows with file paths in the `toignore` dataset
    dt <- dt[!recordingID %in% toignore$File]
    
    # Filter out rows where recordingID doesn't start with "ARU" 
    # or doesn't end with "0000.wav" or "3000.wav"
    dt <- dt[str_detect(recordingID, "^ARU") & str_detect(recordingID, "(0000|3000)\\.wav$")]
    
    # Extract locationID from recordingID
    dt <- extract_locationID(dt)
    
    
    
    return(dt)
  })
}

# Process all folders
aru_activity <- process_aru_folder(aru_all_output_folder,
                                   columns_to_remove,
                                   toignore)


# aru_activity[[7]][, dateTimeLocal := as.POSIXct(ifelse(
#   grepl("^\\d{4}-\\d{2}-\\d{2}$", dateTimeLocal),
#   paste0(dateTimeLocal, " 00:00:00"),
#   dateTimeLocal
# ))]

unique_dates <- unique(rbindlist(lapply(aru_activity, function(dt) {
  dt[, .(dateTimeLocal = as.POSIXct(dateTimeLocal, tz = "UTC"), locationID)]  # Keep both columns
})))

# Order data by locationID and dateTimeLocal
setorder(unique_dates, locationID, dateTimeLocal)

unique_dates <- unique_dates[grepl("^ARUQ", locationID)]

# Create an ID for consecutive sequences within each locationID
unique_dates[, time_diff := difftime(dateTimeLocal, shift(dateTimeLocal, type = "lag"), units = "mins"), by = locationID]
unique_dates[, segment_id := cumsum(is.na(time_diff) | time_diff > 30), by = locationID]  # Use 30 mins gap to split segments

# Summarize start and end times for each segment
segments <- unique_dates[, .(start = min(dateTimeLocal), end = max(dateTimeLocal)), by = .(locationID, segment_id)]

# Reorder locationID with mixedsort for natural numeric order

# Create a new column with numeric labels only (remove "ARUQ")
segments[, locationNum := as.numeric(sub("ARUQ", "", locationID))]

# Special modifications for ARUQ8 segments after June 28th, 16:30:00
highlight_time <- as.POSIXct("2024-06-28 16:30:00")
segments[, color := ifelse(locationID == "ARUQ8" & start > highlight_time, "red", "black")]

segments[, locationID := factor(locationID, levels = mixedsort(unique(locationID)))]

# Plot using ggplot2
ggplot(segments) +
  geom_segment(aes(x = start, xend = end, y = locationID, yend = locationID, color = color), size = 2) +
  scale_color_identity() +  # Use specified colors directly
  scale_y_discrete(limits = rev) +  # Reverse y-axis for better readability
  scale_x_datetime(
    date_breaks = "7 days",   # More frequent ticks, adjust as needed
    date_labels = "%b %d"  # Custom format: "Jun 28\n16:30"
  ) +
  labs(x = "Date", y = NULL) +
  theme_minimal() +
  theme_minimal_hgrid()

# Plotting all species detection above 0.5 ####
str(aru_analysis_dataframe)

# Summarize data to count observations per species and get top 25
top_species_counts <- aru_analysis_dataframe %>%
  group_by(common_name) %>%
  summarise(observations = n()) %>%
  arrange(desc(observations)) %>%
  slice_head(n = 30)  # Select top 25 species

# Plot bar graph ####
ggplot(top_species_counts, aes(x = reorder(common_name, -observations), y = observations)) +
  geom_bar(stat = "identity", fill = "black") +
  theme_minimal() +
  labs(x = "Species",
       y = "Number of Detections") +
  theme_half_open(font_size = 12) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plotting species detection over time ####
library(ggridges)
library(zoo)

# Define the function for creating the ridgeline plot
create_ridgeline_plot <- function(species_of_interest, scaling = 0.02, log = FALSE) {
  
  # Filter data for the selected species and exclude ARUQ18 and ARUQ17
  filtered_single_species <- aru_daily %>%
    filter(common_name == species_of_interest & !locationID %in% c("ARUQ18", "ARUQ17"))
  
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
                                                      "ARUQ10", "ARUQ11", "ARUQ12", "ARUQ13", "ARUQ14", "ARUQ15", "ARUQ16", "ARUQ19")))
  
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
    arrange(desc(temp_binary), aru_num) %>%
    pull(locationID) %>%
    unique()
  
  ggplot(filtered_single_species, aes(x = date, y = locationID, group = locationID, height = smoothed_total_count, fill = locationID)) +
    geom_ridgeline(stat = "identity", scale = scaling, alpha = 0.7) +
    labs(title = plot_title,
         x = "Date",
         y = y_axis_label) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title = element_text(face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
    coord_cartesian(xlim = c(as.Date("2024-06-20"), as.Date("2024-08-20"))) +
    scale_y_discrete(limits = ordered_locations)  # Apply the new y-axis order
  
}

create_ridgeline_plot("Semipalmated Plover", scaling = 0.7, log = TRUE)
create_ridgeline_plot("Savannah Sparrow", scaling = 0.7, log = TRUE)
create_ridgeline_plot("Lapland Longspur", scaling = 0.7, log = TRUE)

create_ridgeline_plot("Lapland Longspur", scaling = 0.017, log = FALSE)

create_ridgeline_plot("Common Eider", scaling = 1, log = TRUE)
create_ridgeline_plot("Common Redpoll", scaling = 0.7, log = TRUE)

output_folder <- "outputs/aru/summary"
ggsave(file.path(output_folder, "Savannah_Sparrow_log_ridgeline_plot.png"), 
       create_ridgeline_plot("Savannah Sparrow", scaling = 0.7, log = TRUE),
       width = 10, height = 6)

ggsave(file.path(output_folder, "Lapland_Longspur_log_ridgeline_plot.png"), 
       create_ridgeline_plot("Lapland Longspur", scaling = 0.7, log = TRUE),
       width = 10, height = 6)

ggsave(file.path(output_folder, "Common_Eider_log_ridgeline_plot.png"), 
       create_ridgeline_plot("Common Eider", scaling = 1, log = TRUE),
       width = 10, height = 6)

ggsave(file.path(output_folder, "Common_Redpoll_log_ridgeline_plot.png"), 
       create_ridgeline_plot("Common Redpoll", scaling = 0.7, log = TRUE),
       width = 10, height = 6)

ggsave(file.path(output_folder, "Semipalmated_Plover_log_ridgeline_plot.png"), 
       create_ridgeline_plot("Semipalmated Plover", scaling = 0.7, log = TRUE),
       width = 10, height = 6)

###
ggsave(file.path(output_folder, "Savannah_Sparrow_log_ridgeline_plot.pdf"), 
       create_ridgeline_plot("Savannah Sparrow", scaling = 0.7, log = TRUE),
       width = 10, height = 6)

ggsave(file.path(output_folder, "Lapland_Longspur_log_ridgeline_plot.pdf"), 
       create_ridgeline_plot("Lapland Longspur", scaling = 0.7, log = TRUE),
       width = 10, height = 6)

ggsave(file.path(output_folder, "Common_Eider_log_ridgeline_plot.pdf"), 
       create_ridgeline_plot("Common Eider", scaling = 1, log = TRUE),
       width = 10, height = 6)

ggsave(file.path(output_folder, "Common_Redpoll_log_ridgeline_plot.pdf"), 
       create_ridgeline_plot("Common Redpoll", scaling = 0.7, log = TRUE),
       width = 10, height = 6)

ggsave(file.path(output_folder, "Semipalmated_Plover_log_ridgeline_plot.pdf"), 
       create_ridgeline_plot("Semipalmated Plover", scaling = 0.7, log = TRUE),
       width = 10, height = 6)



#######
ggsave(file.path(output_folder, "Bluethroat_ridgeline_plot.pdf"), 
       create_ridgeline_plot("Bluethroat", scaling = 1, log = FALSE),
       width = 10, height = 6)


ggsave(file.path(output_folder, "Lapland_Longspur_ridgeline_plot.pdf"), 
       create_ridgeline_plot("Lapland Longspur", scaling = 0.015, log = FALSE),
       width = 10, height = 6)

#####
# Define the species of interest
species_of_interest <- "Lapland Longspur"  # Change this to your desired species

# Filter data for the selected species and exclude ARUQ18 and ARUQ17
filtered_single_species <- aru_daily %>%
  filter(common_name == species_of_interest & !locationID %in% c("ARUQ18", "ARUQ17"))

# Apply 7-day rolling mean to the 'total_count' column
filtered_single_species$smoothed_total_count <- zoo::rollmean(filtered_single_species$total_count, k = 7, fill = NA)

# Create a numeric factor for ordering if needed (optional)
filtered_single_species <- filtered_single_species %>%
  mutate(ARU_num = as.factor(as.numeric(sub("ARUQ", "", locationID))))

# Reorder the locationID factor to ensure 'ARUQ0' is first, and others follow in order
filtered_single_species <- filtered_single_species %>%
  mutate(locationID = factor(locationID, levels = c("ARUQ0", "ARUQ1", "ARUQ2", "ARUQ3", "ARUQ4", "ARUQ5", "ARUQ6", "ARUQ7", "ARUQ8", "ARUQ9", "ARUQ10", "ARUQ11", "ARUQ12", "ARUQ13", "ARUQ14", "ARUQ15", "ARUQ16", "ARUQ19")))


# Create the ridgeline plot
ggplot(filtered_single_species, aes(x = date, y = locationID, group = locationID, height = smoothed_total_count, fill = locationID)) +
  geom_ridgeline(stat = "identity", scale = 0.02, alpha = 0.7) +  # Increased scale for more separation
  labs(title = paste("Smoothed Detections for", species_of_interest),
       x = "Date",
       y = "Smoothed Detections") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Bold x-axis text
        axis.text.y = element_text(face = "bold"),  # Bold y-axis text
        plot.title = element_text(face = "bold"),  # Bold plot title
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()) +  # Remove minor grid lines 
  scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
  coord_cartesian(xlim = c(as.Date("2024-06-20"), as.Date("2024-08-20"))) +
  scale_y_discrete(limits = rev(levels(filtered_single_species$locationID)))  # Reverse the y-axis order
#####

# 
# # Plot the smoothed data with the moving average
# ggplot(filtered_single_species, aes(x = date, y = smoothed_total_count, color = locationID)) +
#   geom_line() +  # Plot the smoothed line
#   scale_y_log10() +  # Log scale for y-axis
#   theme_minimal() +
#   labs(title = paste("Smoothed Detections Over Time for", species_of_interest),
#        x = "Date",
#        y = "Total Count (Log Scale)",
#        color = "Location") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Plot the ridgeline plot with time on the x-axis
# ggplot(filtered_single_species, aes(x = date, y = locationID, height = smoothed_total_count, fill = locationID)) +
#   geom_density_ridges(scale = 3) +  # Add ridgelines
#   theme_minimal() +
#   labs(title = paste("Smoothed Detections Over Time for", species_of_interest),
#        x = "Date",
#        y = "Location",
#        fill = "Location") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_x_date(date_labels = "%b %d", date_breaks = "1 month") 
# 
# # Plot detections over time on a log scale, split by locationID
# ggplot(filtered_daily_data, aes(x = date, y = total_count, color = common_name)) +
#   geom_line() +
#   scale_y_log10() +  # Log scale for y-axis
#   facet_wrap(~locationID) +
#   theme_minimal() +
#   labs(title = "Daily Detections Over Time for Selected Species",
#        x = "Date",
#        y = "Total Count (Log Scale)",
#        color = "Species") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 
# # Bluethroat plot ####
# aru_analysis_dataframe %>%
#   filter(common_name == "Bluethroat") %>%
#   ggplot(aes(x = time_interval, y = species_count, colour = locationID)) +
#   geom_point() +
#   labs(title = "Bluethroat Detection Over Time",
#        x = "Time Interval",
#        y = "Species Count") +
#   theme_half_open() + 
#   # set y axis min to 0
#   scale_y_continuous(limits = c(0, NA))
#   
# 

##########

confidence_threshold <- 0.5 # Confidence threshold used by BirdNET in the output I want

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
  c("Lapland Longspur", "Semipalmated Plover", "Common Eider")



# Filter ARU data to only include species in the species list
aru_dataframe <- aru_dataframe %>%
  filter(common_name %in% species_list) %>%
  mutate(date = as.Date(time_interval))

# Summarize ARU data to daily level
aru_daily <- aru_dataframe %>%
  group_by(locationID, date, common_name) %>%
  summarize(total_count = sum(species_count)) %>%
  ungroup() %>%
  left_join(ARU_average_temps, by = c("locationID" = "aru_name")) %>%
  mutate(temp_binary = as.factor(temp_binary))

period_length <- 20 # 20 days
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

# Filtering by taxa
passerine <- aru_summary %>%
  filter(common_name == "Lapland Longspur")
shorebird <- aru_summary %>%
  filter(common_name == "Semipalmated Plover")
waterbird <- aru_summary %>%
  filter(common_name == "Common Eider")


