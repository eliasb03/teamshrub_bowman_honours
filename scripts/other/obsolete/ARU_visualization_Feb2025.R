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




# Bluethroat plot ####
aru_analysis_dataframe %>%
  filter(common_name == "Bluethroat") %>%
  ggplot(aes(x = time_interval, y = species_count, colour = locationID)) +
  geom_point() +
  labs(title = "Bluethroat Detection Over Time",
       x = "Time Interval",
       y = "Species Count") +
  theme_half_open() + 
  # set y axis min to 0
  scale_y_continuous(limits = c(0, NA))
  


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


