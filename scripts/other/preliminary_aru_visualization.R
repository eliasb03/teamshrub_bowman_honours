# preliminary_aru_visualization


# Define the function
plot_frequency_distribution <- function(input_vector, top_n = 15, plot_title = "Frequency Distribution") {
  # Create a frequency table
  freq_table <- as.data.frame(table(input_vector)) %>%
    arrange(desc(Freq))
  
  # Separate top n and combine others
  top_items <- freq_table[1:top_n, ]
  others_count <- sum(freq_table$Freq[(top_n + 1):nrow(freq_table)])
  others <- data.frame(input_vector = "Other", Freq = others_count)
  
  # Combine top items with "Other" and position "Other" at the end
  final_table <- bind_rows(top_items, others)
  
  # Convert input_vector to a factor with levels ensuring "Other" is last
  final_table$input_vector <- factor(final_table$input_vector, 
                                     levels = c(as.character(top_items$input_vector), "Other"))
  
  # Plotting the bar chart
  ggplot(final_table, aes(x = input_vector, y = Freq)) +
    geom_bar(stat = "identity", fill = "black") +
    labs(title = plot_title, x = "Species", y = "Number of Detections") +
    theme_minimal() +
    theme_half_open(font_size = 14) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
  
}

# importing the 3 processed ARU data
aruq3_records <- read.csv(
  "D:/ARU_QHI_2024/ARUQ3_21Aug2024/Output/ARUQ3_21Aug2024.BirdNET_formatted_results.csv")
aruq5_records <- read.csv(
  "D:/ARU_QHI_2024/ARUQ5_20Aug2024/Output/ARUQ5_20Aug2024.BirdNET_formatted_results.csv")
aruq6_records <- read.csv(
  "D:/ARU_code_optimization_data/ARUQ6_17Aug2024/Output/ARUQ6_17Aug2024.BirdNET_formatted_results.csv")

# binding arus
three_arus <- bind_rows(aruq3_records, aruq5_records, aruq6_records)

three_arus_processed <- three_arus %>%
  mutate(name = recordingID %>% str_extract("ARUQ[0-9]+")) %>% # Create an ARU name column based on characters before _ in recording ID
  filter(name %in% c("ARUQ3", "ARUQ5", "ARUQ6")) %>% # remove observations where aru isn't "ARUQ3", "ARUQ5", or "ARUQ6"
  left_join(aru_locations %>% # join with metadata regarding aru_locations
              select(name, unit_name, grid_code, geometry), 
            by = "name") %>%
  mutate(
    locationID = case_when(
      name == "ARUQ3" ~ "upland",
      name %in% c("ARUQ5", "ARUQ6") ~ "creek bed",
      TRUE ~ locationID  # Retain existing value if no condition is met (NA in this case)
    )
  ) %>% 
  filter(confidence >= 0.5)




three_arus_processed <- three_arus %>%
  mutate(name = recordingID %>% str_extract("ARUQ[0-9]+")) %>% # Create an ARU name column based on characters before _ in recording ID
  filter(name %in% c("ARUQ3", "ARUQ5", "ARUQ6")) %>% # remove observations where aru isn't "ARUQ3", "ARUQ5", or "ARUQ6"
  left_join(aru_locations %>% # join with metadata regarding aru_locations
              select(name, unit_name, grid_code, geometry), 
            by = "name") %>%
  mutate(
    locationID = case_when(
      name == "ARUQ3" ~ "upland",
      name %in% c("ARUQ5", "ARUQ6") ~ "creek bed",
      TRUE ~ locationID  # Retain existing value if no condition is met (NA in this case)
    )
  ) %>% 
  filter(confidence >= 0.5)

plot_frequency_distribution(three_arus_processed$common_name, top_n = 10, "Top 10 most common species in Three ARUs data \n(0.5 confidence)")








aru3_processed <- aruq3_records %>%
  mutate(name = recordingID %>% str_extract("ARUQ[0-9]+")) %>% # Create an ARU name column based on characters before _ in recording ID
  filter(name %in% c("ARUQ3", "ARUQ5", "ARUQ6")) %>% # remove observations where aru isn't "ARUQ3", "ARUQ5", or "ARUQ6"
  left_join(aru_locations %>% # join with metadata regarding aru_locations
              select(name, unit_name, grid_code, geometry), 
            by = "name") %>%
  mutate(
    locationID = case_when(
      name == "ARUQ3" ~ "upland",
      name %in% c("ARUQ5", "ARUQ6") ~ "creek bed",
      TRUE ~ locationID  # Retain existing value if no condition is met (NA in this case)
    )
  ) %>% 
  filter(confidence >= 0.5)

plot_frequency_distribution(aru3_processed$common_name, top_n = 20, "Top 20 most common species in ARU 3 data \n(0.5 confidence)")


# 
# birdnet_heatmap(
#   data = three_arus_processed,
#   locationID = "upland",
#   common.name = 'Snow Bunting',
#   conf.threshold = 0.2,
#   dates.sampled = three_arus_processed$date,
#   #julian.breaks = seq(from = 173, to = 186, by = 1),
#   comparable.color.breaks = FALSE
# )
# 

###################################################
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Summarize data to get count of detections over time by site and species
bird_summary_hourly <- three_arus_processed %>%
  filter(common_name == "Lapland Longspur") %>%  # Specify the species of interest
  mutate(hourly_detection = floor_date(ymd_hms(detectionTimeLocal), "hour")) %>%  # Aggregate by hour
  group_by(hourly_detection, name) %>%
  summarise(call_count = n()) %>%
  ungroup()

# Plot the data
ggplot(bird_summary_hourly, aes(x = hourly_detection, y = call_count, color = name)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("ARUQ3" = "blue", "ARUQ5" = "green", "ARUQ6" = "red")) +
  labs(title = "Bird Calls Detected Over Time by Site",
       x = "Time",
       y = "Number of Bird Calls",
       color = "Site") +
  theme_minimal() +
  theme(text = element_text(size = 14))

#######

bird_summary_daily <- three_arus_processed %>%
  filter(common_name == "Lapland Longspur") %>%  # Specify the species of interest
  mutate(daily_detection = floor_date(ymd_hms(detectionTimeLocal), "day")) %>%  # Aggregate by hour
  group_by(daily_detection, name) %>%
  summarise(call_count = n()) %>%
  ungroup()

# Plot the data
ggplot(bird_summary_daily, aes(x = daily_detection, y = call_count, color = name)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("ARUQ3" = "blue", "ARUQ5" = "green", "ARUQ6" = "red")) +
  labs(title = "Bird Calls Detected Over Time by Site",
       x = "Time",
       y = "Number of Bird Calls",
       color = "Site") +
  theme_minimal() +
  theme(text = element_text(size = 14))


#####################
########### Depends on Jeremys Tomst Script
# ARUQ3 --> Tomst 29 
# ARUQ5 --> Tomst 39
# ARUQ6 --> Tomst 4

# Create new Dataset with data filtered for Tomst 29, Tomst 39, Tomst 4
# Filter data for Tomst 29, Tomst 39, Tomst 4
#########
# Weekly
aru_tomst_weekly <- weekly_values %>%
  filter(locality_id %in% c("TOMST29_QHI", "TOMST39_QHI", "TOMST4_QHI")) %>%
  mutate(aru = case_when(
    locality_id == "TOMST29_QHI" ~ "ARUQ3",
    locality_id == "TOMST39_QHI" ~ "ARUQ5",
    locality_id == "TOMST4_QHI" ~ "ARUQ6"
  )) %>%
  filter(sensor_name == "TMS_T3_mean") %>%
  filter(week > 20)


ggplot(aru_tomst_weekly, aes(x = week, y = mean_value, color = aru)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("ARUQ3" = "blue", "ARUQ5" = "green", "ARUQ6" = "red")) +
  labs(x = "Week of Year",
       y = "Temperature",
       color = "Site") +
  theme_minimal() +
  theme(text = element_text(size = 14))


#########
# Monthly
aru_tomst_monthly <- monthly_values %>%
  filter(locality_id %in% c("TOMST29_QHI", "TOMST39_QHI", "TOMST4_QHI")) %>%
  mutate(aru = case_when(
    locality_id == "TOMST29_QHI" ~ "ARUQ3",
    locality_id == "TOMST39_QHI" ~ "ARUQ5",
    locality_id == "TOMST4_QHI" ~ "ARUQ6"
  )) %>% 
  #mutate(date = as.Date(paste("2024", .$month, .$day, sep = "-"), format = "%Y-%m-%d")) %>%
  filter(sensor_name == "TMS_T3_mean") %>% 
  filter(month > 4)

ggplot(aru_tomst_monthly, aes(x = month, y = mean_value, color = aru)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("ARUQ3" = "blue", "ARUQ5" = "green", "ARUQ6" = "red")) +
  labs(x = "Month",
       y = "Temperature",
       color = "Site") +
  theme_minimal() +
  theme(text = element_text(size = 14))



summarize_bird_calls <- function(data, species, sites, time_aggregation = c("hourly", "daily", "weekly", "3-day", "2-day"), add_main_line = TRUE, log_scale = FALSE, add_smooth = FALSE, smoothing_method = NULL) {
  # Match argument to avoid typos
  time_aggregation <- match.arg(time_aggregation)
  
  # Filter by species and sites
  filtered_data <- data %>%
    filter(common_name == species, name %in% sites)
  
  # Create a time-aggregated column based on user selection
  summarized_data <- filtered_data %>%
    mutate(detection_time = case_when(
      time_aggregation == "hourly" ~ floor_date(ymd_hms(detectionTimeLocal), "hour"),
      time_aggregation == "daily" ~ floor_date(ymd_hms(detectionTimeLocal), "day"),
      time_aggregation == "weekly" ~ floor_date(ymd_hms(detectionTimeLocal), "week"),
      time_aggregation == "3-day" ~ floor_date(ymd_hms(detectionTimeLocal), "3 days"),
      time_aggregation == "2-day" ~ floor_date(ymd_hms(detectionTimeLocal), "2 days")
    )) %>%
    group_by(detection_time, name) %>%
    summarise(call_count = n(), .groups = "drop")
  
  # Plot the data
  plot <- ggplot(summarized_data, aes(x = detection_time, y = call_count, color = name)) +
    geom_point(size = 0.25) +
    scale_color_manual(values = c("ARUQ3" = "blue", "ARUQ5" = "green", "ARUQ6" = "red")) +
    labs(title = paste("Bird Calls Detected Over Time by Site -", species),
         x = "Time",
         y = "Number of Bird Calls",
         color = "Site") +
    theme_minimal() +
    theme(text = element_text(size = 14))

  
  # Add main line with optional smoothing method
  if (add_main_line) {
    if (!is.null(smoothing_method)) {
      plot <- plot + geom_smooth(method = smoothing_method, se = FALSE, size = 1)
    } else {
      plot <- plot + geom_line(size = 1)
    }
  }
  
  # Add log scale to y-axis if log_scale is TRUE
  if (log_scale) {
    plot <- plot + scale_y_log10()
  }
  
  # Add a smooth line if add_smooth is TRUE
  if (add_smooth) {
    plot <- plot + geom_smooth(se = FALSE)
  }
  
  return(list(summary = summarized_data, plot = plot))
}

summarize_bird_calls(
  data = three_arus_processed,
  species = "Lapland Longspur",
  sites = c("ARUQ3", "ARUQ5", "ARUQ6"),
  time_aggregation = "2-day",
  add_main_line = TRUE,
  log_scale = FALSE,
  add_smooth = FALSE
  #,smoothing_method = "loess" 
)

################################################################################
# Daily
aru_tomst_daily <- daily_values %>%
  filter(locality_id %in% c("TOMST29_QHI", "TOMST39_QHI", "TOMST4_QHI")) %>%
  mutate(aru = case_when(
    locality_id == "TOMST29_QHI" ~ "ARUQ3",
    locality_id == "TOMST39_QHI" ~ "ARUQ5",
    locality_id == "TOMST4_QHI" ~ "ARUQ6"
  )) %>% 
  mutate(date = as.Date(paste("2024", .$month, .$day, sep = "-"), format = "%Y-%m-%d")) %>%
  filter(sensor_name == "TMS_T3_mean") %>% 
  filter(week > 20) %>%
  filter(week < 30)

ggplot(aru_tomst_daily, aes(x = date, y = mean_value, color = aru)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("ARUQ3" = "#B8DE29FF", "ARUQ6" = "#440154FF", "ARUQ5" = "#FDE725FF")) +
  labs(x = "Date",
       y = "Daily Temperature",
       color = "Site") +
  theme_minimal() +
  theme_half_open(font_size = 14)


summarize_bird_calls <- function(data, species, sites, time_aggregation = c("hourly", "daily", "weekly", "3-day", "2-day"), add_main_line = TRUE, log_scale = FALSE, add_smooth = FALSE, smoothing_method = NULL) {
  # Match argument to avoid typos
  time_aggregation <- match.arg(time_aggregation)
  
  # Filter by species and sites
  filtered_data <- data %>%
    filter(common_name == species, name %in% sites)
  
  # Create a time-aggregated column based on user selection
  summarized_data <- filtered_data %>%
    mutate(detection_time = case_when(
      time_aggregation == "hourly" ~ floor_date(ymd_hms(detectionTimeLocal), "hour"),
      time_aggregation == "daily" ~ floor_date(ymd_hms(detectionTimeLocal), "day"),
      time_aggregation == "weekly" ~ floor_date(ymd_hms(detectionTimeLocal), "week"),
      time_aggregation == "3-day" ~ floor_date(ymd_hms(detectionTimeLocal), "3 days"),
      time_aggregation == "2-day" ~ floor_date(ymd_hms(detectionTimeLocal), "2 days")
    )) %>%
    group_by(detection_time, name) %>%
    summarise(call_count = n(), .groups = "drop")
  
  # Plot the data
  plot <- ggplot(summarized_data, aes(x = detection_time, y = call_count, color = name)) +
    geom_point(size = 0.25) +
    scale_color_manual(values = c("ARUQ3" = "#453781FF", "ARUQ5" = "#440154FF", "ARUQ6" = "#FDE725FF")) +
    labs(title = paste0("Bird Calls Detected Over Time by Site\n", species),
         x = "Date",
         y = "Number of Bird Calls",
         color = "Site") +
    theme_minimal() +
    theme_half_open(font_size = 14)
  
  
  # Add main line with optional smoothing method
  if (add_main_line) {
    if (!is.null(smoothing_method)) {
      plot <- plot + geom_smooth(method = smoothing_method, se = FALSE, size = 1)
    } else {
      plot <- plot + geom_line(size = 1)
    }
  }
  
  # Add log scale to y-axis if log_scale is TRUE
  if (log_scale) {
    plot <- plot + scale_y_log10()
  }
  
  # Add a smooth line if add_smooth is TRUE
  if (add_smooth) {
    plot <- plot + geom_smooth(se = FALSE)
  }
  
  return(list(summary = summarized_data, plot = plot))
}

summarize_bird_calls(
  data = three_arus_processed,
  species = "Lapland Longspur",
  sites = c("ARUQ3", "ARUQ5", "ARUQ6"),
  time_aggregation = "2-day",
  add_main_line = TRUE,
  log_scale = FALSE,
  add_smooth = FALSE
)


# Version with just 2 
###############
################################################################################
# Daily
aru_tomst_daily_filtered <- daily_values %>%
  filter(locality_id %in% c("TOMST29_QHI", "TOMST39_QHI", "TOMST4_QHI")) %>%
  mutate(aru = case_when(
    locality_id == "TOMST29_QHI" ~ "ARUQ3",
    locality_id == "TOMST39_QHI" ~ "ARUQ5",
    locality_id == "TOMST4_QHI" ~ "ARUQ6"
  )) %>% 
  mutate(date = as.Date(paste("2024", .$month, .$day, sep = "-"), format = "%Y-%m-%d")) %>%
  filter(sensor_name == "TMS_T3_mean") %>% 
  filter(week > 20) %>%
  filter(week < 30) %>%
  filter(aru != "ARUQ5")

ggplot(aru_tomst_daily_filtered, aes(x = date, y = mean_value, color = aru)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("ARUQ3" = "#FDE725FF", "ARUQ6" = "#440154FF")) +
  labs(x = "Date",
       y = "Daily Temperature",
       color = "Site") +
  theme_minimal() +
  theme_half_open(font_size = 14)


summarize_bird_calls <- function(data, species, sites, time_aggregation = c("hourly", "daily", "weekly", "3-day", "2-day"), add_main_line = TRUE, log_scale = FALSE, add_smooth = FALSE, smoothing_method = NULL) {
  # Match argument to avoid typos
  time_aggregation <- match.arg(time_aggregation)
  
  # Filter by species and sites
  filtered_data <- data %>%
    filter(common_name == species, name %in% sites)
  
  # Create a time-aggregated column based on user selection
  summarized_data <- filtered_data %>%
    mutate(detection_time = case_when(
      time_aggregation == "hourly" ~ floor_date(ymd_hms(detectionTimeLocal), "hour"),
      time_aggregation == "daily" ~ floor_date(ymd_hms(detectionTimeLocal), "day"),
      time_aggregation == "weekly" ~ floor_date(ymd_hms(detectionTimeLocal), "week"),
      time_aggregation == "3-day" ~ floor_date(ymd_hms(detectionTimeLocal), "3 days"),
      time_aggregation == "2-day" ~ floor_date(ymd_hms(detectionTimeLocal), "2 days")
    )) %>%
    group_by(detection_time, name) %>%
    summarise(call_count = n(), .groups = "drop")
  
  # Plot the data
  plot <- ggplot(summarized_data, aes(x = detection_time, y = call_count, color = name)) +
    geom_point(size = 0.25) +
    scale_color_manual(values = c("ARUQ3" = "#FDE725FF", "ARUQ6" = "#440154FF")) +
    labs(title = paste0("Bird Calls Detected Over Time by Site\n", species),
         x = "Date",
         y = "Number of Bird Detections",
         color = "Site") +
    theme_minimal() +
    theme_half_open(font_size = 14)
  
  
  # Add main line with optional smoothing method
  if (add_main_line) {
    if (!is.null(smoothing_method)) {
      plot <- plot + geom_smooth(method = smoothing_method, se = FALSE, size = 1)
    } else {
      plot <- plot + geom_line(size = 1)
    }
  }
  
  # Add log scale to y-axis if log_scale is TRUE
  if (log_scale) {
    plot <- plot + scale_y_log10()
  }
  
  # Add a smooth line if add_smooth is TRUE
  if (add_smooth) {
    plot <- plot + geom_smooth(se = FALSE)
  }
  
  return(list(summary = summarized_data, plot = plot))
}

summarize_bird_calls(
  data = three_arus_processed,
  species = "Lapland Longspur",
  sites = c("ARUQ3", "ARUQ6"),
  time_aggregation = "2-day",
  add_main_line = TRUE,
  log_scale = FALSE,
  add_smooth = FALSE
)

#####################
data <- data.frame(
  time = 1:100,
  value = 0.5 * 1:100 + 40  # Perfectly linear increase
)


# Create the plot
ggplot(data, aes(x = time, y = value)) +
  geom_line(aes(color = time), size = 1.5) +  # Line thickness
  scale_color_viridis(option = "viridis", begin = 1, end = 0, direction = -1) +  # Purple to yellow gradient
  theme_minimal() +
  labs(
    title = "Increase Over Time",
    x = "Time",
    y = "Value"
  ) +
  scale_y_continuous(limits = c(0, 120)) +
  theme_half_open(font_size = 14) +
  theme(
    axis.title = element_blank(),           # Remove axis titles
    axis.text = element_blank(),            # Remove axis text
    axis.ticks = element_blank()            # Remove axis ticks
  )

#######################
# create_birdnet_heatmap <- function(data, common.name, locationID, conf.threshold,
#                                    tz.local, latitude, longitude, sun.lines, sun.linetypes) {
# 
#   # Convert data to data.table if it's not already
#   if (!is.data.table(data)) {
#     setDT(data)
#   }
# 
#   # Ensure timestamps are in POSIXct format
#   parsed_times <- ymd_hms(data$detectionTimeLocal, quiet = TRUE)  # Convert to POSIXct quietly
# 
#   # Remove rows with NA timestamps
#   data <- data[!is.na(timestamp)]
# 
#   # Extract unique dates sampled
#   dates.sampled <- unique(as.Date(data$timestamp))
# 
#   # Create a list of hours sampled, filtering for NA hours
#   hours.sampled <- list(
#     sunrise = data[hour >= 6 & hour < 12, hour],  # Define your sunrise hours
#     sunset = data[hour >= 18 & hour < 24, hour]   # Define your sunset hours
#   )
# 
#   # Call the birdnet_heatmap_time function with the prepared arguments
#   birdnet_heatmap_time(
#     data = data,
#     common.name = common.name,
#     locationID = locationID,
#     conf.threshold = conf.threshold,
#     dates.sampled = dates.sampled,
#     hours.sampled = hours.sampled,
#     y.axis.limits = c(0, 23),
#     julian.breaks = c(30, 60, 90, 120, 150, 180, 210, 240, 270),
#     minute.timestep = 5,
#     comparable.color.breaks = FALSE,
#     tz.local = tz.local,
#     latitude = latitude,
#     longitude = longitude,
#     sun.lines = sun.lines,
#     sun.linetypes = sun.linetypes
#   )
# }
# 
# create_birdnet_heatmap(data = three_arus_processed, common.name = 'Snow Bunting',
#                        locationID = 'upland', conf.threshold = 0.5,
#                        tz.local = 'America/Whitehorse',
#                        latitude = qhi_latitude, longitude = qhi_longitude,
#                        sun.lines = c('dusk', 'dawn', 'sunrise', 'sunset'),
#                        sun.linetypes = c('dotdash', 'longdash', 'dotted', 'solid'))

  
###############
# Load necessary packages if not already done
  library(data.table)
  library(NSNSDAcoustics)  # Assuming birdnet_heatmap_time is in this package
  # Ensure data is in data.table format if not already
  
  dat <- as.data.table(three_arus_processed)
  
  # Add recordingID from filepath if not present
  dat[, recordingID := basename(filepath)]
  
  # Add time columns (adds date and time columns based on local time)
  dat <- add_time_cols(dt = dat,
                       tz.recorder = 'America/Dawson',
                       tz.local = 'America/Dawson')
  
  # # Filter data for "Lapland Longspur" at the location "upland"
  # dat_filtered <- dat[common_name == "Lapland Longspur" &
  #                       locationID == "ARUQ3"]
  dat_filtered <- dat
  
  # Extract unique sampled dates
  dates.sampled <- as_date(unique(dat_filtered[, date]))
  
  # Extract unique sampled hours
  dat_filtered[, hour := as.numeric(format(as.POSIXct(detectionTimeLocal, tz = 'America/Dawson'), "%H"))]
  hours.sampled <- sort(unique(dat_filtered[, hour]))
  
  # Define the specific minutes that were typically sampled (0, 10, 30, 40)
  minutes.sampled <- c(0, 10, 30, 40)
  
  # Generate the heatmap
  birdnet_heatmap_time(
    data = dat_filtered,
    common.name = 'Lapland Longspur',
    locationID = 'ARUQ3',
    conf.threshold = 0.25,
    dates.sampled = dates.sampled,
    hours.sampled = hours.sampled,
    #y.axis.limits = c(min(hours.sampled), max(hours.sampled)),
    minute.timestep = 60,           # Match the intervals with your typical sampling periods
    comparable.color.breaks = FALSE,
    tz.local = 'America/Dawson',
    latitude = 69.5,
    longitude = -139,
    sun.lines = c('sunrise', 'sunset'),
    sun.linetypes = c('longdash', 'solid')
  )
  
  ?birdnet_heatmap_time
  
  
  ############################
  # Define the function
generate_birdnet_heatmap <- function(data, species, location, conf_threshold = 0.25, 
                                       tz_recorder = 'America/Dawson', tz_local = 'America/Dawson',
                                       latitude = 69.5, longitude = -139, minute_timestep = 60) {
    
    # Convert data to data.table format if not already
    data <- as.data.table(data)
    
    # Add recordingID from filepath if not present
    data[, recordingID := basename(filepath)]
    
    # Add date and time columns based on local time
    data <- add_time_cols(dt = data, tz.recorder = tz_recorder, tz.local = tz_local)
    
    # Extract unique sampled dates and hours
    dates_sampled <- as_date(unique(data[, date]))
    data[, hour := as.numeric(format(as.POSIXct(detectionTimeLocal, tz = tz_local), "%H"))]
    hours_sampled <- sort(unique(data[, hour]))
    
    # Generate the heatmap
    birdnet_heatmap_time(
      data = data,
      common.name = species,
      locationID = location,
      conf.threshold = conf_threshold,
      dates.sampled = dates_sampled,
      hours.sampled = hours_sampled,
      y.axis.limits = c(min(hours_sampled), max(hours_sampled)),
      minute.timestep = minute_timestep,
      comparable.color.breaks = FALSE,
      tz.local = tz_local,
      latitude = latitude,
      longitude = longitude,
      sun.lines = c('sunrise', 'sunset'),
      sun.linetypes = c('longdash', 'solid')
    )
  }
  
# Example usage:
  generate_birdnet_heatmap(
    data = three_arus_processed,
    species = 'Lapland Longspur',
    location = 'ARUQ3'
  )
  generate_birdnet_heatmap(
    data = three_arus_processed,
    species = 'Lapland Longspur',
    location = 'ARUQ6'
  )
  
  
##########################
