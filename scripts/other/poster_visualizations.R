# Poster Visualizations
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(cowplot)
library(terra)
library(sf)
library(tmap)
library(ggmosaic)

#----------------------
# Variable Background ####
#----------------------

pbackground_color <- "white"
pline_color <- "black"
long_breaks <- 20
short_breaks <- 10
alpha_val <- .2

# Ice Trends ####
ice_data <- ice_data_yearly
ice_plot <- 
  ggplot(ice_data, aes(x = year, y = spring_drop_doy)) +
  #geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = lm, color = pline_color, alpha = alpha_val) +
  labs(
    x = "Year",
    y = "Day of Spring Drop in\nSea Ice Cover"
  ) +
  scale_x_continuous(limits = c(1975, 2024),
                     breaks=seq(1980, 2020, by = long_breaks)) +
  theme_half_open(font_size = 14) +
  theme(plot.background = element_rect(fill=pbackground_color, color=NA))


# Temp Trends ####
temp_data <- read.csv("data/raw/qhi_june_temp_handmade.csv")
temp_plot <- 
  ggplot(temp_data, aes(x = year, y = june_temp)) +
  #geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = lm, color = pline_color, alpha = alpha_val) +
  labs(
    x = "Year",
    y = "Average June\nTemperature (°C)"
  ) +
  scale_x_continuous(limits = c(1975, 2024),
                     breaks=seq(1980, 2020, by = long_breaks)) +
  theme_half_open(font_size = 14) +
  theme(plot.background = element_rect(fill=pbackground_color, color=NA))

# Snow Trends ####
snow_data <- snowmelt_avg %>%
  rename(year = Year)
snow_plot <- 
  ggplot(snow_data, aes(x = year, y = P1_mean)) +
  #geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = lm, color = pline_color, alpha = alpha_val) +
  labs(
    x = "Year",
    y = "Day of Snowmelt"
  ) +
    scale_x_continuous(limits = c(min(snow_data$year), max(snow_data$year)),
                       breaks=seq(2000, 2020, by = short_breaks)) +
  theme_half_open(font_size = 14) +
  theme(plot.background = element_rect(fill=pbackground_color, color=NA))

# Phenology Trends ####
leaf_data <- SALARC_budburst_avg %>%
  rename(year = Year)
leaf_plot <- 
  ggplot(leaf_data, aes(x = year, y = P2_mean)) +
  #geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = lm, color = pline_color, alpha = alpha_val) +
  labs(
    x = "Year",
    y = "Day of Salix Arctica\nLeaf Out Date"
  ) +
    scale_x_continuous(limits = c(min(leaf_data$year), max(leaf_data$year)),
                       breaks=seq(2000, 2020,  by = short_breaks)) +
  theme_half_open(font_size = 14) +
  theme(plot.background = element_rect(fill=pbackground_color, color=NA))

### Joined Cowplot ####
plot_grid(ice_plot, temp_plot, snow_plot, leaf_plot, ncol = 4, labels = c("", "", "", ""))
#----------------------
# SEM 
#----------------------

# Bird Trends ####
ggplot(bbs.summary.f, aes(x = year, y = yearly.rel.abundance.scaled, color = guild)) +
  geom_smooth(method = lm, alpha = 0.3) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  scale_color_manual(
    values = c(
      "shorebird" = "#FF0000", # Teal
      "passerine" = "#d95f02", # Orange
      "waterbird" = "#7570b3"  # Purple
    )
  ) +
  theme_half_open(font_size = 14) +
  theme(plot.background = element_rect(fill=pbackground_color, color=NA))

# make insectivore and marine bird modified guilds
bbs.summary.f.mod <- bbs.summary.f %>%
  mutate(mod.guild = ifelse(guild %in% c("shorebird", "passerine"), "Insectivore", "Marine Bird")) %>% filter(species != "Glaucous Gull")

# Bird Trends ####
ggplot(bbs.summary.f.mod, aes(x = year, y = yearly.rel.abundance.scaled, color = mod.guild)) +
  geom_smooth(method = lm, alpha = 0.3) +
  labs(
    x = "Year",
    y = "Relative Abundance"
  ) +
  scale_color_manual(
    values = c(
      "Insectivore" = "#685850", #"#FF0939", # Teal
      "Marine Bird" = "#E0900A"#"#09BFFF" # Orange
    )
  ) +
  theme_half_open(font_size = 14) +
  theme(plot.background = element_rect(fill=pbackground_color, color=NA))

View(bbs.summary.f)

# Logistic Regression Plots ####
# Creating logistic dataset
# logistic.data <- bbs.summary.f %>% 
#   select(species, guild, logistic.abundance, year) %>%
#   left_join(select(ice_data, year, spring_drop_doy), by = "year") %>%
#   left_join(temp_data, by = "year") %>% 
#   left_join(select(snow_data, year, P1_mean), by = "year") %>%
#   left_join(select(leaf_data, year, P2_mean), by = "year") %>%
#   mutate(level = ifelse(logistic.abundance == 1, "High Abundance Year", "Low Abundance Year"),
#          guild2 = ifelse(guild %in% c("passerine","shorebird"), "Insectivore", "Marine Bird")) %>%
#   mutate(level = factor(level, levels = c("Low Abundance Year", "High Abundance Year")),
#          guild = factor(guild, levels = c("passerine", "shorebird", "waterbird")),
#          guild2 = factor(guild2, levels = c("Marine Bird", "Insectivore")))

logistic.data <- bbs.summary %>% 
  select(species, guild, abundance_level, logistic.abundance, yearly.rel.abundance.scaled, year) %>%
  left_join(select(ice_data, year, spring_drop_doy), by = "year") %>%
  left_join(temp_data, by = "year") %>% 
  left_join(select(snow_data, year, P1_mean), by = "year") %>%
  left_join(select(leaf_data, year, P2_mean), by = "year") %>%
  mutate(level = ifelse(logistic.abundance == 1, "High Abundance Year", "Low Abundance Year"),
    abundance_level = factor(abundance_level, levels = c("Absent", "Low", "High")),
    guild2 = ifelse(guild %in% c("passerine", "shorebird"), "Insectivore", "Marine Bird")
  ) %>%
  mutate(
    level = factor(level, levels = c("Low Abundance Year", "High Abundance Year")),
    guild = factor(guild, levels = c("passerine", "shorebird", "waterbird")),
    guild2 = factor(guild2, levels = c("Marine Bird", "Insectivore"))
  )  %>%  filter(species %in% species.list$species)


# logistic.data.unfiltered <- bbs.summary %>% 
#   select(species, guild, logistic.abundance, year) %>%
#   left_join(select(ice_data, year, spring_drop_doy), by = "year") %>%
#   left_join(temp_data, by = "year") %>% 
#   left_join(select(snow_data, year, P1_mean), by = "year") %>%
#   left_join(select(leaf_data, year, P2_mean), by = "year") %>%
#   mutate(level = ifelse(logistic.abundance == 1, "High Abundance Year", "Low Abundance Year"),
#          guild2 = ifelse(guild %in% c("passerine","shorebird"), "Insectivore", "Marine Bird")) %>%
#   mutate(level = factor(level, levels = c("Low Abundance Year", "High Abundance Year")),
#          guild = factor(guild, levels = c("passerine", "shorebird", "waterbird")),
#          guild2 = factor(guild2, levels = c("Insectivore", "Marine Bird")))


plotting.logistic.data <- logistic.data %>%
  filter(species != "Glaucous Gull")
  # filter(
  #   species %in% c(
  #     "Common Eider",
  #     #"Red-throated Loon",
  #     "American Wigeon",
  #     #"Mallard",
  #     "Brant",
  #     "Lapland Longspur",
  #     "Semipalmated Sandpiper",
  #     "Semipalmated Plover",
  #     "Savannah Sparrow"#,
  #     #"American Pipit",
  #     #"Red-winged Blackbird"
  #   )
  # )

# count the most common marine bird species in logistic.data.unfiltered
# logistic.data.unfiltered %>%
#   filter(guild == "shorebird") %>%
#   count(species) %>%
#   arrange(desc(n)) %>%
#   head(10) %>%
#   pull(species)

#group_colors <- c("High Abundance Year" = "#FF0939", "Low Abundance Year" = "#09BFFF")
group_colors <- c("High Abundance Year" = "#97D959", "Low Abundance Year" = "#9E7749")


box_plot_theme <- function(first = FALSE, last = FALSE) {
  list(
    scale_fill_manual(values = group_colors),
    #scale_color_manual(c("black", "black")),
    theme_half_open(font_size = 14), 
    theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.title = element_blank(),
          plot.background = element_rect(fill=pbackground_color, color=NA),
          plot.margin = unit(c(0, 0, 0, 0), "cm")
          )
  
    # if (first) {
    #   theme(
    #     axis.title.x = element_blank(),
    #     axis.title.y = element_blank(),
    #     legend.position = "none",
    #     axis.title = element_blank(),
    #     plot.background = element_rect(fill=pbackground_color, color=NA),
    #     plot.margin = unit(c(0, 0, 0, 0), "cm"))
    # }   else if (last) {
    #   theme(
    #     axis.title.x = element_blank(),
    #     axis.title.y = element_blank(),
    #     axis.title = element_blank(),
    #     axis.text.y = element_blank(),
    #     plot.background = element_rect(fill=pbackground_color, color=NA),
    #     plot.margin = unit(c(0, 0, 0, 0), "cm"),)
    # }
    # else if (!first) {
    #   theme(
    #     axis.title.x = element_blank(),
    #     axis.title.y = element_blank(),
    #     legend.position = "none",
    #     axis.title = element_blank(),
    #     axis.text.y = element_blank(),
    #     plot.background = element_rect(fill=pbackground_color, color=NA),
    #     plot.margin = unit(c(0, 0, 0, 0), "cm"))
    # }
  )
}


  
  
#snowmelt.box <- 
  ggplot(plotting.logistic.data, aes(y = guild2, x = P1_mean, fill = level)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(
    title = "Snowmelt Date",
    y = "Bird Guild",
    x = "Snowmelt Date"
  ) +
  box_plot_theme(last = TRUE)

#ice.box <- 
  ggplot(plotting.logistic.data, aes(y = guild2, x = spring_drop_doy, fill = level)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(
    title = "Spring Drop in\nSea Ice Cover",
    y = "Bird Guild",
    x = "Day of Year"
  ) +
  box_plot_theme(first = FALSE)

leaf.box <- 
  ggplot(plotting.logistic.data, aes(y = guild2, x = P2_mean, fill = level)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(
    title = "Salix Arctic\nLeafout Date",
    y = "Bird Guild",
    x = "Day of Year"
  ) +
  box_plot_theme(last = TRUE)

temp.box <- 
  ggplot(plotting.logistic.data, aes(y = guild2, x = june_temp, fill = level)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(
    title = "June Average Temperature",
    y = "Bird Guild",
    x = "Day of Year"
  ) +
  box_plot_theme()

snowmelt.box
ice.box


plot_grid(
  NULL, ice.box, snowmelt.box, NULL,
  ncol = 4)

# plot_grid(
#   ice.box, temp.box, snowmelt.box, leaf.box,# legend,
#   ncol = 2,
#   align = "hv",
#   rel_widths = c(10, 10, 10, 10))

#ice_histogram <- 
ggplot(plotting.logistic.data, aes(x = spring_drop_doy)) +
  geom_histogram(binwidth = 5, fill = "#1f77b4", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Spring Sea Ice Drop-Off (Day of Year)",
    x = "Day of Year (Sea Ice Drop-Off)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)

#snow_histogram <- 
ggplot(plotting.logistic.data, aes(x = P1_mean)) +
  geom_histogram(binwidth = 5, fill = "#ff7f0e", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Snowmelt Date (Day of Year)",
    x = "Day of Year (Snowmelt)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)


# Define thresholds
ice_threshold <- 143  # Example threshold for spring_drop_doy
snow_threshold <- 143 # Example threshold for P1_mean

# Categorize and count observations
categorized_data <- plotting.logistic.data %>%
  mutate(
    ice_level = ifelse(spring_drop_doy <= ice_threshold, "Low", "High"),
    snow_level = ifelse(P1_mean <= snow_threshold, "Low", "High")
  )

# Summarize counts for each category
summary_counts <- categorized_data %>%
  summarise(
    ice_low_count = sum(ice_level == "Low", na.rm = TRUE),
    ice_high_count = sum(ice_level == "High", na.rm = TRUE),
    snow_low_count = sum(snow_level == "Low", na.rm = TRUE),
    snow_high_count = sum(snow_level == "High", na.rm = TRUE)
  )

# View summary counts
summary_counts

mosaic_data <- plotting.logistic.data %>%
  mutate(
    ice_level = ifelse(spring_drop_doy <= ice_threshold, "Early", "Late"),
    snow_level = ifelse(P1_mean <= snow_threshold, "Early", "Late")
) %>%
  select(species, guild, abundance_level, level, ice_level, snow_level)# %>%
  #drop_na(level, ice_level) %>% # Ensure no missing data in the factors
  

# Create the mosaic plot
ice_mosaic_plot <- ggplot(data = ice_mosaic_data) +
  geom_mosaic(aes(weight = 1, x = product(abundance_level, ice_level), fill = abundance_level)) +
  labs(
    title = "Mosaic Plot of Bird Abundance vs. Ice Years",
    x = "Ice Years (Early vs. Late)",
    y = "Proportion",
    fill = "Abundance Level"
  ) +
  scale_fill_manual(values = c("High" = "#FF0939", "Low" = "#09BFFF", "Absent" = "yellow")) +
  theme_minimal(base_size = 14)

ice_mosaic_plot

#----------------------
# Acoustic ####
#----------------------
# ARU Calling Graphs ####
arus_prob <- arus_summarized %>%
  group_by(common_name, aru_name) %>%
  mutate(
    detection_prob = count / sum(count),
    count_winsorized = Winsorize(count, val = quantile(count, probs = c(0, 0.90))), #Cap at 5th and 90th percentiles
    detection_prob_win = count_winsorized / sum(count_winsorized)
  )

aru_prob_plotting <- arus_prob %>%
  filter(common_name == "Lapland Longspur") %>%
  #filter(guild == "passerine") %>%
  filter(aru_name %in% c("ARUQ5", "ARUQ6")) %>%
  mutate(date = as.Date(date))

group_colors <- c("ARUQ5" = "#FF0939", "ARUQ6" = "#09BFFF")

ggplot(aru_prob_plotting, aes(x = date, y = detection_prob_win, group = aru_name)) +
  #geom_line(aes(color = aru_name)) +
  geom_smooth(aes(color = aru_name), alpha = 0.3) +
  #geom_point(aes(color = aru_name)) +
  labs(
    x = "Date",
    y = "Detection Probability"
  ) + 
  scale_color_manual(values = group_colors) +
  scale_x_date(
    date_breaks = "1 months",  # Show labels every 2 months
    date_labels = "%b %Y"     # Display in "Month Year" format
  ) +
  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.title = element_blank(),
  plot.background = element_rect(fill=pbackground_color, color=NA)) +
  theme_half_open(font_size = 14)


# ARU Microclimates ####

View(aru.tomst)

microclimate.plotting <- aru.tomst %>%
  mutate(
    temp_hl_expand = ifelse(temp_hml == "high", "Warm Site",
                            ifelse(temp_hml == "low", "Cool Site",
                                   ifelse(mean_value > 9.8, "Warm Site",
                                          ifelse(mean_value < 9.7, "Cool Site", "Intermediate")))))
  #filter(temp_hl_expand != "NA")

group_colors <- c("Warm Site" = "#FF0939", "Cool Site" = "#09BFFF", "Intermediate" = "darkgrey")

box_plot_theme <- function() {
  list(
    scale_fill_manual(values = group_colors),
    #scale_color_manual(c("black", "black")),
    theme_half_open(font_size = 14),
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.title = element_blank(),
      plot.background = element_rect(fill=pbackground_color, color=NA)
    ))}

ggplot(microclimate.plotting, aes(x = temp_hl_expand, y = mean_value)) +
  geom_boxplot(aes(fill = temp_hl_expand)) +
  box_plot_theme() + 
  labs(
    x = "Classification",
    y = "Temperature (°C)",
    color = "ARU"
  ) +
  theme_half_open(font_size = 14)

#######
ggplot(aru.tomst, aes(x = temp_hml, y = mean_value)) +
  geom_boxplot() +
  geom_jitter() +
  labs(
    x = "Classification",
    y = "Temperature (°C)",
    color = "ARU"
  ) +
  theme_half_open(font_size = 14)

# ARU Maps ####
aru.tomst.tomerge <- aru.tomst %>%
  mutate(name = aru_name) %>%
  select("name", "locality_id", "temp_hl", "temp_hml")

aru_loc_bbox <- st_bbox(aru_locations)

aru_data <- aru_locations %>%
  inner_join(aru.tomst.tomerge, by = "name")


ggplot() +
  geom_sf(data = qhi_coast, color = "black", fill = "lightgrey") +
  geom_sf(data = aru_data, aes(color = temp_hl), size = 3) +
  coord_sf(xlim = c(aru_loc_bbox["xmin"]-0.005, aru_loc_bbox["xmax"]+0.01), 
           ylim = c(aru_loc_bbox["ymin"]-0.001, aru_loc_bbox["ymax"]+0.005)) +
  scale_color_manual(
    values = c("high" = "red", "low" = "blue"), # Define colors for each category
    name = "Temperature Class" # Optional legend title
  ) +
  #theme_half_open()
  theme_void()



################### OLD ##########

#### Old stufff #### 
#####
# aru_tomst_daily_filtered <- daily_values %>%
#   filter(locality_id %in% c("TOMST29_QHI", "TOMST39_QHI", "TOMST4_QHI")) %>%
#   mutate(aru = case_when(
#     locality_id == "TOMST29_QHI" ~ "ARUQ3",
#     locality_id == "TOMST39_QHI" ~ "ARUQ5",
#     locality_id == "TOMST4_QHI" ~ "ARUQ6"
#   )) %>% 
#   mutate(date = as.Date(paste("2024", .$month, .$day, sep = "-"), format = "%Y-%m-%d")) %>%
#   filter(sensor_name == "TMS_T3_mean") %>% 
#   filter(week > 20) %>%
#   filter(week < 30) %>%
#   filter(aru != "ARUQ5")
# 
# ggplot(aru_tomst_daily_filtered, aes(x = date, y = mean_value, color = aru)) +
#   geom_line(size = 1) +
#   scale_color_manual(values = c("ARUQ3" = "#FDE725FF", "ARUQ6" = "#440154FF")) +
#   labs(x = "Date",
#        y = "Daily Temperature",
#        color = "Site") +
#   theme_minimal() +
#   theme_half_open(font_size = 14)
# 
# summarize_bird_calls <- function(data, species, sites, time_aggregation = c("hourly", "daily", "weekly", "3-day", "2-day"), add_main_line = TRUE, log_scale = FALSE, add_smooth = FALSE, smoothing_method = NULL) {
#   # Match argument to avoid typos
#   time_aggregation <- match.arg(time_aggregation)
#   
#   # Filter by species and sites
#   filtered_data <- data %>%
#     filter(common_name == species, name %in% sites)
#   
#   # Create a time-aggregated column based on user selection
#   summarized_data <- filtered_data %>%
#     mutate(detection_time = case_when(
#       time_aggregation == "hourly" ~ floor_date(ymd_hms(detectionTimeLocal), "hour"),
#       time_aggregation == "daily" ~ floor_date(ymd_hms(detectionTimeLocal), "day"),
#       time_aggregation == "weekly" ~ floor_date(ymd_hms(detectionTimeLocal), "week"),
#       time_aggregation == "3-day" ~ floor_date(ymd_hms(detectionTimeLocal), "3 days"),
#       time_aggregation == "2-day" ~ floor_date(ymd_hms(detectionTimeLocal), "2 days")
#     )) %>%
#     group_by(detection_time, name) %>%
#     summarise(call_count = n(), .groups = "drop")
#   
#   # Plot the data
#   plot <- ggplot(summarized_data, aes(x = detection_time, y = call_count, color = name)) +
#     geom_point(size = 0.25) +
#     scale_color_manual(values = c("ARUQ3" = "#FDE725FF", "ARUQ6" = "#440154FF")) +
#     labs(title = paste0("Bird Calls Detected Over Time by Site\n", species),
#          x = "Date",
#          y = "Number of Bird Detections",
#          color = "Site") +
#     theme_minimal() +
#     theme_half_open(font_size = 14)
#   
#   
#   # Add main line with optional smoothing method
#   if (add_main_line) {
#     if (!is.null(smoothing_method)) {
#       plot <- plot + geom_smooth(method = smoothing_method, se = FALSE, size = 1)
#     } else {
#       plot <- plot + geom_line(size = 1)
#     }
#   }
#   
#   # Add log scale to y-axis if log_scale is TRUE
#   if (log_scale) {
#     plot <- plot + scale_y_log10()
#   }
#   
#   # Add a smooth line if add_smooth is TRUE
#   if (add_smooth) {
#     plot <- plot + geom_smooth(se = FALSE)
#   }
#   
#   return(list(summary = summarized_data, plot = plot))
# }
# 
# summarize_bird_calls(
#   data = three_arus_processed,
#   species = "Lapland Longspur",
#   sites = c("ARUQ3", "ARUQ6"),
#   time_aggregation = "2-day",
#   add_main_line = TRUE,
#   log_scale = FALSE,
#   add_smooth = FALSE
# )
# 
# summarize_guild_calls <- function(data, guild_name, sites, time_aggregation = c("hourly", "daily", "weekly", "3-day", "2-day"), add_main_line = TRUE, log_scale = FALSE, add_smooth = FALSE, smoothing_method = NULL) {
#   # Match argument to avoid typos
#   time_aggregation <- match.arg(time_aggregation)
#   
#   # Filter by species and sites
#   filtered_data <- data %>%
#     filter(guild == guild_name, name %in% sites)
#   
#   # Create a time-aggregated column based on user selection
#   summarized_data <- filtered_data %>%
#     mutate(detection_time = case_when(
#       time_aggregation == "hourly" ~ floor_date(ymd_hms(detectionTimeLocal), "hour"),
#       time_aggregation == "daily" ~ floor_date(ymd_hms(detectionTimeLocal), "day"),
#       time_aggregation == "weekly" ~ floor_date(ymd_hms(detectionTimeLocal), "week"),
#       time_aggregation == "3-day" ~ floor_date(ymd_hms(detectionTimeLocal), "3 days"),
#       time_aggregation == "2-day" ~ floor_date(ymd_hms(detectionTimeLocal), "2 days")
#     )) %>%
#     group_by(detection_time, name) %>%
#     summarise(call_count = n(), .groups = "drop")
#   
#   # Plot the data
#   plot <- ggplot(summarized_data, aes(x = detection_time, y = call_count, color = name)) +
#     geom_point(size = 0.25) +
#     #scale_color_manual(values = c("ARUQ3" = "#FDE725FF", "ARUQ6" = "#440154FF")) +
#     labs(title = paste0("Bird Calls Detected Over Time by Site\n", guild_name),
#          x = "Date",
#          y = "Number of Bird Detections",
#          color = "Site") +
#     theme_minimal() +
#     theme_half_open(font_size = 14)
#   
#   
#   # Add main line with optional smoothing method
#   if (add_main_line) {
#     if (!is.null(smoothing_method)) {
#       plot <- plot + geom_smooth(method = smoothing_method, se = FALSE, size = 1)
#     } else {
#       plot <- plot + geom_line(size = 1)
#     }
#   }
#   
#   # Add log scale to y-axis if log_scale is TRUE
#   if (log_scale) {
#     plot <- plot + scale_y_log10()
#   }
#   
#   # Add a smooth line if add_smooth is TRUE
#   if (add_smooth) {
#     plot <- plot + geom_smooth(se = FALSE)
#   }
#   
#   return(list(summary = summarized_data, plot = plot))
# }
# 
# summarize_guild_calls(
#   data = three_arus_processed,
#   guild = "shorebirds",
#   sites = c("ARUQ3", "ARUQ6"),
#   time_aggregation = "2-day",
#   add_main_line = TRUE,
#   log_scale = FALSE,
#   add_smooth = FALSE
# )
# 
# time_aggregation <- "2-day"
# 
# # guild_filtered_data <- filter(three_arus_processed, guild == "shorebird") %>%
# #   filter(name %in% c("ARUQ3", "ARUQ6"))
# 
# 
# guild_filtered_data <- three_arus_processed %>%
#   filter(name %in% c("ARUQ3", "ARUQ6"))
# 
# guild_filtered_data <- guild_filtered_data %>%
#   mutate(detection_time = case_when(
#     time_aggregation == "hourly" ~ floor_date(ymd_hms(detectionTimeLocal), "hour"),
#     time_aggregation == "daily" ~ floor_date(ymd_hms(detectionTimeLocal), "day"),
#     time_aggregation == "weekly" ~ floor_date(ymd_hms(detectionTimeLocal), "week"),
#     time_aggregation == "3-day" ~ floor_date(ymd_hms(detectionTimeLocal), "3 days"),
#     time_aggregation == "2-day" ~ floor_date(ymd_hms(detectionTimeLocal), "2 days")
#   )) %>%
#   group_by(detection_time, name, common_name, guild) %>%
#   summarise(call_count = n(), .groups = "drop")
# 
# guild_filtered_data <- filter(guild_filtered_data, guild == "birdofprey") %>%
#   filter(call_count < 1000)
# 
# ggplot(guild_filtered_data, aes(x = detection_time, y = call_count, color = name)) +
#   #geom_line(linewidth = 1) +
#   geom_point(size = 3, alpha = 0.5) +
#   geom_jitter() +
#   scale_color_manual(values = c("ARUQ3" = "#FDE725FF", "ARUQ6" = "#440154FF")) +
#   labs(x = "Date",
#        y = "Number of Bird Detections",
#        color = "Site") +
#   theme_minimal() +
#   theme_half_open(font_size = 14)

# 
# basemap_path <- "D:/ARU_mapping/planet_imagery/qhi_early_june_elias_07nov2024_psscene_visual/composite.TIF"
# basemap <- terra::rast(basemap_path)
# crs(basemap)
# # crs(qhi_coast)
# #basemap_df <- as.data.frame(basemap, xy = TRUE)
# # 69.649826, -138.806919
# # 69.505541, -139.353093
# #extent_area <- terra::ext(-139.353093, -138.806919, 69.505541, 69.649826) 
# 
# basemap_resampled <- terra::aggregate(basemap, fact = 30)  # Adjust 'fact' as needed (higher = coarser)
# basemap_df <- as.data.frame(basemap_resampled, xy = TRUE)
# 
# 
# # Convert the cropped raster to a data frame
# basemap_df <- as.data.frame(basemap_cropped, xy = TRUE)
# 
# ggplot() +
#   layer_spatial(basemap_df) +
#   theme_minimal()
# 
# ggplot() +
#   geom_raster(data = basemap_df, aes(x = x, y = y)) + 
#   theme_minimal()

# Making Boxplots ####
# species.log.data <- filter(logistic.data, species == "Lapland Longspur") 
# species.log.data <- filter(logistic.data, species == "Common Eider") 
# species.log.data <- filter(logistic.data, species == "Savannah Sparrow") 
# species.log.data <- filter(logistic.data, species == "Semipalmated Sandpiper") 
# 
# ggplot(species.log.data, aes(x = factor(logistic.abundance), y = spring_drop_doy)) +
#   geom_boxplot(fill = c("#1f77b4", "#ff7f0e"), alpha = 0.7) +
#   labs(
#     x = "Abundance",
#     y = "Spring Drop DOY"
#   ) +
#   theme_minimal(base_size = 15) +
#   scale_x_discrete(labels = c("Low Year", "High Year"))
# 
# ggplot(species.log.data, aes(x = factor(logistic.abundance), y = june_temp)) +
#   geom_boxplot(fill = c("#1f77b4", "#ff7f0e"), alpha = 0.7) +
#   labs(
#     x = "Abundance",
#     y = "June Average Temperature"
#   ) +
#   theme_minimal(base_size = 15) +
#   scale_x_discrete(labels = c("Low Year", "High Year"))
# 
# ggplot(species.log.data, aes(x = factor(logistic.abundance), y = P1_mean)) +
#   geom_boxplot(fill = c("#1f77b4", "#ff7f0e"), alpha = 0.7) +
#   labs(
#     x = "Abundance",
#     y = "Snowmelt Date"
#   ) +
#   theme_minimal(base_size = 15) +
#   scale_x_discrete(labels = c("Low Year", "High Year"))
# 
# ####
# 
# ggplot(logistic.data, aes(x = level, y = P1_mean)) +
#   #geom_point() +
#   geom_boxplot(aes(color = guild), alpha = 0.7) +
#   labs(
#     x = "Abundance",
#     y = "Snowmelt Date"
#   ) +
#   scale_x_discrete(labels = c("Low Year", "High Year")) +
#   theme_half_open(font_size = 15)
# 
# ggplot(logistic.data, aes(x = guild2, y = P1_mean)) +
#   #geom_point() +
#   geom_boxplot(aes(color = level), alpha = 0.7) +
#   labs(
#     x = "Abundance",
#     y = "Snowmelt Date"
#   ) +
#   #scale_x_discrete(labels = c("Low Year", "High Year")) +
#   theme_half_open(font_size = 15)
# 

#####
# generate_logistic_predictions <- function(data, predictor_col) {
#   # Ensure the predictor_col is valid
#   if (!predictor_col %in% names(data)) {
#     stop("The specified predictor column does not exist in the dataset.")
#   }
#   
#   # Fit the logistic regression model
#   formula <- as.formula(paste("logistic.abundance ~", predictor_col))
#   model <- glm(formula, data = data, family = binomial)
#   
#   # Generate prediction data frame
#   pred_data <- data.frame(
#     x = seq(min(data[[predictor_col]], na.rm = TRUE), 
#             max(data[[predictor_col]], na.rm = TRUE), 
#             length.out = 500)
#   )
#   names(pred_data) <- predictor_col  # Ensure predictor_col matches the model's formula
#   
#   # Predict probabilities
#   pred_data$y_pred <- predict(model, newdata = pred_data, type = "response")
#   
#   # Return results
#   return(list(original_data = data, prediction_data = pred_data))
# }
# 
# species.log.data <- filter(logistic.data, species == "Lapland Longspur") 
# species.log.data <- filter(logistic.data, species == "Common Eider") 
# species.log.data <- filter(logistic.data, species == "Savannah Sparrow") 

# ice_pred <- generate_logistic_predictions(species.log.data, "spring_drop_doy")
# 
# ggplot(ice_pred$original_data, aes(x = spring_drop_doy, y = logistic.abundance)) +
#   geom_point(color = "blue", size = 2, alpha = 0.7) +  # Scatter plot
#   geom_line(data = ice_pred$prediction_data, aes(x = spring_drop_doy, y = y_pred), 
#             color = "red", size = 1.2) +  # Logistic curve
#   labs(
#     x = "Spring Drop DOY",
#     y = "Predicted Probability"
#   ) +
#   theme_minimal(base_size = 14)

# 
# temp_pred <- generate_logistic_predictions(species.log.data, "june_temp")
# 
# ggplot(temp_pred$original_data, aes(x = june_temp, y = logistic.abundance)) +
#   geom_point(color = "blue", size = 2, alpha = 0.7) +  # Scatter plot
#   geom_line(data = temp_pred$prediction_data, aes(x = june_temp, y = y_pred), 
#             color = "red", size = 1.2) +  # Logistic curve
#   labs(
#     x = "June Average Tempeature",
#     y = "Predicted Probability"
#   ) +
#   theme_minimal(base_size = 14)
