#------------------------------
# teamshrub_bowman_honours
# 0X_BBS_prelim_v0
# By: Elias Bowman 
# Created: 2024-09-24
# 
# Description: This script will do some preliminary work and visualizations of the Ranger collected Breeding Bird Data from Qikiqtaruk - Herschel Island Territorial Park 
#
#------------------------------



bbs.summary.species <- select_species_list(bbs.summary, species_list)

# function to create frequency table from the bbs.survey data
create_species_freq_table <- function(data, species) {
  data %>%
    filter(species == species) %>%
    group_by(year, period) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(year, period)
}

bbs.summary.species %>%
  group_by(year, spec.code) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(year, spec.code)

# function to create a frequency distribution of the number of observations of each species
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
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = plot_title, x = "Item", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# plot the bbs.long data, x = year, y = sampling.effort
plot(bbs.long$year, bbs.long$sampling.effort, xlab = "Year", ylab = "Sampling Effort", main = "Sampling Effort by Year")

# plot the bbs.long data, x = day of year, y = sampling.effort, points are filled black
plot(bbs.long$year, bbs.long$doy, xlab = "Year", ylab = "Day of Year", main = "Day of Year by Year", col = "red")

# find list of years included in the bbs.survey dataset
list_of_years <- bbs.survey %>%
  distinct(year) %>%
  pull(year) %>%
  unique()

years_with_both <- bbs.survey %>%
  group_by(year) %>%
  filter(n_distinct(period) == 2) %>%
  distinct(year) %>%
  pull(year)

years_with_one <- setdiff(list_of_years, years_with_both)

length(years_with_both)
length(years_with_one)


##### SIMPLE TEMPORARY PLOTTING CODE TO VISUALIZE CHANGE BETWEEN EARLY AND LATE SURVEYS
species_data <- bbs.long %>%
  filter(species == "Common Eider") %>%
  group_by(year, period) %>%  # Group by year and period
  summarise(count = n(), .groups = 'drop') %>%  # Summarize to count observations
  arrange(year, period)  # Arrange for plotting

# Plot

ggplot(species_data, aes(x = period, y = count, group = year)) +
  geom_point(aes(color = as.factor(year)), size = 3) +  # Points for EARLY and LATE
  geom_line(aes(group = year, color = as.factor(year)), na.rm = TRUE) +  # Lines between EARLY and LATE
  theme_minimal() +
  labs(x = "Survey Period", 
       y = "Number of Birds Observed", 
       title = "Bird Observations for Species X in EARLY vs LATE Surveys",
       color = "Year") +
  theme(legend.position = "bottom")

# Create the line plot
ggplot(spec.year.sum, aes(x = year, y = total.observations, color = species)) +
  geom_line() +                                    # Plot lines for each species
  labs(title = paste0("Observations of Top ",top_num, " Species by Year"),
       x = "Year",
       y = "Total Observations",
       color = "Species") +                        # Add labels
  theme_minimal()                                  # Use a clean theme

# Create the scaled line plot
ggplot(spec.year.sum, aes(x = year, y = scaled.observations, color = species)) +
  geom_line() +                                     # Plot lines for each species
  labs(title = "Scaled Observations of Top 15 Species by Year",
       x = "Year",
       y = "Scaled Observations",
       color = "Species") +                         # Add labels
  theme_minimal()                                   # Use a clean theme


#######################
# Create the scaled line plot
june_temp <- read.csv("C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/qhi_june_temp_handmade.csv")

bbs.summary <- bbs.summary %>%
  filter(species %in% species_list)

ggplot(bbs.summary, aes(x = year, y = larger_observation_count, color = species)) +
  geom_line(size = 2) +                                     # Plot lines for each species
  labs(title = "Scaled Observations of Top 15 Species by Year",
       x = "Year",
       y = "Scaled Observations",
       color = "Species") +                         # Add labels
  theme_minimal() +
  theme_half_open(font_size = 14)


ggplot(filter(bbs.summary, species == "Common Eider"), aes(x = year, y = larger_observation_count, color = species)) +
  geom_line(size = 2) +                                     # Plot lines for each species
  labs(title = "Scaled Observations of Top 15 Species by Year",
       x = "Year",
       y = "Observation Count",
       color = "Species") +                         # Add labels
  theme_minimal() +
  theme_half_open(font_size = 14)

ggplot(filter(bbs.summary, species == "Lapland Longspur"), aes(x = year, y = larger_observation_count, color = species)) +
  geom_line(size = 2, colour = "darkgreen") +                                     # Plot lines for each species
  labs(title = "Scaled Observations of Top 15 Species by Year",
       x = "Year",
       y = "Observation Count",
       color = "Species") +                         # Add labels
  theme_minimal() +
  theme_half_open(font_size = 14)


ggplot(june_temp, aes(x = year, y = june_temp)) +
  geom_line(size = 2, colour = "orange") +                                     # Plot lines for each species
  geom_smooth(method = "lm", color = "yellow") +
  labs(x = "Year",
       y = "June Average Temperature") + 
  theme_half_open(font_size = 14)

summary(lm(june_temp ~ year, june_temp))
