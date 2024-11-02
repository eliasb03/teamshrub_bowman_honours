#------------------------------
# teamshrub_bowman_honours
# 0X_BBS_prelim_v0
# By: Elias Bowman 
# Created: 2024-09-24
# 
# Description: This script will import and tidy the Ranger collected Breeding Bird Data from Qikiqtaruk - Herschel Island Territorial Park 
#
#------------------------------


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