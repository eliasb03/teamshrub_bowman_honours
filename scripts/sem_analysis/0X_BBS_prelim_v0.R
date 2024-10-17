#------------------------------
# teamshrub_bowman_honours
# 0X_BBS_prelim_v0
# By: Elias Bowman 
# Created: 2024-09-24
# 
# Description: This script will import and tidy the Ranger collected Breeding Bird Data from Qikiqtaruk - Herschel Island Territorial Park 
#
#------------------------------


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