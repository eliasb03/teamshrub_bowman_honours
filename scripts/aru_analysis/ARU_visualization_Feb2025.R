# ARU visualization script
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(cowplot)


ggplot(aru_analysis_dataframe, aes(x = time_interval, y = locationID)) +
  geom_point(size = 1) +   # Plot points for each observation
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Angle x-axis labels
  labs(title = "Observations over Time by Location", x = "Date/Time", y = "Location")