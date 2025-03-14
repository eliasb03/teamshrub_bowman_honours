# # Load necessary libraries
# library(ggplot2)
# library(lubridate)
# library(dplyr)
# library(tidyr)
# library(cowplot)
# 
# # Define key dates for seasonal markers
# spring_date <- ymd("2023-04-01")
# fall_date <- ymd("2023-08-01")
# center_date <- ymd("2023-06-01")
# 
# # Create a sequence of dates centered around June 1, 2023
# x_dates <- seq(center_date - months(3), center_date + months(3), by = "day")
# 
# # Prepare data frame with two normal distributions
# mean1 <- 0
# mean2 <- mean1 + 2
# sd <- 1
# data <- data.frame(
#   Date = x_dates,
#   Warm_ARU = dnorm(seq(-3, 5, length.out = length(x_dates)), mean = mean1, sd = sd),
#   Cold_ARU = dnorm(seq(-3, 5, length.out = length(x_dates)), mean = mean2, sd = sd)
# )
# 
# # Pivot data for ggplot2 compatibility
# data_long <- data %>%
#   pivot_longer(cols = c("Warm_ARU", "Cold_ARU"),
#                names_to = "Distribution",
#                values_to = "y_values")
# 
# # Identify peak points for labeling
# peaks <- data_long %>%
#   group_by(Distribution) %>%
#   filter(y_values == max(y_values))
# 
# # Maximum y-axis value for plot
# y_max <- max(data_long$y_values) + 0.02
# 
# # Create main plot with ggplot2
# # p1 <- ggplot(data_long, aes(x = Date, y = y_values, color = Distribution)) +
# #   geom_line(size = 1.2) +
# #   geom_vline(data = peaks, aes(xintercept = Date, color = Distribution), linetype = "dashed") +
# #   geom_text(data = peaks, aes(x = Date, y = y_values + 0.02, label = Distribution),
# #             color = c("red", "blue"), vjust = -0.5) +
# #   geom_vline(xintercept = as.numeric(spring_date), color = "black", linetype = "dashed") +
# #   geom_vline(xintercept = as.numeric(fall_date), color = "black", linetype = "dashed") +
# #   annotate("text", x = spring_date, y = 0, label = "Early Summer", vjust = 1.5, color = "black") +
# #   annotate("text", x = fall_date, y = 0, label = "Late Summer", vjust = 1.5, color = "black") +
# #   labs(x = "Seasonal Dates", y = "Number of Detections") +
# #   scale_color_manual(values = c("red", "blue")) +
# #   theme_cowplot(3) +                    # Apply cowplot's theme
# #   theme(legend.position = "none")
# 
# # Arrange plots with cowplot, adding a title if needed
# final_plot <- plot_grid(p1, labels = "A", label_size = 14)  # Adds a panel label "A" for reference
# 
# # Display final plot
# print(final_plot)
# 
# 

# 
# # Load necessary libraries
# library(ggplot2)
# library(lubridate)
# library(dplyr)
# library(tidyr)
# 
# # Define key dates for seasonal markers
# spring_date <- ymd("2023-04-01")
# fall_date <- ymd("2023-08-01")
# center_date <- ymd("2023-06-01")
# 
# # Create a sequence of dates around the center date
# x_dates <- seq(center_date - months(3), center_date + months(3), by = "day")
# 
# # Calculate normal distribution y-values for both distributions
# mean1 <- 0
# mean2 <- mean1 + 2
# sd <- 1
# 
# # Prepare data frame with two distributions
# data <- data.frame(
#   Date = x_dates,
#   Distribution1 = dnorm(seq(-3, 5, length.out = length(x_dates)), mean = mean1, sd = sd),
#   Distribution2 = dnorm(seq(-3, 5, length.out = length(x_dates)), mean = mean2, sd = sd)
# )
# 
# # Pivot the data for `ggplot2` compatibility
# data_long <- data %>%
#   pivot_longer(cols = starts_with("Distribution"),
#                names_to = "Distribution",
#                values_to = "y_values") %>%
#   mutate(Distribution = recode(Distribution, "Distribution1" = "Warm ARU", "Distribution2" = "Cold ARU"))
# 
# # Identify peak points for labeling
# peaks <- data_long %>%
#   group_by(Distribution) %>%
#   filter(y_values == max(y_values))
# 
# # Maximum y-axis value for base plot
# y_max <- max(data$Distribution1, data$Distribution2) + 0.02

## Plotting Section ##

# Uncomment this section to use ggplot2
# ggplot(data_long, aes(x = Date, y = y_values, color = Distribution)) +
#   geom_line(size = 1.2) +
#   geom_vline(data = peaks, aes(xintercept = Date, color = Distribution), linetype = "dashed") +
#   geom_text(data = peaks, aes(x = Date, y = y_values + 0.02, label = Distribution),
#             color = c("red", "blue"), vjust = -0.5) +
#   geom_vline(xintercept = as.numeric(spring_date), color = "black", linetype = "dashed") +
#   geom_vline(xintercept = as.numeric(fall_date), color = "black", linetype = "dashed") +
#   annotate("text", x = spring_date, y = 0, label = "Early Summer", vjust = 1.5, color = "black") +
#   annotate("text", x = fall_date, y = 0, label = "Late Summer", vjust = 1.5, color = "black") +
#   labs(x = "Seasonal Dates", y = "Number of Detections") +
#   scale_color_manual(values = c("blue", "red")) +
#   theme_minimal() +
#   theme(legend.position = "none")
# 
# # Uncomment this section to use base plot
# plot(x_dates, data$Distribution1, type = "l", lwd = 2, col = "red",
#      xlab = "Seasonal Dates",
#      ylab = "Number of Detections",
#      xaxt = "n",
#      ylim = c(0, y_max),
#      xlim = c(min(x_dates), max(x_dates)))
# 
# lines(x_dates, data$Distribution2, lwd = 2, col = "blue")
# 
# # Peak points for base plot
# peak1_x <- x_dates[which.max(data$Distribution1)]
# peak1_y <- max(data$Distribution1)
# peak2_x <- x_dates[which.max(data$Distribution2)]
# peak2_y <- max(data$Distribution2)
# 
# # Add vertical dashed lines and labels for peaks
# abline(v = peak1_x, col = "red", lty = "dashed")
# abline(v = peak2_x, col = "blue", lty = "dashed")
# # mtext(c("Warm ARU", "Cold ARU"), side = 3, at = c(peak1_x, peak2_x), line = 0.5, adj = 0.5, col = c("red", "blue"))
# mtext(c("Early Snowmelt \nARU", "Late Snowmelt\nARU"), side = 3, at = c(peak1_x, peak2_x), line = 0.5, adj = 0.5, col = c("red", "blue"))
# 
# # Add seasonal labels
# mtext(c("Early Summer", "Late Summer"), side = 1, at = c(spring_date, fall_date), line = 0.5,
#       adj = 0.5, col = "black")



# # Load necessary libraries
# library(ggplot2)
# library(lubridate)
# library(dplyr)
#
# # Define key dates for seasonal markers and distribution centers
# spring_date <- ymd("2023-04-01")
# fall_date <- ymd("2023-08-01")
# center_date <- ymd("2023-06-01")
#
# # Create a sequence of dates around the seasonal dates
# x_dates <- seq(center_date - months(3), center_date + months(3), by = "day")
#
# # Calculate normal distribution y-values for both distributions
# mean1 <- 0
# mean2 <- mean1 + 2
# sd <- 1
#
# # Prepare data frame with two distributions
# data <- data.frame(
#   Date = x_dates,
#   Distribution1 = dnorm(seq(-3, 5, length.out = length(x_dates)), mean = mean1, sd = sd),
#   Distribution2 = dnorm(seq(-3, 5, length.out = length(x_dates)), mean = mean2, sd = sd)
# )
#
# # Pivot the data for `ggplot2` compatibility
# data_long <- data %>%
#   pivot_longer(cols = starts_with("Distribution"),
#                names_to = "Distribution",
#                values_to = "y_values") %>%
#   mutate(Distribution = recode(Distribution, "Distribution1" = "Warm ARU", "Distribution2" = "Cold ARU"))
#
# # Find peak points
# peaks <- data_long %>%
#   group_by(Distribution) %>%
#   filter(y_values == max(y_values))
#
# # Plot
# ggplot(data_long, aes(x = Date, y = y_values, color = Distribution)) +
#   geom_line(size = 1.2) +
#
#   # Add dashed vertical lines at peak points
#   geom_vline(data = peaks, aes(xintercept = Date, color = Distribution), linetype = "dashed") +
#
#   # Add labels above the peaks
#   geom_text(data = peaks, aes(x = Date, y = y_values + 0.02, label = Distribution),
#             color = c("red", "blue"), vjust = -0.5) +
#
#   # Add vertical dashed lines for seasonal markers
#   geom_vline(xintercept = as.numeric(spring_date), color = "black", linetype = "dashed") +
#   geom_vline(xintercept = as.numeric(fall_date), color = "black", linetype = "dashed") +
#
#   # Add labels below the seasonal markers
#   annotate("text", x = spring_date, y = 0, label = "Early Summer", vjust = 1.5, color = "black") +
#   annotate("text", x = fall_date, y = 0, label = "Late Summer", vjust = 1.5, color = "black") +
#
#   # Customize labels and themes
#   labs(x = "Seasonal Dates", y = "Number of Detections") +
#   scale_color_manual(values = c("blue", "red")) +
#   theme_minimal() +
#   theme(legend.position = "none")
#
#
# # Load necessary library for date handling
# library(lubridate)
#
# # Define seasonal dates on the x-axis
# spring_date <- ymd("2023-04-1")
# fall_date <- ymd("2023-8-1")
#
# center_date <- ymd("2023-06-01")
#
# # Create a sequence of dates around the seasonal dates
# x_dates <- seq(center_date - months(3), center_date + months(3), by = "day")
#
# # Calculate normal distribution y-values for the first distribution
# mean1 <- 0  # Mean for the first distribution
# sd <- 1
# y_values1 <- dnorm(seq(-3, 5, length.out = length(x_dates)), mean = mean1, sd = sd)
#
# # Calculate the second normal distribution y-values (shifted to the right)
# mean2 <- mean1 + 2  # Shift mean to the right
#
# # Create a new sequence for the second distribution to show its tails
# # The sequence will cover a range that allows the second distribution's tails to be visible
# x_dates2_numeric <- seq(-3, 5, length.out = 100)  # Numeric range for tails
# y_values2 <- dnorm(x_dates2_numeric, mean = mean2, sd = sd)
#
# # Map numeric values back to date format
# # Create a corresponding date sequence for the second distribution
# # Adjust the scaling to fit into your date context
# x_dates2 <- seq(from = min(x_dates), to = max(x_dates), length.out = length(x_dates2_numeric))
#
# # Prepare y-values for max y-axis limit
# y_max <- max(y_values1, y_values2) + 0.02
#
# # Plot the first normal distribution with date labels suppressed on the x-axis
# plot(x_dates, y_values1, type = "l", lwd = 2, col = "red",
#      xlab = "Seasonal Dates",
#      ylab = "Number of Detections",
#      xaxt = "n",
#      ylim = c(0, max(y_max)),
#      xlim = c(min(x_dates), max(x_dates)))
#
# # Overlay the second normal distribution in a different color
# lines(x_dates2, y_values2, lwd = 2, col = "blue")
#
# # Find the maximum point for the first distribution
# peak1_x <- x_dates[which.max(y_values1)]
# peak1_y <- max(y_values1)
#
# # Find the maximum point for the second distribution
# peak2_x <- x_dates2[which.max(y_values2)]
# peak2_y <- max(y_values2)
#
# # Add vertical dashed lines at the peak points
# abline(v = peak1_x, col = "red", lty = "dashed")
# abline(v = peak2_x, col = "blue", lty = "dashed")
#
# # Add labels above the graph
# mtext(c("Warm ARU", "Cold ARU"), side = 3, at = c(peak1_x, peak2_x), line = 0.5,
#       adj = 0.5, col = c("red", "blue"))
#
#
# # Add labels below the graph
# mtext(c("Early Summer", "Late Summer"), side = 1, at = c(spring_date, fall_date), line = 0.5,
#       adj = 0.5, col = "black")
#
#


# Set seed for reproducibility
set.seed(42)

# Generate a dummy dataset
# Curve 1: Left skewed distribution
x1 <- 8 - rbeta(50000, 5, 4) * 10  # Shift and scale for left-skew

# Curve 2: Right skewed distribution
x2 <- rbeta(50000, 5, 4) * 10 + 5  # Shift and scale for right-skew


data <- data.frame(
  x_val = c(x1, x2),
  group = factor(c(rep("Warm ARU", length(x1)), rep("Cool ARU", length(x2))),
                 levels = c("Warm ARU", "Cool ARU"))  # Set the desired order
)

# Plot the bell curves with customizations
ggplot(data, aes(x = x_val, color = group)) +
  geom_density(size = 2, adjust = 1.5) + 
  labs(
    x = "Date",
    y = "# of Call Detections"
  ) +
  
  # Remove legend title by setting name to NULL
  scale_color_manual(values = c("#FDE725FF", "#440154FF"), name = NULL) +
  
  theme_half_open(font_size = 14) +
  
  scale_x_continuous(
    breaks = c((min(data$x_val) + 4), (max(data$x_val) - 3.5)), 
    labels = c("Early Season", "Late Season")
  ) +
 
  # # Add custom annotations at x-axis extremes
  # annotate("text", x = (min(data$x_val) + 3.5), y = 0.1, label = "Warm\n ARU", hjust = 0, color = "red") +
  # annotate("text", x = (max(data$x_val) - 4), y = 0.1, label = "Cold\nARU", hjust = 0, color = "blue") +
  
  
  # Remove numeric values on the y-axis
  theme(
    axis.text.y = element_blank(),
    
    # Adjust axis title position and margins
    axis.title.x = element_text(vjust = 2),
    axis.title.y = element_text(vjust = 2),
    
    # Position legend at the top and center-align it
    legend.position = "top",
    legend.justification = "center",
    legend.margin = margin(t = 0, b = -2)  # Adjust spacing as needed
  )
