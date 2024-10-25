# Load necessary library for date handling
library(lubridate)

# Define seasonal dates on the x-axis
spring_date <- ymd("2023-04-1")
fall_date <- ymd("2023-8-1")

center_date <- ymd("2023-06-01")

# Create a sequence of dates around the seasonal dates
x_dates <- seq(center_date - months(3), center_date + months(3), by = "day")

# Calculate normal distribution y-values for the first distribution
mean1 <- 0  # Mean for the first distribution
sd <- 1
y_values1 <- dnorm(seq(-3, 5, length.out = length(x_dates)), mean = mean1, sd = sd)

# Calculate the second normal distribution y-values (shifted to the right)
mean2 <- mean1 + 2  # Shift mean to the right

# Create a new sequence for the second distribution to show its tails
# The sequence will cover a range that allows the second distribution's tails to be visible
x_dates2_numeric <- seq(-3, 5, length.out = 100)  # Numeric range for tails
y_values2 <- dnorm(x_dates2_numeric, mean = mean2, sd = sd)

# Map numeric values back to date format
# Create a corresponding date sequence for the second distribution
# Adjust the scaling to fit into your date context
x_dates2 <- seq(from = min(x_dates), to = max(x_dates), length.out = length(x_dates2_numeric))

# Prepare y-values for max y-axis limit
y_max <- max(y_values1, y_values2) + 0.02

# Plot the first normal distribution with date labels suppressed on the x-axis
plot(x_dates, y_values1, type = "l", lwd = 2, col = "red",
     xlab = "Seasonal Dates",
     ylab = "Number of Detections",
     xaxt = "n",
     ylim = c(0, max(y_max)),
     xlim = c(min(x_dates), max(x_dates)))

# Overlay the second normal distribution in a different color
lines(x_dates2, y_values2, lwd = 2, col = "blue")

# Find the maximum point for the first distribution
peak1_x <- x_dates[which.max(y_values1)]
peak1_y <- max(y_values1)

# Find the maximum point for the second distribution
peak2_x <- x_dates2[which.max(y_values2)]
peak2_y <- max(y_values2)

# Add vertical dashed lines at the peak points
abline(v = peak1_x, col = "red", lty = "dashed")
abline(v = peak2_x, col = "blue", lty = "dashed")

# Add labels above the graph
mtext(c("Warm ARU", "Cold ARU"), side = 3, at = c(peak1_x, peak2_x), line = 0.5, 
      adj = 0.5, col = c("red", "blue"))


# Add labels below the graph
mtext(c("Early Summer", "Late Summer"), side = 1, at = c(spring_date, fall_date), line = 0.5, 
      adj = 0.5, col = "black")



