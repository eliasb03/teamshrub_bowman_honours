#------------------------------
# teamshrub_bowman_honours
# 08_climate_trends_v1.R
# By: Elias Bowman 
# Created: 2025-04-07
# 
# Description: This script will create simple regressions and plots shwoing trends in climate and habitat variables over time.
#------------------------------

library(ggplot2)
library(tidyverse)
library(brms)
library(cowplot)

# Read in climate and habitat datasets
directory <- "data/clean/sem/"

ice.file <- "ice_data.csv"
phenology.file <- "phenology_data.csv"
climate.file <- "climate_data.csv"

ice_data <- read_csv(file.path(directory, ice.file))
phenology_data <- read_csv(file.path(directory, phenology.file))
climate_data <- read_csv(file.path(directory, climate.file))

# Defining Relevant Functions
# Function to join datasets by year
join_by_year <- function(ice_data, phenology_data, climate_data, 
                         ice_cols, phenology_cols, climate_cols) {
  
  # Select the specified columns from each dataset
  ice_selected <- ice_data %>% select(year, all_of(ice_cols))
  phenology_selected <- phenology_data %>% select(year, all_of(phenology_cols))
  climate_selected <- climate_data %>% select(year, all_of(climate_cols))
  
  # Perform a series of left joins to combine the datasets
  joined_data <- ice_selected %>%
    left_join(phenology_selected, by = "year") %>%
    left_join(climate_selected, by = "year")
  
  return(joined_data)
}


# Specify columns of interest
ice_cols <- c("spring_drop_doy")
phenology_cols <- c("snowmelt_mean", "budburst_mean")
climate_cols <- c("Tave_sp", "mean_temp_breeding_season")

# join_by_year with the environmental datasets
env_data <- join_by_year(
  ice_data = ice_data,
  phenology_data = phenology_data,
  climate_data = climate_data,
  ice_cols = ice_cols,
  phenology_cols = phenology_cols,
  climate_cols = climate_cols
) %>%
  mutate(year_since_2000 = year - 2000)

# Recode dictionary to map variable names to human-readable labels
recode_vars <- c(
  "intercept" = "Intercept",
  "year_since_2000" = "Year (since 2000)",
  "spring_drop_doy" = "Drop in Sea Ice Cover (Day of Year)",
  "snowmelt_mean" = "Snowmelt Date (Day of Year)",
  "budburst_mean" = "S. Arctica Budburst Date (Day of Year)",
  "mean_temp_breeding_season" = "Breeding Temperature (degrees C)",
  "Tave_sp" = "Regional Temperature (degrees C)"
)

# Function to generate the plot for simple models
generate_model_plot <- function(model, variable, color = "blue", data = env_data) {
  
  # Dynamically choose the appropriate variable for the y-axis
  y_variable <- enquo(variable)
  var_name <- quo_name(y_variable)
  
  # Filter data to get the first year with a non-NA value for the variable
  x_min <- data %>%
    filter(!is.na(!!y_variable)) %>%
    pull(year) %>%
    min()
  
  # Generate predictions
  model_preds <- fitted(
    model,
    newdata = tibble(year_since_2000 = seq(min(data$year_since_2000), max(data$year_since_2000))),
    probs = c(0.05, 0.95)
  ) %>%
    as_tibble() %>%
    mutate(year_since_2000 = seq(min(data$year_since_2000), max(data$year_since_2000))) %>%
    mutate(year = year_since_2000 + 2000)
  
  # Get the human-readable y-axis label
  y_label <- recode_vars[var_name]
  if (is.null(y_label)) y_label <- var_name
  
  # Create the plot
  plot <- ggplot(data, aes(x = year, y = !!y_variable)) +
    geom_line(size = 1) +
    geom_line(data = model_preds, aes(y = Estimate), color = color, size = 2) +
    geom_ribbon(data = model_preds, aes(y = Estimate, ymin = Q5, ymax = Q95), alpha = 0.2) +
    scale_x_continuous(breaks = seq(1968, 2025, 7), limits = c(x_min, NA)) +
    labs(x = "Year", y = y_label) +
    theme_half_open(font_size = 14)
  
  return(plot)
}

# Function to generate output table for simple models
generate_model_table <- function(model, table.title = "Model Summary") {
  tidy_output <- summary(model)$fixed %>%
    tibble::rownames_to_column(var = "Parameter") %>%
    rename(
      rhat = Rhat, bulk.ess = Bulk_ESS, tail.ess = Tail_ESS, 
      estimate = Estimate, est.error = Est.Error, l95.ci = `l-95% CI`, u95.ci = `u-95% CI`
    )
  
  # Recode variable names
  recode_vars <- c(
    "intercept" = "Intercept",
    "year_since_2000" = "Year (since 2000)",
    "spring_drop_doy" = "Drop in Sea Ice Cover (Day of Year)",
    "snowmelt_mean" = "Snowmelt Date (Day of Year)",
    "budburst_mean" = "S. Arctica Budburst Date (Day of Year)",
    "mean_temp_breeding_season" = "Breeding Temperature (degrees C)",
    "Tave_sp" = "Regional Temperature (degrees C)"
  )
  
  # Prepare the table
  output_tbl <- tidy_output %>%
    mutate(across(c(estimate, est.error, l95.ci, u95.ci), round, 3)) %>%
    mutate(across(c(bulk.ess, tail.ess), round, 0)) %>%
    mutate(across(c(rhat), round, 2)) %>%
    mutate(
      Parameter = dplyr::recode(Parameter, !!!recode_vars)
    ) %>%
    rename(
      "Parameter" = Parameter,
      "Estimate" = estimate,
      "Est.Error" = est.error,
      "Lower 95% CI" = l95.ci,
      "Upper 95% CI" = u95.ci,
      "R-hat" = rhat,
      "Bulk ESS" = bulk.ess,
      "Tail ESS" = tail.ess
    ) %>%
    arrange(Parameter)
  
  # Create the table with styling
  output_table <- output_tbl %>%
    gt() %>%
    tab_header(
      title = table.title
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
      locations = cells_body(rows = `Lower 95% CI` > 0 & `Upper 95% CI` > 0) # Confidence intervals above 0
    ) %>%
    tab_style(
      style = list(
        cell_text(style = "italic"),
        cell_fill(color = "gainsboro")
      ),
      locations = cells_body(rows = `Lower 95% CI` < 0 & `Upper 95% CI` < 0) # Confidence intervals below 0
    ) %>%
    cols_move_to_start("Parameter")
  
  return(output_table)
}

# Running the models for each variable

# Icemelt
icemelt_priors <- c(
  prior(normal(0, 20), class = "b"),         # prior for slope, change over time
  prior(normal(200, 75), class = "Intercept") # prior for intercept
)

icemelt_model <- brm(
  spring_drop_doy ~ year_since_2000,
  prior = icemelt_priors,
  data = env_data,
  chains = 4,
  iter = 2000
)

# Breeding Temperature
breeding_temp_priors <- c(
  prior(normal(0, 6), class = "b"),         # prior for slope, change over time
  prior(normal(-6, 5), class = "Intercept") # prior for intercept
)

breeding_temp_model <- brm(
  mean_temp_breeding_season ~ year_since_2000,
  prior = breeding_temp_priors,
  data = env_data,
  chains = 4,
  iter = 2000
)

# Snowmelt
snowmelt_priors <- c(
  prior(normal(0, 20), class = "b"),         # prior for slope, change over time
  prior(normal(150, 75), class = "Intercept") # prior for intercept
)

snowmelt_model <- brm(
  snowmelt_mean ~ year_since_2000,
  prior = snowmelt_priors,
  data = env_data,
  chains = 4,
  iter = 2000
)

# Budburst 
budburst_priors <- c(
  prior(normal(0, 20), class = "b"),         # prior for slope, change over time
  prior(normal(160, 75), class = "Intercept") # prior for intercept
)

budburst_model <- brm(
  budburst_mean ~ year_since_2000,
  prior = budburst_priors,
  data = env_data,
  chains = 4,
  iter = 2000
)


# Plotting Outputs

icemelt_plot <- generate_model_plot(icemelt_model, spring_drop_doy, color = "blue")
icemelt_table <- generate_model_table(icemelt_model, table.title = "Day of Sea Ice Cover Drop Model Summary")

breeding_temp_plot <- generate_model_plot(breeding_temp_model, mean_temp_breeding_season, color = "blue")
breeding_temp_table <- generate_model_table(breeding_temp_model, table.title = "Breeding Temperature Model Summary")

snowmelt_plot <- generate_model_plot(snowmelt_model, snowmelt_mean, color = "blue")
snowmelt_table <- generate_model_table(snowmelt_model, table.title = "Day of Snowmelt Model Summary")

budburst_plot <- generate_model_plot(budburst_model, budburst_mean, color = "blue")
budburst_table <- generate_model_table(budburst_model, table.title = "Day of Budburst Model Summary")


icemelt_plot
icemelt_table
summary(env_data$spring_drop_doy, na.rm = TRUE)

breeding_temp_plot
breeding_temp_table
summary(env_data$mean_temp_breeding_season, na.rm = TRUE)

snowmelt_plot
snowmelt_table
summary(env_data$snowmelt_mean, na.rm = TRUE)

budburst_table
budburst_plot
summary(env_data$budburst_mean, na.rm = TRUE)



####

library(sf)              # Load the package

geo_dir <- "data/clean/shape"

# Read in shapefiles
qhi_coast     <- st_read(file.path(geo_dir, "qhi_coast.shp"))[1, ]
north_coast   <- st_read(file.path(geo_dir, "north_coast.shp"))

# Function to clip polygon boundary with bounding box
clip_polygon_boundary <- function(polygon_sf, bbox_coords, crs_target = 32608) {
  bbox_poly <- st_polygon(list(bbox_coords)) %>%
    st_sfc(crs = st_crs(polygon_sf)) %>%
    st_sf()
  
  boundary_line <- st_boundary(polygon_sf)
  clipped_line <- st_intersection(boundary_line, bbox_poly)
  clipped_line_proj <- st_transform(clipped_line, crs = crs_target)
  sum(st_length(clipped_line_proj))
}

# Function to clip line with vertical bounds and get total length
clip_line_by_x_bounds <- function(line_sf, xmin, xmax, crs_target = 32608) {
  clip_box <- st_polygon(list(matrix(c(
    xmin,  68.0,
    xmax,  68.0,
    xmax,  70.0,
    xmin,  70.0,
    xmin,  68.0
  ), ncol = 2, byrow = TRUE))) %>%
    st_sfc(crs = st_crs(line_sf)) %>%
    st_sf()
  
  clipped_line <- st_intersection(line_sf, clip_box)
  clipped_proj <- st_transform(clipped_line, crs = crs_target)
  sum(st_length(clipped_proj))
}

# Define bounding box for QHI coastline
qhi_bbox_coords <- matrix(
  c(-138.953499, 69.579752,
    -138.888818, 69.579752,
    -138.888818, 69.565975,
    -138.953499, 69.565975,
    -138.953499, 69.579752),
  ncol = 2, byrow = TRUE
)

# Run clipped length calculations
qhi_length <- clip_polygon_boundary(qhi_coast, qhi_bbox_coords)
north_coast_length <- clip_line_by_x_bounds(north_coast, xmin = -141.0, xmax = -136.46)


full_qhi_boundary <- st_boundary(qhi_coast)
full_qhi_proj <- st_transform(full_qhi_boundary, crs = 32608)
full_qhi_length <- sum(st_length(full_qhi_proj))

# Add to north coast length
combined_northslope_length <- full_qhi_length + north_coast_length

# Ratio: how much smaller is the QHI clipped section?
length_ratio <- combined_northslope_length / qhi_length

qhi_length
combined_northslope_length
length_ratio # Yukon north coast - minus the rest QHI is 96.9 times larger
