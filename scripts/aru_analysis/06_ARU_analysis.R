


######### HAVENT DEALT WITH THE PROBLEM OF ARUS STOPS
######### Haven't chosen early and late time periods yet


library(brms)


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


aru_priors <- c(
  set_prior("normal(0, 4.6)", class = "b", coef = "day_from_start"),  # Prior for day_from_start
    # max is roughly 1000, over a 16 day inerval about (1000/16) = ~60, say 100 more or less calls a day, log(100) = 4.6
  set_prior("normal(0, 4.6)", class = "b", coef = "temp_binarylow"),  # Prior for temp_binarylow
  set_prior("normal(0, 4.6)", class = "b", coef = "day_from_start:temp_binarylow")  # Prior for temp interaction term
  #set_prior("normal(0, 5)", class = "Intercept")  # Prior for the intercept
)



passerine_model <- brm(
  total_count ~ day_from_start * temp_binary + (1|locationID) + (1|date),
  data = passerine,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

waterbird_model <- brm(
  total_count ~ day_from_start * temp_binary + (1|locationID) + (1|date),
  data = waterbird,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

shorebird_model <- brm(
  total_count ~ day_from_start * temp_binary + (1|locationID) + (1|date),
  data = shorebird,
  family = poisson(),
  prior = aru_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)
  
# pp_check(passerine_model, resp = "total_count", ndraws = 1000)
# summary(passerine_model)


model_output_path <- "outputs/aru/calling/"
saveRDS(passerine_model, file = paste0(model_output_path, "passerine.rds"))
saveRDS(waterbird_model, file = paste0(model_output_path, "waterbird.rds"))
saveRDS(shorebird_model, file = paste0(model_output_path, "shorebird.rds"))

passerine_model <- readRDS(file = paste0(model_output_path, "passerine.rds"))
waterbird_model <- readRDS(file = paste0(model_output_path, "waterbird.rds"))
shorebird_model <- readRDS(file = paste0(model_output_path, "shorebird.rds"))



library(brms)
library(tidyverse)
library(tidybayes)
library(ggplot2)
library(ggeffects)
library(glmmTMB)
library(cowplot)


# Generate predicted values for day_from_start by temp_binary
pass_pred <- ggpredict(passerine_model, terms = c("day_from_start[0:20]", "temp_binary")) %>%
  as.data.frame() %>%
  mutate(species = "Lapland Longspur")
shor_pred <- ggpredict(shorebird_model, terms = c("day_from_start[0:20]", "temp_binary")) %>%
  as.data.frame() %>%
  mutate(species = "Semipalmated Plover")
watr_pred <- ggpredict(waterbird_model, terms = c("day_from_start[0:20]", "temp_binary")) %>%
  as.data.frame() %>%
  mutate(species = "Common Eider")

all_pred <- bind_rows(
  pass_pred,
  shor_pred,
  watr_pred
  ) %>% mutate(Temperature = group) %>%
  mutate(Temperature = fct_recode(Temperature, 
                                  "Cool" = "low",
                                  "Warm" = "high"),
         Date = as.Date(x + start))

aru_plotting_data <- aru_summary %>%
  mutate(Temperature = temp_binary) %>%
  filter(complete.cases(.)) %>%
  mutate(Temperature = fct_recode(Temperature, 
                                  "Cool" = "low",
                                  "Warm" = "high"),
         Date = as.Date(date))
  
  
calling_temp_resp <- ggplot(data = all_pred, aes(x = Date, y = predicted, colour = group)) +
  geom_line(linewidth = 1) + 
  #geom_point(data = aru_plotting_data, aes(x = Date, y = total_count, colour = Temperature), size = 1, alpha = 0.1, position = "jitter") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.13, colour = NA) +  # Fill ribbons
  scale_colour_viridis_d(name = "Temperature",
                         #option = "viridis",  # Change palette here
                         labels = c("Warm", "Cold"),
                         guide = guide_legend(nrow = 1)) +  # Combined legend for colour
  scale_fill_viridis_d(name = "Temperature",
                       #option = "viridis",  # Change palette here
                       labels = c("Warm", "Cold"),
                       guide = guide_legend(nrow = 1)) +  # Combined legend for fill
  labs(x = NULL,#"Predictor Variable", 
       y = "Predicted Bird Call Detections") +
  theme_half_open(font_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 10, face = "bold"),
    panel.spacing = unit(0.02, "npc"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    legend.position = "bottom"
  ) +
  facet_wrap(~species, nrow = 1, scale = "free",
             strip.position = "top") 

calling_temp_resp
ggsave("outputs/aru/model_figs/calling_temp_model.pdf", plot = calling_temp_resp, width = 10, height = 4, units = "in")

calling_temp_points <- ggplot(data = aru_plotting_data, aes(x = Date, y = total_count, colour = Temperature)) +
  geom_point(size = 1,
             alpha = 1,
             position = "jitter") +
  scale_y_log10() +  # Log scale for y-axis
  facet_wrap(
    ~ common_name,
    nrow = 1,
    scale = "free",
    strip.position = "top"
  ) +
  labs(x = NULL,#"Predictor Variable", 
       y = "log(Total Bird Call Detections)") +
  theme_half_open(font_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 10, face = "bold"),
    panel.spacing = unit(0.02, "npc"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    legend.position = "bottom"
  ) +
  scale_colour_viridis_d(
    name = "Temperature",
    #option = "viridis",  # Change palette here
    labels = c("Warm", "Cold"),
    guide = guide_legend(nrow = 1)
  ) 

ggsave("outputs/aru/model_figs/calling_temp_points.pdf", plot = calling_temp_points, width = 10, height = 4, units = "in")

calling_temp_lines <- ggplot(data = aru_plotting_data, aes(x = Date, y = total_count, colour = Temperature)) +
  geom_line(linewidth = 0.7, aes(group = locationID)) +
  geom_point(size = 1,
             alpha = 1,
             position = "jitter") +
  scale_y_log10() +  # Log scale for y-axis
  facet_wrap(
    ~ common_name,
    nrow = 1,
    scale = "free",
    strip.position = "top"
  ) +
  labs(x = NULL,#"Predictor Variable", 
       y = "log(Total Bird Call Detections)") +
  theme_half_open(font_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 10, face = "bold"),
    panel.spacing = unit(0.02, "npc"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    legend.position = "bottom"
  ) +
  scale_colour_viridis_d(
    name = "Temperature",
    #option = "viridis",  # Change palette here
    labels = c("Warm", "Cold"),
    guide = guide_legend(nrow = 1)
  ) 

ggsave("outputs/aru/model_figs/calling_temp_lines.pdf", plot = calling_temp_lines, width = 10, height = 4, units = "in")

#------------------------
# Output Table
#------------------------
library(gtsummary)
library(gt)
library(broom.mixed)
library(knitr)
library(webshot2)

aru_model_summary_table <- function(model, title = "Model Summary", plot_name = "model_table") {
  # Recode variables for cleaner labels
  recode_vars <- c(
    "intercept" = "Intercept",
    "Intercept" = "Intercept",
    "(Intercept)" = "Intercept",
    "(intercept)" = "Intercept",
    "day_from_start" = "Date",
    "temp_binarylow" = "Low Temperature",
    "day_from_start:temp_binarylow" = "Date:Low Temperature"
  )
  
  # Extract and format model summary
  model_output <- summary(model)$fixed %>%
    tibble::rownames_to_column(var = "Parameter") %>%
    mutate(across(c(Estimate, Est.Error, `l-95% CI`, `u-95% CI`), round, 3)) %>%
    mutate(across(c(Bulk_ESS, Tail_ESS), round, 0)) %>%
    mutate(across(c(Rhat), round, 3)) %>%
    rename(
      "Estimate" = Estimate,
      "Est.Error" = Est.Error,
      "Lower 95% CI" = `l-95% CI`,
      "Upper 95% CI" = `u-95% CI`,
      "R-hat" = Rhat,
      "Bulk ESS" = Bulk_ESS,
      "Tail ESS" = Tail_ESS
    ) %>%
    mutate(
      Parameter = recode(Parameter, !!!recode_vars)
    ) %>%
    mutate(Parameter = factor(Parameter, levels = c("Intercept", "Date", "Low Temperature", "Date:Low Temperature"))) %>%
    arrange(Parameter)
  
  # Create the GT table
  model_table <- model_output %>%
    gt() %>%
    tab_header(
      title = title
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
      locations = cells_body(rows = `Lower 95% CI` > 0 & `Upper 95% CI` > 0)
    ) %>%
    tab_style(
      style = list(
        cell_text(style = "italic"),
        cell_fill(color = "gainsboro")
      ),
      locations = cells_body(rows = `Lower 95% CI` < 0 & `Upper 95% CI` < 0)
    ) %>%
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = Parameter == "Intercept")
    ) %>%
    tab_style(
      style = cell_borders(sides = "top", color = "grey", weight = px(1)),
      locations = cells_body(rows = Parameter == "Date")
    )
  
  output_html <- paste0("outputs/aru/model_figs/", plot_name, ".html")
  output_img = paste0("outputs/aru/model_figs/", plot_name, ".png")
  # Save as HTML
  gtsave(model_table, output_html)
  
  # Save as PNG
  webshot(output_html, file = output_img, vwidth = 200, vheight = 160, zoom = 2)
  
  cat("Table saved to:", output_html, "\nImage saved to:", output_img, "\n")
}


aru_model_summary_table(model = passerine_model,
                        title = "Passerine Calling Response",
                        plot_name = "passerine_table_06feb2025")
aru_model_summary_table(model = shorebird_model,
                        title = "Shorebird Calling Response",
                        plot_name = "shorebird_table_06feb2025")
aru_model_summary_table(model = waterbird_model,
                        title = "Waterfowl Calling Response",
                        plot_name = "waterbird_table_06feb2025")


############################
# Categorical approach
#############


# Setting time periods (chosen arbitrarily)
period_length <- 5 # 20 days
early_start <- as.Date("2024-06-25") # Early time period
late_start <- as.Date("2024-07-1") # Late time period
early_daterange <- c(early_start, as.Date(early_start + period_length))
late_daterange <- c(late_start, as.Date(late_start + period_length))

# Count unique observation days for each species in the early and late periods
observation_days_count <- aru_daily %>%
  filter((date >= early_daterange[1] & date <= early_daterange[2]) |
           (date >= late_daterange[1] & date <= late_daterange[2])
  ) %>%
  mutate(period = as.factor(
    ifelse(
      date >= early_daterange[1] & date <= early_daterange[2],
      "early", "late"
    )
  )) %>%
  group_by(common_name, period, locationID) %>%
  summarise(observation_days = n_distinct(date), .groups = "drop")


# Join the observation days count back to the original dataset
day_threshold <- 5 # observation day threshold
aru_cat <- aru_daily %>% # categorical (early vs late)
  mutate(period = as.factor(
    ifelse(
      date >= early_daterange[1] & date <= early_daterange[2],"early",
      ifelse(date >= late_daterange[1] & date <= late_daterange[2], "late", NA)
    )
  )) %>%
  left_join(observation_days_count,
            by = c("common_name", "period", "locationID")) %>%
  filter(observation_days >= day_threshold)  # Filter out records with less than 5 observation days in each period

# Creating a catogorical summary dataset ready for analysis
aru_cat <- aru_cat %>%
  select(
    locationID,
    date,
    common_name,
    total_count,
    period,
    temp_binary,
    avg_temp,
    tomst_num,
    observation_days,
    non_snow_days
  ) %>%
  mutate(
    locationID = as.factor(locationID),
    date = as.factor(date),
    scaled_count = scale(total_count, center = TRUE, scale = TRUE),
    log_count = log(total_count) # Log transform total count, choosing not to +1
  )



passerine %>%
  count(temp_binary, period)
shorebird %>%
  count(temp_binary, period)
waterbird %>%
  count(temp_binary, period)



# passerine_pred <- passerine %>%
#   add_predicted_draws(passerine_model, ndraws = 100)
# # Plot observed vs. predicted values
# ggplot(passerine_pred, aes(x = .prediction, y = total_count)) +
#   geom_point(alpha = 0.3, color = "blue") +  # Scatter plot of observed vs. predicted
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Reference line
#   labs(
#     x = "Predicted Total Count",
#     y = "Observed Total Count",
#     title = "Observed vs. Predicted Total Count"
#   ) +
#   theme_minimal()

###########

calling_prior_test <- c(
  set_prior("normal(0, 7.5)", class = "b", coef = "temp_binarylow")
)

passerine_early <- passerine %>%
  filter(period == "early")

passerine_temp_only <- brm(
  total_count ~ temp_binary + (1|locationID) + (1|date),
  data = passerine_early,
  family = poisson(),
  #prior = calling_prior_test,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

summary(passerine_temp_only)

###################
calling_priors <- c(
  set_prior("normal(0, 6)", class = "b", coef = "periodlate"),
  set_prior("normal(0, 6)", class = "b", coef = "temp_binarylow")
)

passerine_calling <- brm(
  total_count ~ temp_binary + period + (1|locationID) + (1|date),
  data = passerine,
  family = poisson(),
  prior = calling_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

shorebird_calling <- brm(
  total_count ~ temp_binary + period + (1|locationID) + (1|date),
  data = shorebird,
  family = poisson(),
  prior = calling_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

waterbird_calling <- brm(
  total_count ~ temp_binary + period + (1|locationID) + (1|date),
  data = waterbird,
  family = poisson(),
  prior = calling_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.999, max_treedepth = 15)
)

# Passerine box plot 
ggplot(passerine, aes(x = period, y = total_count, fill = temp_binary)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +  # Dodge to separate boxes within each primary category
  scale_fill_brewer(palette = "Set2") +  # Optional: Change color palette
  labs(x = "time period", y = "Numeric Variable", fill = "temperature") +
  theme_minimal()

# guild_calling <- brm(total_count ~ (temp_binary + period) * common_name + (1|locationID) + (1|date), data = aru_summary, family = poisson(),
#                  prior = calling_priors,
#                  chains = 4, cores = 4, iter = 6000,
#                  control=list(adapt_delta=0.999, max_treedepth = 15))

# Posterior Predictive Checks
pp_check(passerine_calling, resp = "total_count", ndraws = 1000)
pp_check(passerine_calling_log, resp = "total_count", ndraws = 1000)
pp_check(shorebird_calling, resp = "total_count", ndraws = 1000)
pp_check(waterfowl_calling, resp = "total_count", ndraws = 1000)
pp_check(guild_calling, resp = "total_count", ndraws = 1000)

# Saving Model Outputs
model_output_path <- "outputs/aru/categorical/"
saveRDS(passerine_calling, file = paste0(model_output_path, "passerine.rds"))
saveRDS(shorebird_calling, file = paste0(model_output_path, "shorebird.rds"))
saveRDS(waterfowl_calling, file = paste0(model_output_path, "waterbird.rds"))
saveRDS(guild_calling, file = paste0(model_output_path, "guild.rds"))

# Read Model Outputs
model_output_path <- "outputs/aru/categorical/"
passerine_calling <- readRDS(file = paste0(model_output_path, "passerine.rds"))
shorebird_calling <- readRDS(file = paste0(model_output_path, "shorebird.rds"))
waterfowl_calling <- readRDS(file = paste0(model_output_path, "waterbird.rds"))
guild_calling <- readRDS(file = paste0(model_output_path, "guild.rds"))


summary(guild_calling)
summary(passerine_calling)
summary(passerine_calling_log)



############

# Figure from analysis: 2 boxplots of total count, one by period and one by temp_binary, for each species
passerine_plotting <- passerine %>%
  filter(period != "late")
shorebird_plotting <- shorebird %>%
  filter(period != "late")
waterfowl_plotting <- waterfowl %>%
  filter(period != "late")

ggplot(passerine_plotting, aes(x = temp_binary, y = total_count)) +
  geom_boxplot() +
  labs(title = "Total Count by Period and Temperature Binary",
       x = "ARU temperature",
       y = "Total Count",
       fill = "Temperature Binary") +
  theme_minimal()

ggplot(passerine_plotting, aes(x = avg_temp, y = log_count)) +
  geom_point() +
  labs(x = "ARU temperature",
       y = "Log of Total Count") +
  theme_half_open(font_size = 14)

ggplot(shorebird_plotting, aes(x = avg_temp, y = log_count)) +
  geom_point() +
  labs(x = "ARU temperature",
       y = "Log of Total Count") +
  theme_half_open(font_size = 14)

ggplot(waterfowl_plotting, aes(x = avg_temp, y = log_count)) +
  geom_point() +
  labs(x = "ARU temperature",
       y = "Log of Total Count") +
  theme_half_open(font_size = 14)



# Number of observations
n <- 50

# Create fake data
fake_passerine <- tibble(
  locationID = sample(c("ARUQ0", "ARUQ1", "ARUQ2", "ARUQ3"), n, replace = TRUE),
  date = as.Date("2024-06-21") + sample(0:49, n, replace = TRUE),
  common_name = sample(c("Lapland Longspur", "Snow Bunting", "Arctic Warbler"), n, replace = TRUE),
  total_count = rpois(n, lambda = 50),  # Random counts with average ~50
  log_count = log1p(rpois(n, lambda = 50)),  # Log-transformed count
  period = factor(sample(c("early", "late"), n, replace = TRUE)),
  temp_binary = factor(sample(c("high", "low"), n, replace = TRUE)),
  tomst_num = sample(1:40, n, replace = TRUE),
  observation_days = sample(5:21, n, replace = TRUE),
  non_snow_days = sample(20:47, n, replace = TRUE)
) %>%
  mutate(locationID = as.factor(locationID),
         date = as.factor(date))

# Creating Bayesian Model
# calling_priors <- c(
#   # set_prior("uniform(-200, 200)", coef = "periodlate", resp = "total_count"), # Prior for 'late' period
#   # set_prior("uniform(-200, 200)", coef = "temp_binarylow", resp = "total_count"),  # Prior for 'low' temp coefficient
#   # set_prior("uniform(-200, 200)", coef = "temp_binarylow:periodlate", resp = "total_count")  # Prior for interaction term
#   set_prior("uniform(-200, 200)", class = "b", coef = "periodlate"), # Prior for 'late' period
#   set_prior("uniform(-200, 200)", class = "b", coef = "temp_binarylow"),  # Prior for 'low' temp coefficient
#   set_prior("uniform(-200, 200)", class = "b", coef = "temp_binarylow:periodlate")  # Prior for interaction term
# )

# calling_priors <- c(
#   prior(normal(0, 6), class = "b", coef = "periodlate"),
#   prior(normal(0, 6), class = "b", coef = "temp_binarylow"),
#   prior(normal(0, 6), class = "b", coef = "temp_binarylow:periodlate")
# )

# calling_priors <- c(
#   set_prior("normal(0, 6)", class = "b", coef = "periodlate"),
#   set_prior("normal(0, 6)", class = "b", coef = "temp_binarylow"),
#   set_prior("normal(0, 6)", class = "b", coef = "temp_binarylow:periodlate")
# )
# 
# pass_call_mod <- brm(total_count ~ temp_binary * period + (1|locationID) + (1|date), data = fake_passerine, family = poisson(),
#              prior = calling_priors,
#              chains = 4, cores = 4, iter = 2000,
#              control=list(adapt_delta=0.999))
# 
# summary(pass_call_mod)


calling_priors <- c(
  set_prior("normal(0, 6)", class = "b", coef = "periodlate"),
  set_prior("normal(0, 6)", class = "b", coef = "temp_binarylow")
)

pass_call_mod <- brm(total_count ~ temp_binary + period + (1|locationID) + (1|date), data = fake_passerine, family = poisson(),
                     prior = calling_priors,
                     chains = 4, cores = 4, iter = 2000,
                     control=list(adapt_delta=0.999))

summary(pass_call_mod)


passerine_species <- "Lapland Longspur"
passerine_aru <- focal_aru %>% 
  filter(common_name == passerine_species) 


# Plot observations over time
ggplot(filter(passerine_aru, locationID == "ARUQ0"), aes(x = time_interval, y = log_values)) +
  geom_line() +  # Adjust scale for spacing
  scale_fill_viridis_d() +  # Discrete color scalescale_color_viridis_d() +  # Better color scale for discrete ARU locations
  labs(title = paste("Observations of", passerine_species, "over time"),
       x = "Time",
       y = "Species Count",
       color = "ARU Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









# filter the data to the early and late time periods
# aru_ranged <- aru_daily %>%
#   filter((date >= early_daterange[1] & date <= early_daterange[2]) | 
#            (date >= late_daterange[1] & date <= late_daterange[2])) %>%
#   mutate(period = as.factor(ifelse(date >= early_daterange[1] & date <= early_daterange[2], "early", "late"))) %>%
#   group_by(locationID, common_name, period) %>%
#   summarise(observation_days = n_distinct(date)) %>%
#   ungroup()

## OLD TABLE ATTEMPTS ####
# recode_vars <- c(
#   "intercept" = "Intercept",
#   "Intercept" = "Intercept",
#   "(Intercept)" = "Intercept",
#   "(intercept)" = "Intercept",
#   "day_from_start" = "Date",
#   "temp_binarylow" = "Low Temperature",
#   "day_from_start:temp_binarylow" = "Date:Low Temperature"
# )
# 
# 
# pass_output <- summary(passerine_model)$fixed %>%
#   tibble::rownames_to_column(var = "Parameter") %>%
#   #rename(rhat = Rhat, bulk.ess = Bulk_ESS, tail.ess = Tail_ESS, estimate = Estimate, est.error = Est.Error, l95.ci = `l-95% CI`, u95.ci = `u-95% CI`) %>%
#   mutate(across(c(Estimate, Est.Error, `l-95% CI`, `u-95% CI`), round, 3)) %>%
#   mutate(across(c(Bulk_ESS, Tail_ESS), round, 0)) %>%
#   mutate(across(c(Rhat), round, 3)) %>%
#   rename(
#     "Estimate" = Estimate,
#     "Est.Error" = Est.Error,
#     "Lower 95% CI" = `l-95% CI`,
#     "Upper 95% CI" = `u-95% CI`,
#     "R-hat" = Rhat,
#     "Bulk ESS" = Bulk_ESS,
#     "Tail ESS" = Tail_ESS
#   ) %>% 
#   mutate(
#     Parameter = recode(Parameter, !!!recode_vars)
#   ) %>%
#   mutate(Parameter = factor(Parameter, levels = c("Intercept", "Date", "Low Temperature", "Date:Low Temperature"))) %>%
#   arrange(Parameter)
# 
# # Now create the table with the extra column
# passerine_table <- pass_output %>%
#   gt() %>% 
#   tab_header(
#     title = "Passerine Calling Response"
#   ) %>% 
#   tab_style(
#     style = cell_text(weight = "bold"),    # Bold the header text
#     locations = cells_column_labels(everything()) # Apply to all column headers
#   ) %>%
#   tab_style(
#     style = list(
#       cell_fill(color = "gainsboro"),
#       cell_text(style = "italic")
#     ),
#     locations = cells_body(rows = `Lower 95% CI` > 0 & `Upper 95% CI` > 0)
#   ) %>% 
#   tab_style(
#     style = list(
#       cell_text(style = "italic"),
#       cell_fill(color = "gainsboro")
#     ),
#     locations = cells_body(rows = `Lower 95% CI` < 0 & `Upper 95% CI` < 0)
#   ) %>% 
#   tab_style(
#     style = cell_borders(sides = "top", color = "black", weight = px(2)),
#     locations = cells_body(rows = Parameter == "Intercept")
#   ) %>% 
#   tab_style(
#     style = cell_borders(sides = "top", color = "grey", weight = px(1)),
#     locations = cells_body(rows = Parameter == "Date")
#   )
# 
# passerine_table
# 
# gtsave(passerine_table, "outputs/aru/temp_table.html")  # Save as HTML first
# webshot("outputs/aru/temp_table.html", file ="outputs/aru/passerine_table_06feb2025_1.png", 
#         vwidth = 300, vheight = 160, zoom = 2)  # Adjust the dimensions here
