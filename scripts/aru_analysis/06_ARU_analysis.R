

######### HAVENT DEALT WITH THE PROBLEM OF ARUS STOPS
######### Haven't chosen early and late time periods yet


library(brms)


confidence_threshold <- 0.5
species_list <- c("Lapland Longspur", "Semipalmated Plover", "Common Eider")

aru_dataframe <- read_csv(paste0("data/clean/aru/aru_analysis_data", "_conf", confidence_threshold,".csv")) 

ARU_average_temps <- read_csv("data/clean/aru/tomst_averages.csv")

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

# Setting time periods
period_length <- 20 # 20 days
# Early 15 day time period
early_start <- as.Date("2024-06-20")
early_daterange <- c(early_start, as.Date(early_start + period_length))
# Late 15 day time period
late_start <- as.Date("2024-07-20")
late_daterange <- c(late_start, as.Date(late_start + period_length))


# filter the data to the early and late time periods
# aru_ranged <- aru_daily %>%
#   filter((date >= early_daterange[1] & date <= early_daterange[2]) | 
#            (date >= late_daterange[1] & date <= late_daterange[2])) %>%
#   mutate(period = as.factor(ifelse(date >= early_daterange[1] & date <= early_daterange[2], "early", "late"))) %>%
#   group_by(locationID, common_name, period) %>%
#   summarise(observation_days = n_distinct(date)) %>%
#   ungroup()

# Filter data and count unique observation days for each species
observation_days_count <- aru_daily %>%
  filter((date >= early_daterange[1] & date <= early_daterange[2]) | 
           (date >= late_daterange[1] & date <= late_daterange[2])) %>%
  mutate(period = as.factor(ifelse(date >= early_daterange[1] & date <= early_daterange[2], "early", "late"))) %>%
  group_by(common_name, period, locationID) %>%
  summarise(observation_days = n_distinct(date), .groups = "drop")

# Join the observation days count back to the original dataset
day_threshold <- 5 # observation day threshold
aru_ranged <- aru_daily %>%
  mutate(period = as.factor(ifelse(date >= early_daterange[1] & date <= early_daterange[2], "early", 
                                   ifelse(date >= late_daterange[1] & date <= late_daterange[2], "late", NA)))) %>%
  left_join(observation_days_count, by = c("common_name", "period", "locationID")) %>%
  filter(observation_days >= day_threshold)  # Filter out records with less than 5 observation days in each period

aru_summary <- aru_ranged %>%
  select(locationID, date, common_name, total_count, period, temp_binary, avg_temp, tomst_num, observation_days, non_snow_days) %>%
  mutate(locationID = as.factor(locationID),
         date = as.factor(date),
         scaled_count = scale(total_count, center = TRUE, scale = TRUE),
         log_count = log(total_count) # Log transform total count, choosing not to +1  
         )


# Filtering by taxa
passerine <- aru_summary %>%
  filter(common_name == "Lapland Longspur")
shorebird <- aru_summary %>%
  filter(common_name == "Semipalmated Plover")
waterfowl <- aru_summary %>%
  filter(common_name == "Common Eider")


passerine %>%
  count(temp_binary, period)
shorebird %>%
  count(temp_binary, period)
waterfowl %>%
  count(temp_binary, period)


calling_priors <- c(
  set_prior("normal(0, 6)", class = "b", coef = "periodlate"),
  set_prior("normal(0, 6)", class = "b", coef = "temp_binarylow")
)

passerine_calling <- brm(total_count ~ temp_binary + period + (1|locationID) + (1|date), data = passerine, family = poisson(),
                     prior = calling_priors,
                     chains = 4, cores = 4, iter = 4000,
                     control=list(adapt_delta=0.999, max_treedepth = 15))

shorebird_calling <- brm(total_count ~ temp_binary + period + (1|locationID) + (1|date), data = passerine, family = poisson(),
                         prior = calling_priors,
                         chains = 4, cores = 4, iter = 4000,
                         control=list(adapt_delta=0.999, max_treedepth = 15))

waterfowl_calling <- brm(total_count ~ temp_binary + period + (1|locationID) + (1|date), data = passerine, family = poisson(),
                         prior = calling_priors,
                         chains = 4, cores = 4, iter = 4000,
                         control=list(adapt_delta=0.999, max_treedepth = 15))

guild_calling <- brm(total_count ~ (temp_binary + period) * common_name + (1|locationID) + (1|date), data = aru_summary, family = poisson(),
                 prior = calling_priors,
                 chains = 4, cores = 4, iter = 6000,
                 control=list(adapt_delta=0.999, max_treedepth = 15))

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




# Passerine box plot 
ggplot(passerine, aes(x = period, y = total_count, fill = temp_binary)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +  # Dodge to separate boxes within each primary category
  scale_fill_brewer(palette = "Set2") +  # Optional: Change color palette
  labs(x = "time period", y = "Numeric Variable", fill = "temperature") +
  theme_minimal()











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
