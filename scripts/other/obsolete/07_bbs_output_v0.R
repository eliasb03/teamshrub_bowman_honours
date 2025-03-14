



library(cowplot)

# Read model outputs
model_output_path <- "outputs/sem/abundance/"
passerine_bsem <- readRDS(file = paste0(model_output_path, "passerine.rds"))
guild_bsem <- readRDS(file = paste0(model_output_path, "guild.rds"))

# Create model outputs in a nicely formatted way
process_fixed_effects <- function(model) {
  summary(model)$fixed %>%
    tibble::rownames_to_column(var = "Parameter") %>%
    mutate(parameter = Parameter) %>%  # Keep a copy of the original column
    separate(Parameter, into = c("response", "predictor"), sep = "_", extra = "merge", fill = "right") %>%
    mutate(
      # Extract interaction values
      interaction = case_when(
        grepl(":guild", predictor) ~ sub(".*:guild", "", predictor),  # Extract after ":guild"
        grepl("^guild", predictor) ~ sub("^guild", "", predictor),    # Extract after "guild" (for interaction intercepts)
        TRUE ~ NA_character_
      ),
      # Remove :guildX from predictor but keep the main term
      predictor = case_when(
        grepl(":guild", predictor) ~ sub(":guild.*", "", predictor),  # Remove everything after ":guild"
        grepl("^guild", predictor) ~ "Intercept",  # Replace predictor for interaction intercepts
        TRUE ~ predictor
      ),
      # Apply exp() only to rows where response is "birdabundance" and delay_invlink is FALSE
      # across(c(estimate = Estimate, est.error = Est.Error, l95.ci = `l-95% CI`, u95.ci = `u-95% CI`),
      #        ~ ifelse((response == "birdabundance" & !delay_invlink), exp(.x), .x))
    ) %>%
    rename(rhat = Rhat, bulk.ess = Bulk_ESS, tail.ess = Tail_ESS, estimate = Estimate, est.error = Est.Error, l95.ci = `l-95% CI`, u95.ci = `u-95% CI`) %>%
    select(response, predictor, interaction, estimate, est.error, l95.ci, u95.ci, parameter, rhat, bulk.ess, tail.ess)
}
guild_bsem_effect <- process_fixed_effects(guild_bsem)


# List of variable names
variables <- c("icemelt", "breedingtemp", "snowmelt", "budburst")

# Get the responses using lapply, including the "guild" term
responses <- lapply(variables, function(var) predict_response(guild_bsem_zeroless, terms = c(var, "guild")))

# Name the responses with the variable names directly
names(responses) <- variables

# Process and combine data frames using imap_dfr
resp_data <- imap_dfr(responses, ~ as.data.frame(.x) %>%
                        filter(response.level == "birdabundance") %>%
                        mutate(predictor = .y))  # .y is the name of the list element

# Plot icemelt_resp
sem_data_plotting <- sem_data_zeroless %>%
  mutate(group = guild) %>%
  filter(complete.cases(.)) %>%
  #rename(temp = breedingtemp) %>%
  pivot_longer(cols = c("icemelt", "breedingtemp", "snowmelt", "budburst"), names_to = "predictor", values_to = "value")

# set a ylimit
ylim_threshold <- 40

# Print the number of excluded data points
excluded_data <- sem_data_zeroless %>% 
  filter(birdabundance > ylim_threshold)
cat("Number of data points excluded: ", nrow(excluded_data), "\n")

# Recalibrating Data
scaling_params <- read_csv("data/clean/sem/scaling_params.csv") %>%
  rename("predictor" = "variable") #%>%
  # mutate(predictor = case_when(predictor == "breedingtemp" ~ "temp",
  #                       TRUE ~ predictor))   # change breedingtemp to temp

resp_data <- resp_data %>%
  left_join(scaling_params, by = "predictor")%>%   # Join based on predictor
  mutate(x_cal = (x * scaling_value) + mean)          # Revert centering and scaling 

sem_data_plotting <- sem_data_plotting %>%
  left_join(scaling_params, by = "predictor") %>%   # Join based on predictor
  mutate(value_cal = (value*scaling_value) + mean)  # Add the calibration value to value

# Plot the calibrated data
ggplot(data = resp_data, aes(x = x_cal, y = predicted, colour = group)) +
  geom_line(size = 1.2) + 
  geom_point(data = sem_data_plotting, aes(x = value_cal, y = birdabundance, colour = group), 
             size = 1, alpha = 0.5, position = "jitter") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, colour = NA) +  # Fill ribbons
  scale_colour_viridis_d(name = "Guild",  
                         labels = c("Waterbird", "Passerine", "Shorebird"), 
                         guide = "legend") +  # Combined legend for colour
  scale_fill_viridis_d(name = "Guild",  
                       labels = c("Waterbird", "Passerine", "Shorebird"), 
                       guide = "legend") +  # Combined legend for fill
  labs(x = "Predictor Variable", y = "Predicted Bird Abundance") +
  theme_half_open(font_size = 12) +  
  theme(
    legend.position = "right",
    strip.background = element_blank(),
    strip.placement = "outside",           # Ensure strip labels are placed outside the panels
    strip.text = element_text(size = 10),     # Adjust the size as needed
    panel.spacing = unit(0.04, "npc")      # Increase space between panels
  ) + 
  facet_wrap(~predictor, nrow = 2, scale = "free_x",
             strip.position = "bottom",     # Move facet labels to the bottom
             labeller = as_labeller(c("icemelt" = "Ice Melt\n(Day of Year)", 
                                      "budburst" = "Budburst\n(Day of Year)",
                                      "breedingtemp" = "Spring Temperature\n(Degrees C)",
                                      "snowmelt" = "Snowmelt\n(Day of Year)"))) +
  coord_cartesian(ylim = c(0, ylim_threshold))








##########


ggplot(data = resp_data, aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 2) + 
  geom_point(data = sem_data_plotting, aes(x = value, y = birdabundance, colour = group), 
             size = 1, alpha = 0.5, position = "jitter") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, colour = NA) +  # Fill ribbons
  scale_colour_viridis_d(name = "Guild",  
                         labels = c("Waterbird", "Passerine", "Shorebird"), 
                         guide = "legend") +  # Combined legend for colour
  scale_fill_viridis_d(name = "Guild",  
                       labels = c("Waterbird", "Passerine", "Shorebird"), 
                       guide = "legend") +  # Combined legend for fill
  labs(x = "Predictor", y = "Predicted Bird Abundance") +
  theme_half_open(font_size = 15) +  
  theme(
    legend.position = "right",
    strip.background = element_blank(),
    strip.placement = "outside",           # Ensure strip labels are placed outside the panels
    strip.text = element_text(size = 10)     # Adjust the size as needed
  ) + 
  facet_wrap(~predictor, nrow = 2, scale = "free_x",
             strip.position = "bottom",     # Move facet labels to the bottom
             labeller = as_labeller(c("icemelt" = "Week of Ice Melt", 
                                      "budburst" = "Week of Budburst",
                                      "temp" = "Average Spring Temperature",
                                      "snowmelt" = "Week of Snowmelt"))) +
  coord_cartesian(ylim = c(0, ylim_threshold))


ggplot(data = resp_data, aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 2) + 
  geom_point(data = sem_data_plotting, aes(x = value, y = birdabundance, colour = group), 
             size = 1, alpha = 0.5, position = "jitter") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, colour = NA) +  # Fill ribbons
  scale_colour_viridis_d(name = "Guild",  
                         labels = c("Waterbird", "Passerine", "Shorebird"), 
                         guide = "legend") +  # Combined legend for colour
  scale_fill_viridis_d(name = "Guild",  
                       labels = c("Waterbird", "Passerine", "Shorebird"), 
                       guide = "legend") +  # Combined legend for fill
  labs(x = "Predictor", y = "Predicted Bird Abundance") +
  theme_half_open(font_size = 15) +  
  theme(
    legend.position = "right",
    strip.background = element_blank()
  ) + 
  facet_wrap(~predictor, nrow = 2, scale = "free_x",
             labeller = as_labeller(c("icemelt" = "Ice Melt", 
                                      "budburst" = "Budburst",
                                      "temp" = "Spring Temperature",
                                      "snowmelt" = "Snowmelt"))) +
  coord_cartesian(ylim = c(0, ylim_threshold))





####################
# Response predictions
icemelt_resp <- predict_response(guild_bsem_zeroless, terms = c("icemelt", "guild")) 
temp_resp <- predict_response(guild_bsem_zeroless, terms = c("breedingtemp", "guild"))
snowmelt_resp <- predict_response(guild_bsem_zeroless, terms = c("snowmelt", "guild"))
budburst_resp <- predict_response(guild_bsem_zeroless, terms = c("budburst", "guild"))



icemelt_resp_df <- as.data.frame(icemelt_resp) %>%
  filter(response.level == "birdabundance") %>%
  mutate(predictor = "icemelt")
temp_resp_df <- as.data.frame(temp_resp) %>%
  filter(response.level == "birdabundance") %>%
  mutate(predictor = "temp")
snowmelt_resp_df <- as.data.frame(snowmelt_resp) %>%
  filter(response.level == "birdabundance") %>%
  mutate(predictor = "snowmelt")
budburst_resp_df <- as.data.frame(budburst_resp) %>%
  filter(response.level == "birdabundance") %>%
  mutate(predictor = "budburst")

# Combine all response predictions
resp_data <- bind_rows(icemelt_resp_df, temp_resp_df, snowmelt_resp_df, budburst_resp_df)

# ggplot(data = icemelt_resp, aes(x = x, y = predicted, colour = group)) +
#   geom_line(size = 1.2) +  # Thicker lines for clarity
#   geom_point(data = sem_data_plotting, aes(x = icemelt, y = birdabundance, colour = group), size = 2,position = "jitter") +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, colour = NA) +  # Fill ribbons instead of border
#   scale_colour_brewer(palette = "Set1", name = "Group") +  # Consistent color palette for lines
#   scale_fill_brewer(palette = "Set1") +  # Matching fill colors for ribbons
#   scale_y_continuous(transform = "log10") + 
#   labs(x = "Ice Melt Week", y = "Predicted Bird Abundance") +
#   theme_minimal(base_size = 14) +  # Larger base text size
#   theme(
#     legend.position = "right",
#     panel.grid.major = element_blank(),  # Remove major grid lines
#     panel.grid.minor = element_blank()   # Remove minor grid lines
#   ) +
#   facet_wrap(~group,nrow = 3)#,scales = "free_y")


ggplot(data = icemelt_resp, aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 1.2) +  # Thicker lines for clarity
  geom_point(data = sem_data_plotting, aes(x = icemelt, y = birdabundance, colour = group), size = 2, alpha = 0.5, position = "jitter") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, colour = NA) +  # Fill ribbons instead of border
  scale_colour_brewer(palette = "Set1", name = "group") +  # Consistent color palette for lines
  scale_fill_brewer(palette = "Set1") +  # Matching fill colors for ribbons
  #scale_y_continuous(transform = "log10") + 
  labs(x = "Ice Melt Week", y = "Predicted Bird Abundance") +
  theme_minimal(base_size = 14) +  # Larger base text size
  theme(
    legend.position = "right",
    #panel.grid.major = element_blank(),  # Remove major grid lines
    #panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +scale_fill_viridis_d()+scale_colour_viridis_d() 


summary(guild_bsem)
ggplot(icemelt_resp, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.2) +
  labs(x = "Ice melt date", y = "Predicted bird abundance") +
  theme_minimal()



###########
# Gridded plot of birdabundance responses
ggplot(data = resp_data, aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 1.2) +  # Thicker lines for clarity
  geom_point(data = sem_data_plotting, aes(x = value, y = birdabundance, colour = group), size = 1, alpha = 0.5, position = "jitter") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, colour = NA) +  # Fill ribbons instead of border
  scale_colour_viridis_d(name = "group") +  
  scale_fill_viridis_d() +  
  labs(x = "Predictor", y = "Predicted Bird Abundance") +
  theme_half_open(font_size = 14) +  # Larger base text size
  theme(
    legend.position = "right"
  ) + facet_wrap(~predictor, nrow = 2, scale = "free_x")



ggplot(data = resp_data, aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 2) +  # Thicker lines for clarity
  geom_point(data = sem_data_plotting, aes(x = value, y = birdabundance, colour = group), 
             size = 1, alpha = 0.5, position = "jitter") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, colour = NA) +  # Fill ribbons instead of border
  scale_colour_viridis_d(name = "Guild",  
                         labels = c("Waterbird", "Passerine", "Shorebird"), 
                         guide = "legend") +  # Combined legend for colour
  scale_fill_viridis_d(name = "Guild",  
                       labels = c("Waterbird", "Passerine", "Shorebird"), 
                       guide = "legend") +  # Combined legend for fill
  labs(x = "Predictor", y = "Predicted Bird Abundance") +
  theme_half_open(font_size = 15) +  # Larger base text size
  theme(
    legend.position = "right",
    strip.background = element_blank()
  ) + 
  facet_wrap(~predictor, nrow = 2, scale = "free_x",
             labeller = as_labeller(c("icemelt" = "Ice Melt", 
                                      "budburst" = "Budburst",
                                      "temp" = "Spring Temperature",
                                      "snowmelt" = "Snowmelt"))) 

