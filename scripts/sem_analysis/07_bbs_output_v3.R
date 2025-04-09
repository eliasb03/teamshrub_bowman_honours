#------------------------------
# teamshrub_bowman_honours
# 07_bbs_output_v3.R
# By: Elias Bowman 
# Created: 2025-04-09
# 
# Description: This script will construct bbs_sem model outputs
#------------------------------

library(tidyverse)
library(ggeffects)
library(cowplot)
library(dplyr)
library(lavaan)
library(semPlot)


colour_palette <- c(
  "passerine" = "#CC79A7",      # Passerine
  "Passerine" = "#CC79A7",      # Passerine
  "shorebird" = "#E69F00",      # Shorebird
  "Shorebird" = "#E69F00",      # Shorebird
  "waterbird" = "#0072B2",      # Waterbird
  "Waterbird" = "#0072B2",      # Waterbird
  "regional_temp" = "#999999",  # Regional Temperature
  "breeding_temp" = "#F0E442",  # Breeding Temperature
  "icemelt" = "#56B4E9",        # Icemelt
  "snowmelt" = "#D55E00",       # Snowmelt
  "budburst" = "#009E73",       # Budburst
  "other" = "#000000"
)

facet_colors <- c(
  "budburst" = "#009E73",   # Green (Budburst)
  "snowmelt" = "#D55E00",   # Red (Snowmelt)
  "breedingtemp" = "#F0E442", # Yellow (Breeding Temp)
  "icemelt" = "#0072B2"     # Blue (Icemelt)
)

# colour_palette <- c(
#   "shorebird" = "#DDCC77",      # Shorebird
#   "waterbird" = "#332288",      # Waterbird
#   "passerine" = "#44AA99",      # Passerine
#   "regional_temp" = "#AA4499",  # Regional Temperature
#   "breeding_temp" = "#CC6677",  # Breeding Temperature
#   "icemelt" = "#88CCEE",        # Icemelt
#   "snowmelt" = "#882255",       # Snowmelt
#   "budburst" = "#117733",       # Budburst
#   "other" = "#000000"
# )


# Read model outputs
model_output_path <- "outputs/sem/abundance/"
#passerine_bsem <- readRDS(file = paste0(model_output_path, "passerine.rds"))
guild_bsem <- readRDS(file = paste0(model_output_path, "guild.rds"))
summary(guild_bsem)

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

add_interaction_title <- function(data) {
  data %>%
    group_by(response, predictor) %>%
    mutate(
      interaction = case_when(
        is.na(interaction) & response == "birdabundance" ~ "passerine",
        interaction == "shorebird" ~ "shorebird",
        interaction == "waterbird" ~ "waterbird",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      solved = case_when(
        interaction == "passerine" ~ TRUE,
        is.na(interaction) ~ TRUE,
        TRUE ~ FALSE
      )
    )
}

create_intercepts <- function(data) {
  data %>%
    filter(predictor == "Intercept") %>%
    select(-solved, -rhat, -bulk.ess, -tail.ess) %>%
    rename(
      intercept = estimate,
      intercept_error = est.error,
      intercept_l95 = l95.ci,
      intercept_u95 = u95.ci,
      intercept_parameter = parameter
    ) %>%
    mutate(intercept = case_when(
      interaction %in% c("shorebird", "waterbird") ~ intercept + intercept[interaction == "passerine"],
      TRUE ~ intercept
    )) %>%
    ungroup() %>%
    select(-predictor)
}

add_intercepts <- function(data, intercepts) {
  data %>% 
    filter(predictor != "Intercept") %>%
    left_join(intercepts, by = c("response", "interaction"))
}

address_interaction <- function(data) {
  data %>%
    group_by(response, predictor) %>%
    mutate(
      new.estimate = case_when(
        solved ~ estimate,
        interaction %in% c("shorebird", "waterbird") ~ estimate + estimate[interaction == "passerine"]
      ),
      new.est.error = case_when(
        solved ~ est.error,
        interaction %in% c("shorebird", "waterbird") ~ est.error + est.error[interaction == "passerine"]
      ),
      new.l95.ci = case_when(
        solved ~ l95.ci,
        interaction %in% c("shorebird", "waterbird") ~ l95.ci + l95.ci[interaction == "passerine"]
      ),
      new.u95.ci = case_when(
        solved ~ u95.ci,
        interaction %in% c("shorebird", "waterbird") ~ u95.ci + u95.ci[interaction == "passerine"]
      )
    ) %>%
    ungroup()
}

# est error and 95% confidence intervals do not work in this process for interactions
bsem_processed <- guild_bsem %>%
  process_fixed_effects()

bsem_processed <- bsem_processed %>%
  add_interaction_title()

intercepts <- bsem_processed %>%
  create_intercepts()

bsem_processed <- bsem_processed %>%
  address_interaction() %>%
  # select(response, predictor, interaction,
  #        new.estimate, new.est.error, new.l95.ci, new.u95.ci, parameter, 
  #        intercept, intercept_parameter, intercept_error, intercept_l95, intercept_u95,
  #        estimate, est.error, l95.ci, u95.ci, rhat, bulk.ess, tail.ess) %>%
  select(response, predictor, interaction,
         new.estimate, new.est.error, new.l95.ci, new.u95.ci, parameter, 
         #intercept, intercept_parameter, intercept_error, intercept_l95, intercept_u95,
         estimate, est.error, l95.ci, u95.ci, rhat, bulk.ess, tail.ess) %>%
  mutate(response_single = if_else(
    is.na(interaction), 
    response, 
    paste0(interaction, str_sub(response, 5, nchar(response)))
  ))

#---------------------------
# Summary Table
#---------------------------

library(gtsummary)
library(gt)
library(broom.mixed)
library(knitr)
library(webshot2)
library(dplyr)

recode_vars <- c(
  "intercept" = "Intercept",
  "Intercept" = "Intercept",
  "(Intercept)" = "Intercept",
  "(intercept)" = "Intercept",
  "birdabundance" = "Bird Abundance",
  "breedingtemp" = "Breeding Temperature",
  "regiontemp" = "Regional Temperature",
  "budburst" = "Budburst",
  "icemelt" = "Ice Melt",
  "snowmelt" = "Snowmelt",
  "passerine" = "Passerine",
  "shorebird" = "Shorebird",
  "waterbird" = "Waterbird",
  
  "guildshorebird" = "Shorebird",
  "breedingtemp:guildshorebird" = "Breeding Temperature:Shorebird",
  "icemelt:guildshorebird" = "Icemelt:Shorebird",
  "snowmelt:guildshorebird" = "Snowmelt:Shorebird",
  "budburst:guildshorebird" = "Budburst:Shorebird",
  
  "guildwaterbird" = "Waterbird",
  "breedingtemp:guildwaterbird" = "Breeding Temperature:Waterbird",
  "icemelt:guildwaterbird" = "Icemelt:Waterbird",
  "snowmelt:guildwaterbird" = "Snowmelt:Waterbird",
  "budburst:guildwaterbird" = "Budburst:Waterbird"
  
)


tidy_output <- summary(guild_bsem)$fixed %>%
  tibble::rownames_to_column(var = "Parameter") %>%
  separate(Parameter, into = c("response", "term"), sep = "_", extra = "merge", fill = "right") %>%
  rename(rhat = Rhat, bulk.ess = Bulk_ESS, tail.ess = Tail_ESS, estimate = Estimate, est.error = Est.Error, l95.ci = `l-95% CI`, u95.ci = `u-95% CI`)

output_tbl <- tidy_output %>%
  #select(response, term, estimate, est.error, l95.ci, up95.ci, ) %>%
  mutate(across(c(estimate, est.error, l95.ci, u95.ci), round, 3)) %>%
  mutate(across(c(bulk.ess, tail.ess), round, 0)) %>%
  mutate(across(c(rhat), round, 2)) %>%
  rename(
    "Response" = response,
    "Parameter" = term,
    "Estimate" = estimate,
    "Est.Error" = est.error,
    "Lower 95% CI" = l95.ci,
    "Upper 95% CI" = u95.ci,
    "R-hat" = rhat,
    "Bulk ESS" = bulk.ess,
    "Tail ESS" = tail.ess
  ) %>% 
  mutate(
    Response = dplyr::recode(Response, !!!recode_vars),
    Parameter = dplyr::recode(Parameter, !!!recode_vars)
  ) %>%
  mutate(Response = factor(Response, levels = c("Bird Abundance", "Ice Melt", "Breeding Temperature", "Snowmelt", "Budburst"))) %>%
  mutate(Parameter = factor(Parameter, levels = c("Intercept", "Shorebird", "Waterbird", "Ice Melt", "Icemelt:Shorebird", "Icemelt:Waterbird", "Breeding Temperature","Breeding Temperature:Shorebird", "Breeding Temperature:Waterbird",  "Snowmelt","Snowmelt:Shorebird", "Snowmelt:Waterbird", "Budburst", "Budburst:Shorebird", "Budburst:Waterbird", "Regional Temperature"))) %>%
  arrange(Response, Parameter)



# Now create the table with the extra column
output_table <- output_tbl %>%
  gt() %>% 
  tab_header(
    title = "Bird Abundance SEM Model Output"
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),    # Bold the header text
    locations = cells_column_labels(everything()) # Apply to all column headers
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
    locations = cells_body(rows = Parameter %in% c("Ice Melt", "Breeding Temperature", "Snowmelt", "Budburst"))
  ) %>%
  # tab_style(
  #   style = cell_text(align = "left"),
  #   locations = cells_body(columns = c("Parameter", "Response"))
  # ) %>%
  cols_move_to_start("Parameter")

output_table
# gtsave(output_table, 
#        filename = "outputs/sem/plot/guild_output_05feb2025_3.png", 
#        width = 200,    # Increased width to ensure all content fits
#        height = 160)   # Increased height to ensure all content fits

gtsave(output_table, "outputs/sem/plot/temp_table.html")  # Save as HTML first
webshot(
  "outputs/sem/plot/temp_table.html",
  file = "outputs/sem/plot/guild_output_08mar2025.png",
  vwidth = 200,
  vheight = 160,
  zoom = 2
)  # Adjust the dimensions here



#---------------------------
# Bird Abundance Specific Variable Outputs
#---------------------------
# List of variable names
variables <- c("icemelt", "breedingtemp", "snowmelt", "budburst")

# Get the responses using lapply, including the "guild" term
responses <- lapply(variables, function(var) predict_response(guild_bsem, terms = c(var, "guild")))

# Name the responses with the variable names directly
names(responses) <- variables

# Recalibrating Data
scaling_params <- read_csv("data/clean/sem/scaling_params.csv") %>%
  rename("predictor" = "variable") #%>%

resp_data <- imap_dfr(responses, ~ as.data.frame(.x) %>%
                        filter(response.level == "birdabundance") %>%
                        mutate(predictor = .y))  # .y is the name of the list element

resp_data <- resp_data %>%
  left_join(scaling_params, by = "predictor") %>%   # Join based on predictor
  mutate(x_cal = (x * scaling_value) + mean)          # Revert centering and scaling 


predicted_effect_sizes <- resp_data %>%
  group_by(predictor, group) %>%
  arrange(abs(x)) %>%
  slice(1:3) %>%
  ungroup() %>%
  group_by(predictor, group) %>%
  summarize(
    min_x = min(x),
    max_x = max(x),
    left_y = predicted[which.min(x)],
    right_y = predicted[which.max(x)],
    slope = (left_y - right_y) / (min_x - max_x)
  )


sem_data <- read_csv("data/clean/sem/sem_data.csv") %>%
  filter(guild != "birdofprey") %>% # remove bird of prey guild
  filter(birdabundance > 0) 

# Plot icemelt_resp
sem_data_plotting <- sem_data %>%
  mutate(group = guild) %>%
  filter(complete.cases(.)) %>%
  pivot_longer(cols = c("icemelt", "breedingtemp", "snowmelt", "budburst"), names_to = "predictor", values_to = "value")

# set a ylimit
ylim_threshold <- 40

# Print the number of excluded data points
excluded_data <- sem_data %>% 
  filter(birdabundance > ylim_threshold)
cat("Number of data points excluded: ", nrow(excluded_data), "\n")

sem_data_plotting <- sem_data_plotting %>%
  left_join(scaling_params, by = "predictor") %>%   # Join based on predictor
  mutate(value_cal = (value * scaling_value) + mean)  # Add the calibration value to value

# Plot the calibrated data
resp_data$predictor <- factor(resp_data$predictor, levels = c("icemelt","breedingtemp", "snowmelt", "budburst"))
sem_data_plotting$predictor <- factor(sem_data_plotting$predictor, levels = c("icemelt","breedingtemp", "snowmelt", "budburst"))
group_levels <- c("passerine", "shorebird", "waterbird")
group_labels <- c("Passerine", "Shorebird", "Waterbird")

# Apply to both dataframes
resp_data$group <- factor(resp_data$group, levels = group_levels, labels = group_labels)
sem_data_plotting$group <- factor(sem_data_plotting$group, levels = group_levels, labels = group_labels)

bird_plot_1x4 <- 
  ggplot(data = resp_data, aes(x = x_cal, y = predicted, colour = group)) +
  geom_line(linewidth = 1.3) + 
  geom_point(data = sem_data_plotting, aes(x = value_cal, y = birdabundance, colour = group), 
             size = 1, alpha = 0.5, position = "jitter") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.15, colour = NA) +  # Fill ribbons
  scale_colour_manual(name = "Guild", values = colour_palette) +  # Use custom colours
  scale_fill_manual(name = "Guild", values = colour_palette) +  # Use custom fill colours
  labs(x = NULL, y = "Predicted Bird Abundance") +
  theme_half_open(font_size = 12) +  
  theme(
    legend.position = "bottom", #c(0.5, 1),#-0.34),
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title.align = 0.5,
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 10),
    panel.spacing = unit(0.03, "npc"),
    plot.margin = margin(t = 5, r = 10, b = 5, #24, 
                         l = 10)
  ) +
  facet_wrap(~predictor, nrow = 1, scale = "free_x",
             strip.position = "bottom",
             labeller = as_labeller(c(
               "icemelt" = "Ice Melt\n(Day of Year)", 
               "budburst" = "Budburst\n(Day of Year)",
               "breedingtemp" = "Breeding Temperature\n(Degrees C)",
               "snowmelt" = "Snowmelt\n(Day of Year)"))) +
  coord_cartesian(ylim = c(0, ylim_threshold)) 

ggsave("outputs/sem/plot/bird_abundance_preds_1x4_09mar2025.png", bird_plot_1x4, width = 10, height = 3.5, dpi = 600)


bird_plot_2x2 <- 
  ggplot(data = resp_data, aes(x = x_cal, y = predicted, colour = group)) +
  geom_line(linewidth = 1.2) + 
  geom_point(data = sem_data_plotting, aes(x = value_cal, y = birdabundance, colour = group), 
             size = .9, alpha = 0.5, position = "jitter") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.15, colour = NA) +  # Fill ribbons
  scale_colour_manual(name = "Guild", values = colour_palette) +  # Use custom colours
  scale_fill_manual(name = "Guild", values = colour_palette) +  # Use custom fill colours
  labs(x = NULL, y = "Predicted Bird Abundance") +
  theme_half_open(font_size = 12) +  
  theme(
    legend.position = "bottom", #c(0.5, 1),#-0.34),
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title.align = 0.5,
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 10),
    panel.spacing.x = unit(0.06, "npc"),
    panel.spacing.y = unit(0.02, "npc"),
    plot.margin = margin(t = 5, r = 10, b = 5, #24, 
                         l = 10)
  ) +
  facet_wrap(~predictor, nrow = 2, scale = "free_x",
             strip.position = "bottom",
             labeller = as_labeller(c(
               "icemelt" = "Ice Melt\n(Day of Year)", 
               "budburst" = "Budburst\n(Day of Year)",
               "breedingtemp" = "Breeding Temperature\n(Degrees C)",
               "snowmelt" = "Snowmelt\n(Day of Year)"))) +
  coord_cartesian(ylim = c(0, ylim_threshold)) 

ggsave("outputs/sem/plot/bird_abundance_preds_2x2_09mar2025.png", bird_plot_2x2, width = 8, height = 5, dpi = 600)

#---------------------------
# SEM Figure
#---------------------------
bsem_plotting <- bsem_processed %>%
  left_join(predicted_effect_sizes %>% 
              select(predictor, group, slope), 
            by = c("predictor", "interaction" = "group")) %>%
  mutate(new.estimate = if_else(!is.na(slope), slope, new.estimate)) %>%
  select(-slope)

create_sem_plot <- function(bsem_plotting = bsem_plotting, file_modifier = "") {
  
  # Create model specification string
  model_lines <- bsem_plotting %>% 
    filter(predictor != "Intercept") %>%
    group_by(response_single) %>%
    summarize(line = paste0(response_single, " ~ ", 
                            paste(paste0(new.estimate, "*", predictor), collapse = " + "))) %>%
    pull(line) %>%
    unique()
  
  # Define response order and reorder model lines
  response_order <- c("passerineabundance", "shorebirdabundance", "waterbirdabundance", 
                      "budburst", "snowmelt", "breedingtemp", "icemelt")
  responses <- factor(sapply(model_lines, function(x) strsplit(x, " ~ ")[[1]][1]), 
                      levels = response_order)
  model_lines <- model_lines[order(responses)]
  
  # Combine all lines into a single model specification string
  lavaan_model <- paste(model_lines, collapse = "\n")
  cat(lavaan_model)
  
  # Create labeled covariance matrix
  cov_matrix <- diag(8)
  colnames(cov_matrix) <- rownames(cov_matrix) <- c("passerineabundance", "shorebirdabundance", 
                                                    "waterbirdabundance", "icemelt", "breedingtemp", 
                                                    "snowmelt", "budburst", "regiontemp")
  
  # Create a fake model with specified path coefficients
  fit <- lavaan(model = lavaan_model, sample.cov = cov_matrix, sample.nobs = 100, 
                fixed.x = TRUE, do.fit = FALSE)
  
  # Define a custom layout for the SEM plot
  # custom_layout <- matrix(c(
  #   4.5, -8, 
  #   9, -10.5, 
  #   0, -5.5, 
  #   0, -2, 
  #   3, -2, 
  #   6, -2, 
  #   9, -2, 
  #   4.5, 2.5
  # ), ncol = 2, byrow = TRUE)
  custom_layout <- matrix(c(
    0, -5, 
    4.5, -7, 
    9, -9, 
    9, -2, 
    6, -2, 
    3, -2, 
    0, -2, 
    4.5, 2.4
  ), ncol = 2, byrow = TRUE)
  
  
  # Define full names for each variable
  full_names <- c(
    "Passerine\nAbundance" = "passerineabundance",
    "Shorebird\nAbundance" = "shorebirdabundance",
    "Waterbird\nAbundance" = "waterbirdabundance",
    "Budburst" = "budburst",
    "Snowmelt" = "snowmelt",
    "Breeding\nTemperature" = "breedingtemp",
    "Ice Melt" = "icemelt",
    "Regional\nTemperature" = "regiontemp"
  )
  
  # Extract path coefficients and assign edge colors
  path_coefs <- parameterEstimates(fit)
  non_covariate_params <- path_coefs[path_coefs$op != "~~", ] %>%
    left_join(bsem_plotting, by = c("lhs" = "response_single", "rhs" = "predictor")) %>%
    group_by(rhs) %>%
    mutate(
      refrence.u95 = ifelse(!is.na(interaction) & interaction %in% c("passerine", "shorebird", "waterbird"), 
                            u95.ci[interaction == "passerine"][1], u95.ci),
      refrence.l95 = ifelse(!is.na(interaction) & interaction %in% c("passerine", "shorebird", "waterbird"), 
                            l95.ci[interaction == "passerine"][1], l95.ci)
    ) %>%
    ungroup() %>%
    # mutate(
    #   edge_color = case_when(
    #     # Red: Both conditions met
    #     refrence.l95 < 0 & refrence.u95 < 0 & l95.ci < 0 & u95.ci > 0 ~ "red",
    #     
    #     # Blue: Both conditions met
    #     refrence.l95 > 0 & refrence.u95 > 0 & l95.ci < 0 & u95.ci > 0 ~ "blue",
    #     
    #     # Pale Red: Crossing zero with est < 0
    #     refrence.l95 < 0 & refrence.u95 > 0 & est < 0 ~ scales::alpha("red", 0.25),
    #     
    #     # Pale Blue: Crossing zero with est > 0
    #     refrence.l95 < 0 & refrence.u95 > 0 & est > 0 ~ scales::alpha("blue", 0.25),
    #     
    #     # Grey: Crossing zero
    #     refrence.l95 < 0 & refrence.u95 > 0 ~ "grey",
    #     
    #     # Black: Default
    #     TRUE ~ "black"
    #     # refrence.l95 < 0 & refrence.u95 > 0 ~ "grey",
    #     # refrence.l95 > 0 & refrence.u95 > 0 ~ "blue",
    #     # refrence.l95 < 0 & refrence.u95 < 0 ~ "red",
    #     # # l95.ci < 0 & u95.ci > 0 ~ "grey",
    #     # # l95.ci > 0 & u95.ci > 0 ~ "blue",
    #     # # l95.ci < 0 & u95.ci < 0 ~ "red",
    #     # TRUE ~ "black"
    #   )
    mutate(
      edge_color = case_when(
        # Full conditions for birdabundance
        (response == "birdabundance" & interaction != "passerine") ~ case_when(
          # Red: Both conditions met
          (refrence.l95 < 0 & refrence.u95 < 0) & (l95.ci < 0 & u95.ci > 0) ~ "red",
          
          # Blue: Both conditions met
          (refrence.l95 > 0 & refrence.u95 > 0) & (l95.ci < 0 & u95.ci > 0) ~ "blue",
          
          # Pale Red: Crossing zero with est < 0
          (refrence.l95 < 0 & refrence.u95 > 0) & est < 0 ~ scales::alpha("coral", 0.3),
          
          # Pale Blue: Crossing zero with est > 0
          (refrence.l95 < 0 & refrence.u95 > 0) & est > 0 ~ scales::alpha("lightblue", 0.5),
          # 
          # # Grey: Crossing zero
          # refrence.l95 < 0 & refrence.u95 > 0 ~ "grey",
          # 
          # # Black: Default for birdabundance
          # TRUE ~ "black"
        ),
        
        # Simplified conditions for all other responses
        TRUE ~ case_when(
          # Red: Confidence intervals below zero
          l95.ci < 0 & u95.ci < 0 ~ "red",
          
          # Blue: Confidence intervals above zero
          l95.ci > 0 & u95.ci > 0 ~ "blue",
          
          # Pale Red: Crossing zero with est < 0
          l95.ci < 0 & u95.ci > 0 & est < 0 ~ scales::alpha("coral", 0.3),
          
          # Pale Blue: Crossing zero with est > 0
          l95.ci < 0 & u95.ci > 0 & est > 0 ~ scales::alpha("lightblue", 0.5),
          # 
          # # Grey: Crossing zero
          # l95.ci < 0 & u95.ci > 0 ~ "grey",
          # 
          # # Black: Default for others
          # TRUE ~ "black"
        )
      )
    )
  
  edge_colors <- non_covariate_params$edge_color
  edge_labels <- ifelse(
  non_covariate_params$response == "birdabundance",
  sprintf("%.3f", non_covariate_params$est),
  sprintf("%.3f \n[%.2f, %.2f]",
          non_covariate_params$est,
          non_covariate_params$l95.ci,
          non_covariate_params$u95.ci)
        )
  
  node_categories <- c(
    "Passerine Abundance" = "passerine",
    "Shorebird Abundance" = "shorebird",
    "Waterbird Abundance" = "waterbird",
    "Budburst" = "budburst",
    "Snowmelt" = "snowmelt",
    "Breeding Temperature" = "breeding_temp",
    "Icemelt" = "icemelt",
    "Regional Temperature" = "regional_temp"
  )
  
  node_colors <- colour_palette[node_categories]
  node_colors <- adjustcolor(node_colors, alpha.f = 0.65)
  
  
  par(family = "Times New Roman")
  # Plot and save SEM path diagram
  sem_plot <- semPaths(
    fit,
    what = "par",
    edgeLabels = edge_labels,
    edge.label.margin = 0.01,
    edge.label.color = "black",
    edge.label.bg = "white",
    layout = custom_layout,
    edge.label.cex = 0.8,
    nodeLabels = names(full_names),
    label.cex = 1.15,
    label.font = 1,
    edge.label.font = 1,
    color = node_colors,
    sizeMan = 9,
    edge.color = edge_colors,
    style = "lisrel",
    curve = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.65, 0, 0, 0, 0, 0, 0),
    fade = FALSE,
    residuals = FALSE,
    rescale = TRUE,
    fixedStyle = 1,
    bg = "transparent",
    filetype = "png",
    filename = paste0("outputs/sem/plot/birdabundance_semplot_09apr2025", file_modifier), 
    width = 6, height = 5,
    mar = c(1.25, 1.25, 1.25, 1.25),   # <-- no outer margin
    title = FALSE
  )
}


create_sem_plot(bsem_plotting, file_modifier = "")
