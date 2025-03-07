#------------------------------
# teamshrub_bowman_honours
# 07_bbs_output_v1.R
# By: Elias Bowman 
# Created: 2025-03-03
# 
# Description: This script will construct bbs_sem model outputs
#------------------------------

library(tidyverse)
library(ggeffects)
library(cowplot)
library(dplyr)

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

#### BIG NOTE: NO WAY THE 95% CI ARE CORRECT RN. NEED TO FIX THIS
guild_bsem_process <- guild_bsem %>%
  process_fixed_effects()
  
guild_bsem_effect <- guild_bsem_process %>%
  add_interaction_title()

intercepts <- guild_bsem_effect %>%
  create_intercepts()

#guild_bsem_effect <- guild_bsem_effect %>%
#  add_intercepts(intercepts)

guild_bsem_interacted <- guild_bsem_effect %>%
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
    Response = recode(Response, !!!recode_vars),
    Parameter = recode(Parameter, !!!recode_vars)
  ) %>%
  mutate(Response = factor(Response, levels = c("Bird Abundance", "Ice Melt", "Breeding Temperature", "Snowmelt", "Budburst"))) %>%
  mutate(Parameter = factor(Parameter, levels = c("(Intercept)", "Intercept", "Shorebird", "Waterbird", "Ice Melt", "Icemelt:Shorebird", "Icemelt:Waterbird", "Breeding Temperature","Breeding Temperature:Shorebird", "Breeding Temperature:Waterbird",  "Snowmelt","Snowmelt:Shorebird", "Snowmelt:Waterbird", "Budburst", "Budburst:Shorebird", "Budburst:Waterbird", "Regional Temperature"))) %>%
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
webshot("outputs/sem/plot/temp_table.html", file = "outputs/sem/plot/guild_output_05feb2025_8.png", 
        vwidth = 200, vheight = 160, zoom = 2)  # Adjust the dimensions here



#---------------------------
# Bird Abundance Specific Variable Outputs
#---------------------------
# List of variable names
variables <- c("icemelt", "breedingtemp", "snowmelt", "budburst")

# Get the responses using lapply, including the "guild" term
responses <- lapply(variables, function(var) predict_response(guild_bsem, terms = c(var, "guild")))

# Name the responses with the variable names directly
names(responses) <- variables

# Process and combine data frames using imap_dfr
resp_data <- imap_dfr(responses, ~ as.data.frame(.x) %>%
                        filter(response.level == "birdabundance") %>%
                        mutate(predictor = .y))  # .y is the name of the list element

predicted_effect_sizes <- resp_data %>%
  group_by(predictor, group) %>%
  arrange(abs(x)) %>%
  slice(1:3) %>%
  ungroup() %>%
  group_by(predictor, group) %>%
  summarize(
    min_x = min(x),
    max_x = max(x),
    min_y = predicted[which.min(x)],
    max_y = predicted[which.max(x)],
    slope = (max_y - min_y) / (max_x - min_x),
    mid_y = abs(abs(min_y)+abs(max_y))/2,
    mid_x = (min_x + max_x)/2
  )
  

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

# Recalibrating Data
scaling_params <- read_csv("data/clean/sem/scaling_params.csv") %>%
  rename("predictor" = "variable") #%>%

resp_data <- resp_data %>%
  left_join(scaling_params, by = "predictor")%>%   # Join based on predictor
  mutate(x_cal = (x * scaling_value) + mean)          # Revert centering and scaling 

sem_data_plotting <- sem_data_plotting %>%
  left_join(scaling_params, by = "predictor") %>%   # Join based on predictor
  mutate(value_cal = (value*scaling_value) + mean)  # Add the calibration value to value

effect_size_scaled <- predicted_effect_sizes %>%
  left_join(scaling_params, by = "predictor") %>%   # Join based on predictor
  mutate(slope_cal = (slope/scaling_value))# + mean)  # Add the calibration value to value

# Plot the calibrated data
ggplot(data = resp_data, aes(x = x_cal, y = predicted, colour = group)) +
  geom_line(linewidth = 1) + 
  geom_point(data = sem_data_plotting, aes(x = value_cal, y = birdabundance, colour = group), 
             size = 1, alpha = 0.5, position = "jitter") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.13, colour = NA) +  # Fill ribbons
  scale_colour_viridis_d(name = "Guild",  
                         labels = c("Waterbird", "Passerine", "Shorebird"), 
                         guide = guide_legend(nrow = 1)) +  # Combined legend for colour
  scale_fill_viridis_d(name = "Guild",  
                       labels = c("Waterbird", "Passerine", "Shorebird"), 
                       guide = guide_legend(nrow = 1)) +  # Combined legend for fill
  labs(x = NULL,#"Predictor Variable", 
       y = "Predicted Bird Abundance") +
  theme_half_open(font_size = 12) +  
  theme(
    legend.position = c(0.5, -0.12),         # Bottom center position for legend
    legend.justification = "center",         # Center the legend horizontally
    legend.title.align = 0.5,                # Center the legend title
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 10),
    panel.spacing = unit(0.02, "npc"),
    plot.margin = margin(t = 10, r = 10, b = 24, l = 10)
  ) +
  
  facet_wrap(~predictor, nrow = 2, scale = "free_x",
             strip.position = "bottom",     # Move facet labels to the bottom
             labeller = as_labeller(c("icemelt" = "Ice Melt\n(Day of Year)", 
                                      "budburst" = "Budburst\n(Day of Year)",
                                      "breedingtemp" = "Spring Temperature\n(Degrees C)",
                                      "snowmelt" = "Snowmelt\n(Day of Year)"))) +
  coord_cartesian(ylim = c(0, ylim_threshold)) 
  
#---------------------------
# SEM Figure
#---------------------------
library(lavaan)
library(semPlot)

effect_size_scaled

guild_bsem_interacted <- guild_bsem_interacted %>%
  left_join(effect_size_scaled %>% 
              select(predictor, group, slope), 
            by = c("predictor", "interaction" = "group")) %>%
  mutate(new.estimate = if_else(!is.na(slope), slope, new.estimate)) %>%
  select(-slope)  # Remove the temporary 'mean' column

create_sem_plot <- function(guild_bsem_interacted, file_modifier = "") {
  
  # Create model specification string
  model_lines <- guild_bsem_interacted %>% 
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
  custom_layout <- matrix(c(
    4.5, -8, 9, -10.5, 0, -5.5, 0, -2, 3, -2, 6, -2, 9, -2, 4.5, 2.5
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
    left_join(guild_bsem_interacted, by = c("lhs" = "response_single", "rhs" = "predictor")) %>%
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
  
  #View(non_covariate_params)
  
  edge_colors <- non_covariate_params$edge_color
  edge_labels <- sprintf(
    "%.3f \n[%.2f, %.2f]",
    non_covariate_params$est,
    non_covariate_params$l95.ci,
    non_covariate_params$u95.ci
  )
  
  # Plot and save SEM path diagram
  sem_plot <- semPaths(
    fit,
    what = "par",
    edgeLabels = edge_labels,
    edge.label.margin = 0,
    edge.label.color = "black",
    layout = custom_layout,
    edge.label.cex = 0.5,
    nodeLabels = names(full_names),
    sizeMan = 6,
    edge.color = edge_colors,
    style = "lisrel",
    curve = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1.8, 0, 0, 0, 0, 0, 0),
    fade = FALSE,
    residuals = FALSE,
    rescale = TRUE,
    fixedStyle = 1,
    filetype = "pdf",
    filename = paste0("outputs/sem/plot/birdabundance_semplot_06mar2025", file_modifier), 
    width = 12, height = 15
  )
}

# Run the function
create_sem_plot(guild_bsem_interacted)










############################
#---------------------------
# Old Table Attempt
#---------------------------
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
  # Subtle highlight for negative effects (slightly darker gray, italic text)
  tab_style(
    style = list(
      cell_fill(color = "gainsboro"),
      cell_text(style = "italic")
    ),
    locations = cells_body(rows = `Lower 95% CI` < 0 & `Upper 95% CI` < 0)
  ) %>%
  tab_style(
    style = cell_borders(sides = "top", color = "black", weight = px(3)),
    locations = cells_body(rows = Parameter == "(Intercept)")
  ) %>%
  # Add a grey line above specified parameters
  tab_style(
    style = cell_borders(sides = "top", color = "grey", weight = px(1)),
    locations = cells_body(rows = Parameter %in% c("Ice Melt", "Breeding Temperature", "Snowmelt", "Budburst"))
  ) %>%
  opt_row_striping() 

# Interaction Added Table
# interacted_tbl <- guild_bsem_interacted %>%
#   select(response, predictor, interaction, new.estimate, new.est.error, new.l95.ci, new.u95.ci,
#          rhat, bulk.ess, tail.ess) %>%
#   mutate(across(c(new.estimate, new.est.error, new.l95.ci, new.u95.ci), round, 3)) %>%
#   mutate(across(c(bulk.ess, tail.ess), round, 0)) %>%
#   mutate(across(c(rhat), round, 2)) %>%
#   rename(
#     "Response" = response,
#     "Predictor" = predictor,
#     "Guild" = interaction,
#     "Estimate" = new.estimate,
#     "Est. Error" = new.est.error,
#     "Lower 95% CI" = new.l95.ci,
#     "Upper 95% CI" = new.u95.ci
#   ) %>%
#   # Recode column values based on the defined framework
#   mutate(
#     Response = recode(Response, !!!recode_vars),
#     Predictor = recode(Predictor, !!!recode_vars),
#     Guild = recode(Guild, !!!recode_vars)
#   ) %>%
#   mutate(Response = factor(Response, levels = c("Bird Abundance", "Ice Melt", "Breeding Temperature", "Snowmelt", "Budburst"))) %>%
#   mutate(Predictor = factor(Predictor, levels = c("Intercept", "Ice Melt", "Breeding Temperature", "Snowmelt", "Budburst", "Regional Temperature"))) %>%
#   arrange(Response, Guild, Predictor)
# 
# guild_table <- interacted_tbl %>%
#   gt() %>% 
#   tab_header(
#     title = "Bird Abundance SEM Model Interactions Added (NOTE 95CI BAD)"
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),    # Bold the header text
#     locations = cells_column_labels(everything()) # Apply to all column headers
#   ) %>%
#   tab_style(
#     style = cell_borders(sides = "top", color = "grey", weight = px(2)),  # Bold line above
#     locations = cells_body(
#       rows = Predictor == "Intercept"
#     )
#   ) %>%
#   opt_row_striping()  # Add alternating row colors
# guild_table
# gtsave(guild_table, filename = "outputs/scrap/guild_table_v1.png", vwidth = 1200, vheight = 1400)


# Non-interacted table
# tidy_model <- tidy(guild_bsem, effects = "fixed", conf.int = TRUE, conf.level = 0.95)
# 
# 
# # Convert tidy output to gtsummary table
# guild_tbl <- tidy_model %>%
#   select(response, term, estimate, std.error, conf.low, conf.high) %>%
#   mutate(across(c(estimate, std.error, conf.low, conf.high), round, 3)) %>%
#   mutate(across(c(bulk.ess, tail.ess), round, 0)) %>%
#   mutate(across(c(rhat), round, 2)) %>%
#   rename(
#     "Response" = response,
#     "Parameter" = term,
#     "Estimate" = estimate,
#     "Std. Error" = std.error,
#     "Lower 95% CI" = conf.low,
#     "Upper 95% CI" = conf.high,
#     #"R-hat" = rhat,
#     #"Bulk ESS" = bulk.ess,
#     #"Tail ESS" = tail.ess
#   ) %>% 
#   mutate(
#     Response = recode(Response, !!!recode_vars),
#     Parameter = recode(Parameter, !!!recode_vars)
#   ) %>%
#   mutate(Response = factor(Response, levels = c("Bird Abundance", "Ice Melt", "Breeding Temperature", "Snowmelt", "Budburst"))) %>%
#   mutate(Parameter = factor(Parameter, levels = c("(Intercept)", "Shorebird", "Waterbird", "Ice Melt", "Icemelt:Shorebird", "Icemelt:Waterbird", "Breeding Temperature","Breeding Temperature:Shorebird", "Breeding Temperature:Waterbird",  "Snowmelt","Snowmelt:Shorebird", "Snowmelt:Waterbird", "Budburst", "Budburst:Shorebird", "Budburst:Waterbird", "Regional Temperature"))) %>%
#   arrange(Response, Parameter)
# 
# guild_wint_table <- guild_tbl %>%
#   gt() %>% 
#   tab_header(
#     title = "Bird Abundance SEM Model Output"
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
#   # Subtle highlight for negative effects (slightly darker gray, italic text)
#   tab_style(
#     style = list(
#       cell_fill(color = "gainsboro"),
#       cell_text(style = "italic")
#     ),
#     locations = cells_body(rows = `Lower 95% CI` < 0 & `Upper 95% CI` < 0)
#   ) %>%
#   tab_style(
#     style = cell_borders(sides = "top", color = "black", weight = px(3)),
#     locations = cells_body(rows = Parameter == "(Intercept)")
#   ) %>%
#   # Add a grey line above specified parameters
#   tab_style(
#     style = cell_borders(sides = "top", color = "grey", weight = px(1)),
#     locations = cells_body(rows = Parameter %in% c("Ice Melt", "Breeding Temperature", "Snowmelt", "Budburst"))
#   ) %>%
#   opt_row_striping()  # Add alternating row colors
# 
# guild_wint_table
# gtsave(guild_wint_table, filename = "outputs/scrap/guild_interaction_table_v1.png", vwidth = 1200, vheight = 1400)

#---------------------------
# Old SEM Attempt
#---------------------------

############
# 
# # Group semPlot# Group by the response_single variable and paste the predictors together
# model_lines <- guild_bsem_interacted %>% 
#   filter(predictor != "Intercept") %>%
#   group_by(response_single) %>%
#   summarize(line = paste0(response_single, " ~ ", 
#                           paste(paste0(new.estimate, "*", predictor), collapse = " + "))) %>%
#   pull(line)
# 
# # Remove duplicate lines
# model_lines <- unique(model_lines)
# 
# response_order <- c("passerineabundance", "shorebirdabundance", "waterbirdabundance", "budburst", "snowmelt", "breedingtemp", "icemelt")
# #response_order <- c("passerineabundance", "shorebirdabundance", "waterbirdabundance", "icemelt", "breedingtemp", "snowmelt", "budburst", "regiontemp")
# 
# # Extract the response variable from each line (before the "~")
# responses <- sapply(model_lines, function(x) strsplit(x, " ~ ")[[1]][1])
# 
# # Create a factor to reorder the responses based on the specified order
# responses <- factor(responses, levels = response_order)
# 
# # Reorder the model lines according to the response order
# model_lines <- model_lines[order(responses)]
# 
# # Combine all lines into a single model specification string
# lavaan_model <- paste(model_lines, collapse = "\n")
# 
# # Print your model specification to check
# cat(lavaan_model)
# 
# # Create a labeled covariance matrix for the variables
# cov_matrix <- diag(8)
# colnames(cov_matrix) <- rownames(cov_matrix) <- c("passerineabundance", "shorebirdabundance", "waterbirdabundance", "icemelt", "breedingtemp", "snowmelt", "budburst", "regiontemp")
# 
# # Create a fake model with specified path coefficients using lavaanify
# fit <- lavaan(model = lavaan_model, sample.cov = cov_matrix, sample.nobs = 100, fixed.x = TRUE, do.fit = FALSE)
# 
# # Define a custom layout for the SEM plot
# custom_layout <- matrix(c(
#   4.5, -8,     # passerineabundance
#   9, -10.5,   # shorebirdabundance
#   0, -5.5,     # waterbirdabundance
#   0, -2,     # budburst
#   3, -2,     # snowmelt
#   6, -2,     # breedingtemp
#   9, -2,     # icemelt
#   4.5, 2.5    # regiontemp 
#   # 
#   # # option 1
#   # 6, 4.5,     # passerineabundance
#   # 8, 9,   # shorebirdabundance
#   # 4, 0,     # waterbirdabundance
#   # # # option 2
#   # # 5, 9,     # passerineabundance
#   # # 6.5, 4.5,   # shorebirdabundance
#   # # 5, 0,     # waterbirdabundance
#   # # # option 3
#   # # 5, 9,     # passerineabundance
#   # # 6.5, 4.5,   # shorebirdabundance
#   # # 4, 0,     # waterbirdabundance
#   # # 
#   # 2, 0,     # budburst
#   # 2, 3,     # snowmelt
#   # 2, 6,     # breedingtemp
#   # 2, 9,     # icemelt
#   # -1, 4.5    # regiontemp 
# ), ncol = 2, byrow = TRUE)
# 
# # Define full names for each variable
# full_names <- c(
#   "Passerine\nAbundance" = "passerineabundance",
#   "Shorebird\nAbundance" = "shorebirdabundance",
#   "Waterbird\nAbundance" = "waterbirdabundance",
#   #  "Bird\nAbundance" = "birdabundance",
#   "Budburst" = "budburst",
#   "Snowmelt" = "snowmelt",
#   "Breeding\nTemp" = "breedingtemp",
#   "Ice Melt" = "icemelt",
#   "Region\nTemp" = "regiontemp"
# )
# 
# # Scale the edge widths by the absolute value of the effect sizes (coefficients)
# path_coefs <- parameterEstimates(fit)
# 
# non_covariate_params <- path_coefs[path_coefs$op != "~~", ] %>%
#   left_join(guild_bsem_interacted, by = c("lhs" = "response_single", "rhs" = "predictor"))
# 
# non_covariate_params <- non_covariate_params %>%
#   mutate(
#     edge_color = case_when(
#       new.l95.ci < 0 & new.u95.ci > 0 ~ "grey",  # CI spans zero, grey
#       new.l95.ci > 0 & new.u95.ci > 0 ~ "blue",  # CI is entirely positive, blue
#       new.l95.ci < 0 & new.u95.ci < 0 ~ "red",   # CI is entirely negative, red
#       TRUE ~ "black"  # Default to black if it doesn't fit the above cases
#     )) %>%
#   mutate(
#     edge_color_2 = case_when(
#       est > 0 & new.l95.ci < 0 & new.u95.ci > 0 ~ "lightblue",  # Positive est, CI overlaps zero, pale blue
#       est < 0 & new.l95.ci < 0 & new.u95.ci > 0 ~ "lightcoral",  # Negative est, CI overlaps zero, pale red
#       est > 0 & new.l95.ci > 0 & new.u95.ci > 0 ~ "blue",        # Positive est, CI entirely positive, blue
#       est < 0 & new.l95.ci < 0 & new.u95.ci < 0 ~ "red",          # Negative est, CI entirely negative, red
#       TRUE ~ "black"  # Default to black if it doesn't fit the above cases
#     ))
# 
# edge_colors <- non_covariate_params$edge_color
# #edge_colors <- non_covariate_params$edge_color_2
# 
# edge_labels <- sprintf(
#   "%.3f \n[%.2f, %.2f]",
#   non_covariate_params$est,
#   non_covariate_params$new.l95.ci,
#   non_covariate_params$new.u95.ci
# )
# 
# # Plot using the modified label column
# sem_plot <- semPaths(
#   fit,
#   what = "par",
#   edgeLabels = edge_labels,
#   edge.label.margin = 0,
#   #edge.label.bg = FALSE,
#   edge.label.color = "black",
#   layout = custom_layout,
#   edge.label.cex = 0.5,
#   nodeLabels = names(full_names),
#   sizeMan = 6,
#   #color = "white",
#   edge.color = edge_colors,
#   style = "lisrel",
#   curve = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1.8, 0, 0, 0, 0, 0, 0),
#   fade = FALSE,
#   residuals = FALSE,
#   rescale = TRUE,
#   fixedStyle = 1,
#   filetype = "pdf",
#   filename = "outputs/sem/plot/birdabundance_semplot_05mar2025", width = 12, height = 15
# )



model_summary <- summary(guild_bsem)

fixed_effects <- model_summary$fixed %>%
  mutate(coef = rownames(.)) %>%
  select(coef, Estimate, Est.Error, `l-95% CI`, `u-95% CI`) %>%
  filter(!grepl(":guild", coef))  # Remove interaction

target_coefs <- c(
  "birdabundance_icemelt",
  "birdabundance_breedingtemp",
  "birdabundance_snowmelt",
  "birdabundance_budburst",
  
  "icemelt_regiontemp",
  
  "breedingtemp_icemelt",
  "breedingtemp_regiontemp",
  
  "snowmelt_breedingtemp",
  "snowmelt_regiontemp",
  
  "budburst_snowmelt",
  "budburst_breedingtemp",
  "budburst_regiontemp")

target_paths <- c(
  "Ice Melt → Bird Abundance", 
  "Spring Temperature → Bird Abundance", 
  "Snowmelt → Bird Abundance", 
  "Budburst → Bird Abundance", 
  
  "Region Temperature → Ice Melt",
  
  "Ice Melt → Spring Temperature", 
  "Region Temperature → Spring Temperature",
  
  "Spring Temperature → Snowmelt",
  "Region Temperature → Snowmelt",
  
  "Snowmelt → Budburst", 
  "Spring Temperature → Budburst", 
  "Region Temperature → Budburst"
)

# Create a tibble with the estimates for only these coefficients.
estimates <- fixed_effects %>%
  filter(coef %in% target_coefs) %>%
  # Ensure the correct ordering by matching names:
  mutate(path = target_paths[match(coef, target_coefs)]) %>%
  rename(estimate = Estimate, ci_low = `l-95% CI`, ci_high = `u-95% CI`, est.error = Est.Error) %>%
  mutate(across(c(estimate, ci_low, ci_high, est.error), ~ round(., 3))) %>%
  separate(path, into = c("from", "to"), sep = " → ")

# Define node positions for the plot.
node_coords <- tibble(
  name = c("Ice Melt", "Spring Temperature", "Snowmelt", "Budburst", "Bird Abundance", "Region Temperature"),
  x = c(0, 0, 0, 0, 4.5, -4.5),
  y = c(6, 2, -2, -6, 0, 0)
) 
# Join the node coordinates to get starting and ending positions:
edges <- estimates %>%
  left_join(node_coords, by = c("from" = "name")) %>%
  rename(x_from = x, y_from = y) %>%
  left_join(node_coords, by = c("to" = "name")) %>%
  rename(x_to = x, y_to = y) %>%
  # Compute midpoint for label placement (with a slight vertical offset)
  mutate(x_mid = (x_from + x_to) / 2,
         y_mid = (y_from + y_to) / 2) #+ 0.15) 

node_coords <- node_coords %>% 
  mutate(clean = case_when(
    name == "Ice Melt" ~ "Ice Melt",
    name == "Spring Temperature" ~ "Spring\nTemperature",
    name == "Snowmelt" ~ "Snowmelt",
    name == "Budburst" ~ "Budburst",
    name == "Bird Abundance" ~ "Bird\nAbundance",
    name == "Region Temperature" ~ "Regional\nTemperature",
    TRUE ~ name),
  )

# Plot the graph with modified labels
ggplot() +
  geom_segment(
    data = edges,
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to,
      size = abs(estimate),
      color = ifelse(estimate > 0, "positive", "negative")
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    lineend = "round"
  ) +
  scale_size_continuous(range = c(0.5, 3)) +
  scale_color_manual(values = c("positive" = "blue", "negative" = "red")) +
  geom_label(
    data = edges,
    aes(
      x = x_mid,
      y = y_mid,
      label = ifelse(
        is.na(estimate),
        "",
        paste0(estimate, "\n[", ci_low, ", ", ci_high, "]")
      )
    ),
    size = 4,
    color = "darkred",
    fill = "white",
    label.size = 0,
    vjust = .75
  ) +
  geom_label(data = node_coords,
             aes(x = x, y = y, label = clean),
             fill = "white",#"grey",
             color = "black",
             size = 5,
             label.size = 1,
             label.padding = unit(c(0.4, 0.1, 0.4, 0.1), "cm"),
             label.r = unit(0, "pt")) +
  theme_void() +
  theme(legend.position = "none") +
  xlim(-6, 6) +
  ylim(-6.5, 6.5)



# ggsave("outputs/scrap/sem_plot_test_1.pdf", plot = sem_plot, width = 10, height = 8, units = "in")
# qgraph(sem_plot, file = "outputs/scrap/sem_plot_test_1.pdf", width = 10, height = 8)



####
# edge_colors <- ifelse(path_coefs$est > 0, "blue", "red")  # Blue for positive, red for negative

# # Plot the model with custom colors for positive and negative effects
# semPaths(
#   fit,
#   what = "par",
#   whatLabels = "est",
#   layout = custom_layout,
#   edge.label.cex = 1,
#   nodeLabels = names(full_names),
#   sizeMan = 5,
#   color = "white",
#   edge.color = edge_colors,  # Use custom edge colors
#   style = "lisrel",
#   curve = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, 0, 0),
#   fade = FALSE,
#   residuals = FALSE,
#   fixedStyle = 1
# )
########

##################
# geom_abline(
#   data = effect_size_scaled,
#   aes(intercept = mid_y*scaling_value, slope = slope_cal)
# ) +
# geom_abline(
#   data = effect_size_scaled,
#   aes(intercept = mean/slope, slope = slope_cal)
# )+

# 
# # Plot the calibrated data
# ggplot(data = resp_data, aes(x = x, y = predicted, colour = group)) +
#   geom_line(linewidth = 1) + 
#   geom_point(data = sem_data_plotting, aes(x = value, y = birdabundance, colour = group), 
#              size = 1, alpha = 0.5, position = "jitter") +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
#               alpha = 0.13, colour = NA) +  # Fill ribbons
#   scale_colour_viridis_d(name = "Guild",  
#                          labels = c("Waterbird", "Passerine", "Shorebird"), 
#                          guide = guide_legend(nrow = 1)) +  # Combined legend for colour
#   scale_fill_viridis_d(name = "Guild",  
#                        labels = c("Waterbird", "Passerine", "Shorebird"), 
#                        guide = guide_legend(nrow = 1)) +  # Combined legend for fill
#   labs(x = NULL,#"Predictor Variable", 
#        y = "Predicted Bird Abundance") +
#   theme_half_open(font_size = 12) +  
#   theme(
#     legend.position = c(0.5, -0.12),         # Bottom center position for legend
#     legend.justification = "center",         # Center the legend horizontally
#     legend.title.align = 0.5,                # Center the legend title
#     strip.background = element_blank(),
#     strip.placement = "outside",
#     strip.text = element_text(size = 10),
#     panel.spacing = unit(0.02, "npc"),
#     plot.margin = margin(t = 10, r = 10, b = 24, l = 10)
#   ) + 
#   facet_wrap(~predictor, nrow = 2, scale = "free_x",
#              strip.position = "bottom",     # Move facet labels to the bottom
#              labeller = as_labeller(c("icemelt" = "Ice Melt\n(Day of Year)", 
#                                       "budburst" = "Budburst\n(Day of Year)",
#                                       "breedingtemp" = "Spring Temperature\n(Degrees C)",
#                                       "snowmelt" = "Snowmelt\n(Day of Year)"))) +
#   #coord_cartesian(ylim = c(0, ylim_threshold)) +
#   geom_abline(
#     data = predicted_effect_sizes, 
#     aes(intercept = mid_y, slope = slope, colour = group)
#   )
