

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

####
guild_bsem_effect <- guild_bsem %>%
  process_fixed_effects() %>%
  add_interaction_title()

intercepts <- guild_bsem_effect %>%
  create_intercepts()

guild_bsem_effect <- guild_bsem_effect %>%
  add_intercepts(intercepts)

guild_bsem_interacted <- guild_bsem_effect %>%
  address_interaction() %>%
  select(response, predictor, interaction,
         new.estimate, new.est.error, new.l95.ci, new.u95.ci, parameter, 
         intercept, intercept_parameter, intercept_error, intercept_l95, intercept_u95,
         estimate, est.error, l95.ci, u95.ci, rhat, bulk.ess, tail.ess)











######################
model_summary <- summary(guild_bsem)

fixed_effects <- model_summary$fixed %>%
  mutate(coef = rownames(.)) %>%
  select(coef, Estimate, Est.Error, `l-95% CI`, `u-95% CI`) 

guild_bsem_effect

#%>% filter(!grepl(":guild", coef))  # Remove interaction

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


#######################

param_df <- data.frame(
  dependent = c("passerineabundance", "passerineabundance", "passerineabundance", "passerineabundance",
                "shorebirdabundance", "shorebirdabundance", "shorebirdabundance", "shorebirdabundance",
                "waterbirdabundance", "waterbirdabundance", "waterbirdabundance", "waterbirdabundance",
                "budburst", "budburst", "budburst",
                "snowmelt", "snowmelt",
                "breedingtemp", "breedingtemp",
                "icemelt"),
  independent = c("icemelt", "breedingtemp", "snowmelt", "budburst",
                  "icemelt", "breedingtemp", "snowmelt", "budburst",
                  "icemelt", "breedingtemp", "snowmelt", "budburst",
                  "snowmelt", "breedingtemp", "regiontemp",
                  "breedingtemp", "regiontemp",
                  "icemelt", "regiontemp",
                  "regiontemp"),
  coefficient = c(-1, 0.3, 0.2, 0.4,
                  0.6, -0.2, 0.1, 0.3,
                  0.4, 0.5, 0.2, 0.3,
                  0.6, 0.5, 0.7,
                  0.5, 0.6,
                  0.4, 0.3,
                  0.3),
  stringsAsFactors = FALSE
)

# Load dplyr (or use base R) to construct the model specification
library(dplyr)

# Group by the dependent variable and paste the predictors together
model_lines <- param_df %>%
  group_by(dependent) %>%
  summarize(line = paste0(dependent, " ~ ", 
                          paste(paste0(coefficient, "*", independent), collapse = " + "))) %>%
  pull(line)

# Remove duplicate lines
model_lines <- unique(model_lines)

# Combine all lines into a single model specification string
lavaan_model <- paste(model_lines, collapse = "\n")

# (Optional) Print your model specification to check
cat(lavaan_model)
