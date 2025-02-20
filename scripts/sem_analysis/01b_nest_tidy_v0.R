#------------------------------
# teamshrub_bowman_honours
# 0X_nests_tidy_v0
# By: Elias Bowman 
# Created: 2025-01-21
#
# Description: This script imports, tidies, and summarizes the annotated breeding bird nest data for the Qikiqtaruk - Herschel Island site. This script will import and tidy the very messy dataset of BBS and Ebird data that Elias annotated
#------------------------------

# Load necessary libraries
library(tidyverse)

# Importing datasets
nest_data_path <- "data/clean/nest/breeding_annotated_21Jan2025.csv"
nest_data <- read_csv(nest_data_path)

#View(nest_data)

nest.factors <- c("nest", "nest_w_eggs", "nest_w_eggs_chicks", "nest_w_chicks")

# Select for correct columns and filter for relavant observations
nest <- nest_data %>% 
  mutate(problem = paste(problematic, `unclear how many eggs`, sep = " "),
         year = year(observation.date)) %>%
  select(observation.date, year, common.name, spec.code, phenological.stage, doy,juvenile.count, egg.count, problem, survey.id, sampling.effort) %>%
  mutate(problem_yn = if_else(problem == "NA NA", FALSE, TRUE)) %>%
  filter(!is.na(phenological.stage) | phenological.stage != "NA") %>%
  filter(phenological.stage %in% nest.factors) %>% # Select for and recode phenological stages to factors
  mutate(phenological.stage = factor(phenological.stage, levels=nest.factors)) 

# Filter and Combine species
species_to_keep <- c(
  "Glaucous Gull",
  "Semipalmated Plover",
  "Common Eider",
  "Lapland Longspur",
  "Sandhill Crane",
  "Red-throated Loon",
  "Snow Bunting",
  #"Red-necked Phalarope",
  #"Long-tailed Jaeger",
  "Semipalmated Sandpiper",
  #"Short-eared Owl",
  "Baird's Sandpiper",
  "Savannah Sparrow",
  #"redpoll sp.",
  "Northern Harrier",
  #"Common Redpoll",
  "Green-winged Teal",
  "American Pipit",
  "Bluethroat",
  #"peep sp.",
  "Black Guillemot",
  "Horned Lark",
  "Rough-legged Hawk",
  "Snowy Owl",
  "American Golden-Plover",
  #"Ruddy Turnstone",
  #"Rock Ptarmigan",
  "Peregrine Falcon",
  #"Tundra Swan",
  "Northern Pintail",
  "Least Sandpiper",
  #"Parasitic Jaeger",
  #"Northern Wheatear",
  #"White-crowned Sparrow",
  "Greater White-fronted Goose",
  "Canada Goose"
)



# 
# nest <- nest %>%
#   mutate(common.name = recode(common.name, 
#                               "Common Redpoll" = "Redpoll",
#                               "redpoll sp." = "Redpoll",
#                               "eider sp." = "Common Eider",
#                               "Parasitic Jaeger" = "Jaeger",
#                               "Long-tailed Jaeger" = "Jaeger",
#                               )) %>%
#   filter(common.name %in% species_to_keep)

# nest <- capitalize_species_code(nest)

# Create the species code reference dataset
species_code_reference <- tibble(
  common.name = c("Glaucous Gull", "Semipalmated Plover", "Common Eider", 
                  "Lapland Longspur", "Sandhill Crane", "Red-throated Loon", 
                  "Snow Bunting", "Semipalmated Sandpiper", "Baird's Sandpiper", 
                  "Savannah Sparrow", "Northern Harrier", "Green-winged Teal", 
                  "American Pipit", "Bluethroat", "Black Guillemot", "Horned Lark", 
                  "Rough-legged Hawk", "Snowy Owl", "American Golden-Plover", 
                  "Peregrine Falcon", "Northern Pintail", "Least Sandpiper", 
                  "Greater White-fronted Goose", "Canada Goose"),
  spec.code = c("GLGU", "SEPL", "COEI", "LALO", "SACR", "RTLO", "SNBU","SESA",
                "BASA", "SAVS", "NOHA", "GWTE", "AMPI", "BLUE", "BLGU", "HOLA", 
                "RLHA", "SNOW", "AMGP", "PEFA", "NOPI", "LESA", "GWFG", "CAGO")) %>%
  mutate(species = common.name) 

# Importing guild mapping dataset
guild.mapping.path <- "data/raw/bird_guild_mapping.csv"
guild.mapping <- read.csv(guild.mapping.path)

# Capitalize species codes
capitalize_species_code <- function(df) {
  df %>%
    mutate(spec.code = toupper(spec.code))
}

# Filter for key species
filter_for_key_species <- function(df, species_to_keep) {
  df %>%
    filter(common.name %in% species_to_keep)
}

# Join common names by species code
join_common_names <- function(data, species_code_reference, use_species = FALSE) {
  if(use_species) {
    data <- data %>%
      left_join(species_code_reference, by = "species", suffix = c("", ".ref")) 
    #%>% select(-species.ref)
  }
  
  data %>%
    left_join(species_code_reference, by = "common.name", suffix = c("", ".ref")) %>%
    mutate(spec.code = coalesce(spec.code, spec.code.ref)) %>%  # Fill missing spec.code
    select(-spec.code.ref)
}

# nest <- nest %>%
#   left_join(species_code_reference, by = "common.name", suffix = c("", ".ref")) %>%
#   mutate(spec.code = coalesce(spec.code, spec.code.ref)) %>%  # Fill missing spec.code
#   select(-spec.code.ref)

# Join guilds

apply_guilds <- function(data, guild.mapping) {
  
  if ("guild" %in% colnames(data) & "guild2" %in% colnames(data)) {
    data <- data %>%
      select(-guild, -guild2)  # Remove exisiting 'guild1' and 'guild2'
  }  
  
  # Check if 'species' column already exists in the data
  if ("species" %in% colnames(data)) {
    # If species exists, just join and resolve any conflicts
    data <- data %>%
      left_join(guild.mapping, by = "spec.code") %>%
      mutate(species = coalesce(species.x, species.y)) %>%  # Resolve species conflicts
      select(-species.x, -species.y)  # Remove the extra columns
  } else {
    # If species doesn't exist, directly add the species column from guild.mapping
    data <- data %>%
      left_join(guild.mapping, by = "spec.code")
  }
  
  return(data)
}

nest <- nest %>% 
  capitalize_species_code() %>%
  filter_for_key_species(species_to_keep) %>% 
  join_common_names(species_code_reference) %>%
  apply_guilds(guild.mapping)


# Recode specific common names and combine relevant species
nest <- nest %>%
  mutate(common.name = recode(common.name, 
                              "Common Redpoll" = "Redpoll",
                              "redpoll sp." = "Redpoll",
                              "eider sp." = "Common Eider",
                              "Parasitic Jaeger" = "Jaeger",
                              "Long-tailed Jaeger" = "Jaeger",
  ))

# Plot observations divided by species

# Create single species datasets to see the coverage
species_year_data <- nest %>%
  group_by(species, year) %>% 
  summarize(
    total_records = n(),                      # Count of records
    unique_nesting_phases = n_distinct(phenological.stage),  # Count of unique nesting phases
    first_observation = min(observation.date), # Earliest observation date
    last_observation = max(observation.date),  # Latest observation date
    .groups = "drop"                          # Ungroup after summarizing
  ) %>%
# Group by species and year
  filter(unique_nesting_phases > 0) %>%                     # Keep groups with at least one record (ensures species was observed in that year)
  ungroup() 

species_year_data <- species_year_data %>%
  join_common_names(species_code_reference, use_species = TRUE) %>%
  apply_guilds(guild.mapping)%>%
  select(-species.ref, -select, -comments)

# Count the number of observation of each species
species_observation_count <- nest %>%
  count(species) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)

# Filter observations based on top 5 species
top_species <- species_observation_count$species





guild_year_data <- nest %>%
  group_by(guild, year) %>% 
  summarize(
    total_records = n(),                      # Count of records
    unique_nesting_phases = n_distinct(phenological.stage),  # Count of unique nesting phases
    first_observation = min(observation.date), # Earliest observation date
    last_observation = max(observation.date),  # Latest observation date
    .groups = "drop"                          # Ungroup after summarizing
  ) %>%
  # Group by species and year
  filter(unique_nesting_phases > 1) %>%                     # Keep groups with at least one record (ensures species was observed in that year)
  ungroup() 



# Plot the number of records per species per year
species_year_data %>%
  ggplot(aes(x = year, y = species, size = total_records, color = unique_nesting_phases)) +
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "Number of records per species per year",
    subtitle = "Size of points represents the number of records, color represents the number of unique nesting phases observed",
    x = "Year",
    y = "Species",
    size = "Total records",
    color = "Unique nesting phases"
  )


species_year_data %>%
  ggplot(aes(x = year, y = species, size = total_records, color = unique_nesting_phases)) +
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "Number of records per species per year",
    subtitle = "Size of points represents the number of records, color represents the number of unique nesting phases observed",
    x = "Year",
    y = "Species",
    size = "Total records",
    color = "Unique nesting phases"
  )


# install rNest from GitHub
library(devtools)
install_github("frousseu/rNest")
library(rNest)

install.packages("remotes")
remotes::install_github("frousseu/rNest", dependencies = FALSE)
