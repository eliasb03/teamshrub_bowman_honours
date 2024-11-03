#------------------------------
# teamshrub_bowman_honours
# 0X_breeding_data_v0
# By: Elias Bowman 
# Created: 2024-11-02
# 
# Description: This script will create a dataset of all breeding records from the Breeding Bird and Ebird Datasets on Qikiqtaruk - Herschel Island
#
#----------------------------s--

# Load necessary packages
library(dplyr)
library(stringr)
library(purrr)

# list of nesting related words (ex. "nest")
nest_words <- c("nest", "nests", "nesting", "egg", "eggs", "chick", "chicks", "fledge", 
                "fledgling", "fledglings", "hatch", "hatching", "hatchling", "hatchlings", 
                "brood", "broods", "nestling", "nestlings", "nestsite", "breed", "breeding", "baby", "brood", "clutch", "mating", "mate", "incubating", "incubate", "flush", "flushed")
# selecting for rows in dataframe to check for nesting words 
columns_to_check <- c("notes", "behaviour", "breed")

ebird_relevant_columns <- c(
  "observation.date",
  "common.name",
  "scientific.name",
  "observation.count",
  "breeding.code",
  "breeding.category",
  "breeding.possibility",
  "behavior.code",
  "age.sex",
  "species.comments",
  "locality",
  "duration.minutes",
  "protocol.type"
)

# Define breeding categories based on Atlas codes
breeding_categories <- function(code) {
  if (code %in% c("H", "S", "P", "T", "C", "N", "A", "B")) {
    return("Possible")
  } else if (code %in% c("S7", "M", "P")) {
    return("Probable")
  } else if (code %in% c("PE", "CN", "NB", "DD", "UN", "ON", "FL", "CF", "FY", "FS", "NE", "NY")) {
    return("Confirmed")
  } else {
    return(NA)  # For codes that don't fit any category
  }
}

# check in bbs.survey for occurences of nesting words
bbs.breeders <- bbs.survey %>%
  filter(if_any(all_of(columns_to_check), ~ str_detect(tolower(.), paste(nest_words, collapse = "|"))))


# Check in ebird for occurrences of nesting
# Filter the dataset
ebird.possible.breeders <- qhi_ebird_df  %>%
  filter(
    str_detect(age.sex, paste(nest_words, collapse = "|")) |           # Check age.sex for nest words
      str_detect(trip.comments, paste(nest_words, collapse = "|")) |     # Check trip.comments for nest words
      str_detect(species.comments, paste(nest_words, collapse = "|")) |   # Check species.comments for nest words
      (!is.na(breeding.code) & breeding.code != "F" & breeding.code != "")                      # Keep valid breeding codes (not NA and not "F")
  ) %>%
  mutate(breeding.possibility = sapply(breeding.code, breeding_categories))

ebird.breeders <- ebird.possible.breeders %>%
  filter(breeding.possibility == "Confirmed")  %>% # Keep only confirmed breeders
  select(all_of(ebird_relevant_columns)) %>%        # Select relevant columns
  mutate(observation.id = row_number())              # Add observation.id column

head(ebird.breeders)

# # count the number of occurences of each breeding code
# ebird.breeders %>%
#   group_by(breeding.code) %>%
#   summarise(count = n(), .groups = 'drop') %>%
#   arrange(desc(count))

ebird.breeders <- ebird.breeders %>%
  distinct(observation.date, species.comments, .keep_all = TRUE) %>%
  mutate(juvenile_count = sapply(str_extract_all(age.sex, "Juvenile \\((\\d+)\\)"), 
                                 function(x) {
                                   if (length(x) > 0) {
                                     # Extract all juvenile counts and convert them to numeric
                                     juvenile_numbers <- str_extract(x, "\\d+")
                                     # Return the sum of juvenile counts, handling NA
                                     sum(as.numeric(juvenile_numbers), na.rm = TRUE)
                                   } else {
                                     0 # Return 0 if there are no juvenile entries
                                   }
                                 })) %>%
  mutate(
    egg_count = sapply(species.comments, function(comment) {
      # Use regex to find numbers that appear before "egg(s)"
      counts <- str_extract_all(comment, "\\d+\\s*(?=\\s*egg|\\s*eggs)", simplify = TRUE)
      # If we found any counts, convert to numeric and sum; otherwise, return 0
      if (length(counts) > 0) {
        sum(as.numeric(counts), na.rm = TRUE)
      } else {
        0 # Return 0 if no counts are found
      }
    }),
    
    chick_count = sapply(species.comments, function(comment) {
      # Use regex to find numbers that appear before "chick(s)"
      counts <- str_extract_all(comment, "\\d+\\s*(?=\\s*chick|\\s*chicks)", simplify = TRUE)
      # If we found any counts, convert to numeric and sum; otherwise, return 0
      if (length(counts) > 0) {
        sum(as.numeric(counts), na.rm = TRUE)
      } else {
        0 # Return 0 if no counts are found
      }
    })
  )


# # Function to extract counts of eggs, chicks, and juveniles from comments and age.sex
# extract_counts <- function(comments, age.sex) {
#   # Extract egg and chick counts from comments
#   egg_count <- str_extract(comments, "\\d+\\s*egg") %>% str_extract("\\d+") %>% as.numeric()
#   chick_count <- str_extract(comments, "\\d+\\s*chick") %>% str_extract("\\d+") %>% as.numeric()
#   
#   # Extract juvenile count from age.sex
#   juvenile_count <- str_extract(age.sex, "\\d+\\s*Juvenile") %>% str_extract("\\d+") %>% as.numeric()
#   
#   return(c(egg_count, chick_count, juvenile_count))
# }
# 
# # Create the new dataset with nesting phases
# nesting_data <- ebird.breeders %>%
#   mutate(
#     # Determine nesting phase based on comments and breeding possibility
#     nesting_phase = case_when(
#       str_detect(species.comments, "building|nesting") ~ "Building nest/nesting",
#       str_detect(species.comments, "no eggs") ~ "Nest with no eggs",
#       str_detect(species.comments, "egg") & !str_detect(species.comments, "chick") ~ "Nest with eggs",
#       str_detect(species.comments, "eggs and chicks") ~ "Nest with eggs and chicks",
#       str_detect(species.comments, "chick") ~ "Nest with chicks",
#       TRUE ~ "Empty nest"  # Default case for any other comments
#     ),
#     # Extract counts of eggs, chicks, and juveniles
#     counts = map2(species.comments, age.sex, extract_counts),
#     egg_count = sapply(counts, function(x) x[1]),
#     chick_count = sapply(counts, function(x) x[2]),
#     juvenile_count = sapply(counts, function(x) x[3])
#   ) %>%
#   select(common.name, observation.date, nesting_phase, egg_count, chick_count, juvenile_count)  # Select relevant columns


