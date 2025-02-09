#------------------------------
# teamshrub_bowman_honours
# nest_data_prep_v1
# By: Elias Bowman 
# Created: 2024-11-02
# 
# Description: This script compiles breeding records from the Breeding Bird Survey (BBS) 
# and eBird datasets for Qikiqtaruk - Herschel Island, filtering and categorizing breeding-related observations, and saves a combined dataset ready to be manually annotated
# The resultant dataset is then manually annotated to create a list of dates and phenophases of breeding records for different species  
#------------------------------

# Load necessary libraries
library(dplyr)
library(stringr)
library(purrr)

# Define keywords indicating nesting or breeding behavior
nest_words <- c("nest", "nests", "nesting", "egg", "eggs", "chick", "chicks", "fledge", 
                "fledgling", "fledglings", "hatch", "hatched", "hatching", "hatchling", "hatchlings", 
                "brood", "broods", "nested",  "nestling", "nestlings", "nestsite", "nest site", "breed", "breeding", 
                "baby", "babies", "clutch", "mating", "mate", "incubating", "incubate", "flush", "flushed")

# Specify columns to check for nest-related keywords in BBS data
columns_to_check <- c("notes", "behaviour", "breed")

# Define relevant columns to retain in the eBird dataset
ebird_relevant_columns <- c(
  "observation.date", "common.name", "scientific.name", "observation.count", 
  "breeding.code", "breeding.category", "breeding.possibility", "behavior.code", 
  "age.sex", "species.comments", "locality", "duration.minutes", "protocol.type"
)

# Define a function to classify breeding categories based on Atlas breeding codes
breeding_categories <- function(code) {
  if (code %in% c("H", "S", "P", "T", "C", "N", "A", "B")) {
    return("Possible")
  } else if (code %in% c("S7", "M", "P")) {
    return("Probable")
  } else if (code %in% c("PE", "CN", "NB", "DD", "UN", "ON", "FL", "CF", "FY", "FS", "NE", "NY")) {
    return("Confirmed")
  } else {
    return(NA)  # For unclassified codes
  }
}

# Filter BBS data for observations with nesting-related words
bbs.breeders <- bbs.survey %>%
  filter(if_any(all_of(columns_to_check), ~ str_detect(tolower(.), paste(nest_words, collapse = "|"))))

# Filter eBird data for observations with nesting-related words or valid breeding codes
ebird.possible.breeders <- qhi_ebird_df %>%
  filter(
    str_detect(age.sex, paste(nest_words, collapse = "|")) |                 # Check age.sex for nest words
      str_detect(trip.comments, paste(nest_words, collapse = "|")) |           # Check trip.comments for nest words
      str_detect(species.comments, paste(nest_words, collapse = "|")) |        # Check species.comments for nest words
      (!is.na(breeding.code) & breeding.code != "F" & breeding.code != "")     # Include valid breeding codes (not NA and not "F")
  ) %>%
  mutate(breeding.possibility = sapply(breeding.code, breeding_categories))  # Classify breeding possibilities

# Filter for confirmed breeders and select relevant columns, adding an observation ID
ebird.breeders <- ebird.possible.breeders %>%
  filter(breeding.possibility == "Confirmed") %>%  # Keep only confirmed breeders
  select(all_of(ebird_relevant_columns)) %>%       # Select relevant columns
  mutate(observation.id = row_number())            # Add unique observation ID

# Remove duplicates based on observation date and species comments
ebird.breeders <- ebird.breeders %>%
  distinct(observation.date, species.comments, .keep_all = TRUE) %>%
  mutate(
    # Calculate juvenile count based on the age.sex column
    juvenile.count = sapply(str_extract_all(age.sex, "Juvenile \\((\\d+)\\)"), function(x) {
      if (length(x) > 0) {
        # Sum juvenile counts if present, handling NA
        sum(as.numeric(str_extract(x, "\\d+")), na.rm = TRUE)
      } else {
        0 # No juvenile entries found
      }
    }),
    
    # Calculate egg count based on species comments
    egg.count = sapply(species.comments, function(comment) {
      counts <- str_extract_all(comment, "\\d+\\s*(?=\\s*egg|\\s*eggs)", simplify = TRUE)
      if (length(counts) > 0) sum(as.numeric(counts), na.rm = TRUE) else 0
    }),
    
    # Add chick counts to juvenile count if juvenile count is initially 0
    juvenile.count = juvenile.count + ifelse(juvenile.count == 0 | is.na(juvenile.count),
                                             sapply(species.comments, function(comment) {
                                               counts <- str_extract_all(comment, "\\d+\\s*(?=\\s*chick|\\s*chicks)", simplify = TRUE)
                                               if (length(counts) > 0) sum(as.numeric(counts), na.rm = TRUE) else 0
                                             }), 
                                             0)
  )

# Rename columns in bbs.breeders to match ebird.breeders for merging
bbs.breeders <- bbs.breeders %>%
  rename(
    observation.date = date.ymd,
    common.name = species,
    observation.count = total,
  ) 

ebird.breeders <- ebird.breeders %>%
  rename(
    notes = species.comments,
    sampling.time = duration.minutes
  ) 

# Define the phenological stages as a factor
phenological_stages <- c("NA", "pre_nesting", "nest_building", "nest", 
                         "nest_w_eggs", "nest_w_eggs_chicks", 
                         "nest_w_chicks", "empty_nest", 
                         "post_nest", "other", "no_nest", "breeding")

to_annotate_order <- c(
  "observation.date",
  "common.name",
  "observation.count",
  "juvenile.count",
  "egg.count",
  "phenological.stage",
  "annotator.notes",
  "problematic",
  "breed",
  "behaviour",
  "notes",
  "age.sex",
  "breeding.code",
  "breeding.category",
  "behavior.code",
  "breeding.possibility",
  "sampling.time",
  "survey.id",
  "doy",
  "spec.code",
  "time",
  "num.observers",
  "sampling.effort",
  "effort.multiplier",
  "observers",
  "date",
  "period",
  "year",
  "month",
  "day",
  "survey.num",
  "transect",
  "start.time",
  "end.time",
  "rec.num",
  "end.time.filled",
  "transect.id",
  "scientific.name",
  "locality",
  "protocol.type"
)

# Combine the two datasets, keeping all columns, addining annotation columns, and reorder columns based on to_annotate_order
combined_breeders_to_annotate <- bind_rows(bbs.breeders, ebird.breeders) %>%
  mutate(
    phenological.stage = factor(NA, levels = phenological_stages),  # Empty factor column for phenological stages
    annotator.notes = NA_character_,                                     # Empty column for annotator comments
    problematic = NA_character_                                     # Empty column for indicating issues with records
  ) %>% 
  select(all_of(to_annotate_order))


# Save the combined dataset to CSV
write.csv(combined_breeders_to_annotate, "data/to_annotate/combined_breeders_to_annotate.csv", row.names = FALSE)

rm(to_annotate_order, phenological_stages, columns_to_check, nest_words, ebird_relevant_columns, breeding_categories)

