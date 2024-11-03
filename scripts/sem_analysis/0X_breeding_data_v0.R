#------------------------------
# teamshrub_bowman_honours
# 0X_breeding_data_v0
# By: Elias Bowman 
# Created: 2024-11-02
# 
# Description: This script will create a dataset of all breeding records from the Breeding Bird and Ebird Datasets on Qikiqtaruk - Herschel Island
#
#----------------------------s--

# list of nesting related words (ex. "nest")
nest_words <- c("nest", "nests", "nesting", "egg", "eggs", "chick", "chicks", "fledge", 
                "fledgling", "fledglings", "hatch", "hatching", "hatchling", "hatchlings", 
                "brood", "broods", "nestling", "nestlings", "nestsite", "breed", "breeding", "baby", "brood", "clutch", "mating", "mate", "incubating", "incubate", "flush", "flushed")
# selecting for rows in dataframe to check for nesting words 
columns_to_check <- c("notes", "behaviour", "breed")

# check in bbs.survey for occurences of nesting words
bbs.breeders <- bbs.survey %>%
  filter(if_any(all_of(columns_to_check), ~ str_detect(tolower(.), paste(nest_words, collapse = "|"))))


# Check in ebird for occurrences of nesting
# Filter the dataset
ebird.breeders <- qhi_ebird_df  %>%
  filter(
    str_detect(age.sex, paste(nest_words, collapse = "|")) |           # Check age.sex for nest words
      str_detect(trip.comments, paste(nest_words, collapse = "|")) |     # Check trip.comments for nest words
      str_detect(species.comments, paste(nest_words, collapse = "|")) |   # Check species.comments for nest words
      (!is.na(breeding.code) & breeding.code != "F" & breeding.code != "")                      # Keep valid breeding codes (not NA and not "F")
  )


