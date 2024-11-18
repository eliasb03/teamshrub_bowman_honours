#------------------------------
# teamshrub_bowman_honours
# 0X_BBS_guilds_v0
# By: Elias Bowman 
# Created: 2024-11-17
#
# Description: This script will summarize the Breeding Bird Data into guild levels
# Should only be run after BBS_tidy
#------------------------------
# Preparing packages and data ####
# Importing data
bbs.summary <- read.csv("data/clean/bbs/bbs_yearly_summary.csv")
species.list <- read.csv("data/clean/bbs/species_list.csv")

# Loading Packages


# Filtering by Cameron-Approved-Species List ####
bbs.summary <- filter(bbs.summary, spec.code %in% species.list$spec.code)

# Creating Yearly Relative Abundance Metric ####
bbs.summary <- bbs.summary %>%
  group_by(year, spec.code) %>%
  mutate(yearly_rel_abundance = total.count / sum(total.count)) %>%
  ungroup()

# probably need a species specific relative abundance or guild specific - or after filtering maybe... hmmmmmmm

# Creating Guild Level Summary




# 
# # View the filtered data
# head(filtered_bbs_summary)
# 
# 
# ###############
# # Create top species versions
# top_spec_num <- 15
# bbs.long.spec.top <- select_top_species(bbs.long, top_spec_num)
# 
# 
# 
# # removing unecessary objects from the environment
# rm(top_spec_num)

