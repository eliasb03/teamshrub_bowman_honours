#------------------------------
# teamshrub_bowman_honours
# 08_ARU_validation_v2
# By: Elias Bowman
# Created: 2025-04-09
#
# Description: This script will select X random vocalization detections for each of the focal bird species, within the first 10 ARUs, and return a short 20-second clip from the file where they were detected.
# It also creates a CSV log with the sample file name, species, confidence, and a placeholder column "valid".
#------------------------------

library(data.table)

# Read the data
aru_data <- fread("data/clean/aru/filtered_aru_data.csv")
str(aru_data)

# Filter to include only ARUs with numbers 1-10
aru_data <- aru_data[ as.numeric(sub("ARUQ([0-9]+).*", "\\1", locationID)) %in% 1:10 ]

# Set parameters
common_list <- unique(aru_data$common_name)      # Use common_name for grouping
x_samples <- 10                                  # Number of samples per species
ffmpeg_path <- "C:/Users/elias/Downloads/Program_Setups/ffmpeg-2025-03-31-git-35c091f4b7-essentials_build/ffmpeg-2025-03-31-git-35c091f4b7-essentials_build/bin/ffmpeg.exe"
base_folder <- file.path("data", "validation")

if (!dir.exists(base_folder)) {
  dir.create(base_folder, recursive = TRUE)
}

# Create a data table for logging sample info
log_dt <- data.table(
  sample_filename = character(), 
  species = character(),
  confidence = numeric(),
  valid = character()
)

# Loop through each species (common name)
for(common in common_list) {
  
  # Subset data and randomly pick samples
  common_data <- aru_data[common_name == common]
  sample_data <- common_data[sample(.N, min(x_samples, .N))]
  
  # Create a folder for this species
  species_dir <- file.path(base_folder, gsub(" ", "_", common))
  if (!dir.exists(species_dir)) {
    dir.create(species_dir, recursive = TRUE)
  }
  
  # Process each selected observation
  for (j in seq_len(nrow(sample_data))) {
    this_row <- sample_data[j]
    in_file <- this_row$filepath
    message("Found input file: ", in_file)
    
    # Skip if the input file doesn't exist
    if (!file.exists(in_file)) {
      message("File not found: ", in_file, " -- skipping.")
      next
    }
    
    # Calculate the midpoint and start time (clip = 20 seconds, 10 sec before and after the midpoint)
    midpoint <- (this_row$start + this_row$end) / 2
    snippet_start <- midpoint - 10
    if (snippet_start < 0) snippet_start <- 0
    
    # Create the output filename using the common name, sample number, and confidence (rounded to 2 decimals with "conf" prefix)
    base_in_file <- tools::file_path_sans_ext(basename(in_file))
    confidence_str <- paste0("conf", sprintf("%.2f", this_row$confidence))
    out_filename <- paste0(gsub(" ", "_", common), "_sample", j, "_", confidence_str, "_", base_in_file, ".wav")
    out_file <- file.path(species_dir, out_filename)
    
    # Build and run the ffmpeg command to extract a 20-second clip
    cmd <- sprintf('"%s" -y -ss %f -i "%s" -t 20 "%s"',
                   ffmpeg_path, snippet_start, in_file, out_file)
    message("Running command: ", cmd)
    message("Processing: ", out_filename)
    
    output <- system(cmd, intern = TRUE)
    cat("ffmpeg output:\n", paste(output, collapse = "\n"), "\n")
    
    # Log the output info
    log_dt <- rbind(log_dt, data.table(
      sample_filename = out_filename,
      species = common,
      confidence = this_row$confidence,
      valid = NA_character_
    ))
  }
}

# Save the log as a CSV file in the validation folder
fwrite(log_dt, file.path(base_folder, "validation_log.csv"))
message("Validation log saved at: ", file.path(base_folder, "validation_log.csv"))
