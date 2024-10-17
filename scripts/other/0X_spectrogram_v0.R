#------------------------------
# teamshrub_bowman_honours
# 0X_spectrogram_v0
# By: Elias Bowman
# 2024-07-05
# Description: This script will do early work with audio files from ARUs. I want this script to scan through one audio file, create subsamples and visualize it. Then I will need a way to select specific moments of noise and create a spectrogram.
#
#------------------------------

# importing relevant packages
library(tuneR)
library(av)
library(signal) # signal processing functions
library(oce) # image plotting functions and nice color maps
library(ggplot2)
library(tidyverse)

# defining functions
spectro = function(data,# function courtesy of: https://hansenjohnson.org/post/spectrograms-in-r/
                   nfft, # function takes a wave form, nfft, window (win), overlap (ol)
                   win,
                   ol,
                   t0 = 0,
                   plot_spec = T,
                   normalize = F,
                   return_data = F,
                   ...) {
  # extract signal
  signal_with_offset = data@left
  
  # demean to remove DC offset
  signal = signal_with_offset - mean(signal_with_offset)
  
  # determine duration
  dur = length(signal) / data@samp.rate
  
  # create spectrogram
  spec = specgram(
    x = signal,
    n = nfft,
    Fs = data@samp.rate,
    window = win,
    overlap = ol
  )
  
  # discard phase info
  P = abs(spec$S)
  
  # normalize
  if (normalize) {
    P = P / max(P)
  }
  
  # convert to dB
  P = 10 * log10(P)
  
  # config time axis
  if (t0 == 0) {
    t = as.numeric(spec$t)
  } else {
    t = as.POSIXct(spec$t, origin = t0)
  }
  
  # rename freq
  f = spec$f
  
  if (plot_spec) {
    # change plot colour defaults
    par(bg = "black")
    par(col.lab = "white")
    par(col.axis = "white")
    par(col.main = "white")
    
    # plot spectrogram
    imagep(
      t,
      f,
      t(P),
      col = oce.colorsViridis,
      drawPalette = T,
      ylab = 'Frequency [Hz]',
      axes = F,
      ...
    )
    
    
    box(col = 'white')
    axis(2, labels = T, col = 'white')
    
    # add x axis
    if (t0 == 0) {
      axis(1, labels = T, col = 'white')
      
    } else{
      axis.POSIXct(
        seq.POSIXt(t0, t0 + dur, 10),
        side = 1,
        format = '%H:%M:%S',
        col = 'white',
        las = 1
      )
      mtext(
        paste0(format(t0, '%B %d, %Y')),
        side = 1,
        adj = 0,
        line = 2,
        col = 'white'
      )
      
    }
  }
  
  if (return_data) {
    # prep output
    spec = list(t = t,
                f = f,
                p = t(P))
    
    return(spec)
  }
}
create_image_directory_name <- function(input_string) {
  # Extract the first section before the underscore
  first_section <- str_split(input_string, "_")[[1]][1]
  
  # Get the current date and time
  date_time <- format(date_time_full, "%Y%m%d_%H%M%S")
  
  # Combine the first section with the date and time
  dir_name <- paste0(first_section, "_", date_time)
  
  return(dir_name)
}
create_image_name <- function(input_string) {
  # Extract the first section before the underscore
  first_section <- str_split(input_string, "_")[[1]][1]
  
  # Get the current date and time
  date_time <- format(date_time, "%Y%m%d_%H%M%S")
  
  # Combine the first section with the date and time
  image_name <- paste0(first_section, "_", date_time, "_image")
  
  return(image_name)
}

# setting variables
f <- 48000 # sampling frequency

# importing audio files
aru_dir <-
  "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/"

aru_folders <- list.dirs(aru_dir, recursive = FALSE)
aru_folders

for (aru in aru_folders) {
  aru_name <- sub(".*\\/", "", aru)
  audio_dir <- paste0(aru, "/Data/audio")
  audio_files <- list.files(audio_dir)
  
  for (file in audio_files) {
    # audio_file_name <- "ARUQ1_20240622_170000"
    #audio_name <- audio_file_name
    ########
    audio_name <- substr(file, 0, (nchar(file)-4))
    
    audio_file <- file.path(audio_dir, paste0(audio_name, ".wav"))
    audio <- readWave(audio_file) # full 10 minute audio file
    ########
    date_time_full <-
      as.POSIXct(substring(audio_name, (nchar(audio_name) - 14), nchar(audio_name)),
                 format = "%Y%m%d_%H%M%S",
                 tz = "America/Whitehorse")
    #######
    base_img_dir <-
      paste0(
        "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/outputs/spectrograms/", aru_name, "/")
    
    img_folder <- create_image_directory_name(audio_name)
    img_dir <- paste0(base_img_dir, img_folder)
    if (!dir.exists(base_img_dir)) {
      dir.create(base_img_dir)
    }
    if (!dir.exists(img_dir)) {
      dir.create(img_dir)
    }
    
    #######
    for (i in 0:9) {
      start = i * 60
      end = start + 60
      
      
      
      # start_sec = 70
      # end_sec = 90
      audio_ss <-
        readWave(audio_file,
                 from = start,
                 to = end,
                 units = "seconds") # audio sub sample
      
      date <-
        as.POSIXct(strsplit(audio_name, "_")[[1]][2],
                   format = "%Y%m%d",
                   tz = "America/Whitehorse")
      time <-
        as.POSIXct(strsplit(audio_name, "_")[[1]][3],
                   format = "%H%M%S",
                   tz = "America/Whitehorse") + seconds(start)
      date_time <-
        as.POSIXct(substring(audio_name, (nchar(audio_name) - 14), nchar(audio_name)),
                   format = "%Y%m%d_%H%M%S",
                   tz = "America/Whitehorse") + seconds(start)
      
      
      # number of points to use for the fast fourier transform
      n_fft = 1024
      
      # window size (in points)
      win_size = 2048
      
      # overlap (in points)
      overlap = 128
      
      img_name <-
        paste0(img_dir, "/", create_image_name(audio_name), ".png")
      
      png(filename = img_name)
      
      # calling the spectrogram function
      spectro(
        audio_ss,
        nfft = n_fft,
        win = win_size,
        ol = overlap,
        t0 = date_time,
        plot_spec = T,
        normalize = T,
        return_data = F
      )
      
      dev.off()
      
    }
  }
}








###########################################
#play(audio_ss)


##############
# #cutw(audio, from = 0, to = 30) # subsampled audio of the first minute

# plot(audio_ss)
# spec(audio_ss, f)

# signal = audio_ss@left
# fs = audio_ss@samp.rate
#
# # demean to remove DC offset
# sno <- signal - mean(signal)
#
# # plotting waveform
# plot(sno, type = "l", xlab = "Samples", ylab = "Amplitude")


# subsampling to 1 minute

# visualizing one minute