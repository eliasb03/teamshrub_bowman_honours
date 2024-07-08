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


# setting variables
f <- 48000 # sampling frequency

# importing a audio file
main_dir <- "C:/Users/elias/OneDrive/Documents/University/Honours/teamshrub_bowman_honours/data/temp/ARUQ_1_P_1Jul2024/Data/audio"
audio_file_name <- "ARUQ1_20240622_170000" 

audio_file <- file.path(main_dir, paste0(audio_name,".wav"))
audio <- readWave(audio_file) # full 10 minute audio file
start_sec = 70
end_sec = 90
audio_ss <- readWave(audio_file, from=start_sec, to=end_sec, units="seconds") # audio sub sample

date <- as.POSIXct(strsplit(audio_name, "_")[[1]][2], format = "%Y%m%d", tz= "America/Whitehorse")
time <- as.POSIXct(strsplit(audio_name, "_")[[1]][3], format = "%H%M%S", tz= "America/Whitehorse") + seconds(start_sec)
date_time <- as.POSIXct(substring(audio_name, (nchar(audio_name)-14), nchar(audio_name)), format = "%Y%m%d_%H%M%S", tz= "America/Whitehorse") + seconds(start_sec)


# number of points to use for the fast fourier transform
nfft=1024

# window size (in points)
win=256

# overlap (in points)
ol=128

# Function courtesy of: https://hansenjohnson.org/post/spectrograms-in-r/
# function takes a wave form, nfft, window (win), overlap (ol)
spectro = function(data, nfft, win, ol, t0=0, plot_spec = T, normalize = F, return_data = F,...){
  # extract signal
  signal_with_offset = data@left
  
  # demean to remove DC offset
  signal = signal_with_offset-mean(signal_with_offset)
  
  # determine duration
  dur = length(signal)/data@samp.rate
  
  # create spectrogram
  spec = specgram(x = signal,
                  n = nfft,
                  Fs = data@samp.rate,
                  window = win,
                  overlap = ol
  )
  
  # discard phase info
  P = abs(spec$S)
  
  # normalize
  if(normalize){
    P = P/max(P)  
  }
  
  # convert to dB
  P = 10*log10(P)
  
  # config time axis
  if(t0==0){
    t = as.numeric(spec$t)
  } else {
    t = as.POSIXct(spec$t, origin = t0)
  }
  
  # rename freq
  f = spec$f
  
  if(plot_spec){
    
    # change plot colour defaults
    par(bg = "black")
    par(col.lab="white")
    par(col.axis="white")
    par(col.main="white")
    
    # plot spectrogram
    imagep(t,f, t(P), col = oce.colorsViridis, drawPalette = T,
           ylab = 'Frequency [Hz]', axes = F,...)

    
    box(col = 'white')
    axis(2, labels = T, col = 'white')
    
    # add x axis
    if(t0==0){
      
      axis(1, labels = T, col = 'white')
      
    }else{
      
      axis.POSIXct(seq.POSIXt(t0, t0+dur, 10), side = 1, format = '%H:%M:%S', col = 'white', las = 1)
      mtext(paste0(format(t0, '%B %d, %Y')), side = 1, adj = 0, line = 2, col = 'white')
      
    }
  }
  
  if(return_data){
    
    # prep output
    spec = list(
      t = t,
      f = f,
      p = t(P)
    )
    
    return(spec)  
  }
}

# calling the spectrogram function
spectro(audio_ss,
        nfft=1024,
        win=256,
        ol=128,
        t0=date_time,
        plot_spec = T,
        normalize = T,
        return_data = F
)

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