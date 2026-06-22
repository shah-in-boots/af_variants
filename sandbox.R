# sandbox.R

library(targets)
library(tarchetypes)
library(EGM)
library(tidyverse)
library(data.table)

# Read in annotated ECG
ecg <- EGM::read_wfdb("21", record_dir = "./data/", annotator = "ann")
ann <- ecg$annotation$ann

# Window it
beats <- EGM::window(
  ecg,
  window_method = "rhythm",
  rhythm_type = "sinus",
  onset_criteria = list(type = "("),
  offset_criteria = list(type = ")"),
  reference_criteria = list(type = "N")
)

# Example of each of the windows plotted out

# One plot per beat (each window is an individual EGM object)
beat_plots <- lapply(seq_along(beats), function(i) {
  ggm(beats[[i]], channels = "II") +
    ggtitle(paste0("Beat ", i))
})

# Lay all beats out side-by-side in a grid
patchwork::wrap_plots(beat_plots)

# Or step through a single beat at a time
ggm(beats[[12]])

