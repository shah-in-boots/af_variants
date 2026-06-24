# Load standardized beat WFDB records into numeric tensors for keras3.
#
# Each beat record is 500 samples x 12 leads (R-wave/"N" aligned, written by
# make_individual_beats()). The model input is therefore a (n_beats, 500, 12)
# array. Lead order is fixed by ECG_LEADS so channels are consistent across
# beats.

# Canonical 12-lead order as written by the WFDB beat records.
ECG_LEADS <- c("I", "II", "III", "AVF", "AVL", "AVR",
               "V1", "V2", "V3", "V4", "V5", "V6")

TARGET_SAMPLES <- 500L

# Read one beat record into a 500 x 12 matrix (per-lead z-scored).
# `beat_path` is the path to the `.dat` file; the record name and directory are
# derived from it. No annotation is read (beats carry none).
read_beat_tensor <- function(beat_path, target_samples = TARGET_SAMPLES) {
  rec <- fs::path_ext_remove(fs::path_file(beat_path))
  dir <- fs::path_dir(beat_path)

  ecg <- EGM::read_wfdb(rec, record_dir = dir)
  sig <- as.data.frame(ecg$signal)

  mat <- as.matrix(sig[, ECG_LEADS, drop = FALSE])
  storage.mode(mat) <- "double"

  # Pad/trim to target length (should already be exact, but be defensive).
  n <- nrow(mat)
  if (n < target_samples) {
    mat <- rbind(mat, matrix(0, target_samples - n, ncol(mat)))
  } else if (n > target_samples) {
    mat <- mat[seq_len(target_samples), , drop = FALSE]
  }

  # Per-lead z-score (constant leads -> zeros).
  mu <- colMeans(mat)
  sdv <- apply(mat, 2, stats::sd)
  sdv[sdv == 0 | is.na(sdv)] <- 1
  sweep(sweep(mat, 2, mu, "-"), 2, sdv, "/")
}

# Stack a set of beats into an (n, 500, 12) array `x` and an integer label
# vector `y`. v1 loads into memory (fine for a subset / smoke test); the scaling
# path is a tfdatasets generator that reads beats per batch.
build_beat_arrays <- function(tbl, target_samples = TARGET_SAMPLES) {
  n <- nrow(tbl)
  x <- array(0, dim = c(n, target_samples, length(ECG_LEADS)))
  for (i in seq_len(n)) {
    x[i, , ] <- read_beat_tensor(tbl$beat_path[i], target_samples)
  }
  list(x = x, y = as.integer(tbl$label))
}
