match_ecg_to_genetics <- function(ids, ecg_dir) {
  # Get ECG file
  ecg_file <- fs::path(ecg_dir, "ecg-ids.parquet")

  # ECG data
  # `ecg_number` is the de-identified record name used in the WFDB folders and is
  # cast to integer so it joins cleanly to the beat-parsed keys downstream.
  ecg_dat <-
    arrow::read_parquet(ecg_file) |>
    dplyr::filter(null_ecg == 0) |>
    dplyr::select(
      pt_number,
      ecg_number,
      mrn,
      cluster_id = record_id,
      ecg_name,
      muse_sinus = ecg_sinus
    ) |>
    dplyr::mutate(
      mrn = as.character(mrn),
      ecg_number = as.integer(ecg_number)
    )

  # Combine the ECG datasets and IDs together
  # Make sure real ECG number is present
  ecg_ids <-
    dplyr::left_join(
      ids,
      ecg_dat,
      by = c("mrn", "cluster_id", "pt_number"),
      relationship = "many-to-many"
    ) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(ecg_number)) |>
    dplyr::distinct(ecg_number, .keep_all = TRUE)

  ecg_ids
}

# Assemble the beat-level table: one row per written beat, named and joined to
# its parent ECG metadata and the patient's genetic/ID information. Keeps only
# genetics-matched beats (patient present in the roster, i.e. has `broad_id`).
# `beat_paths` is the flat character vector of `.dat` paths from the branched
# `beat_paths` target; each beat record is named `<ecg_number>_<n>`.
build_beat_table <- function(beat_paths, ecg_ids) {
  beats <- tibble::tibble(
    beat_path  = as.character(beat_paths),
    beat_name  = fs::path_ext_remove(fs::path_file(beat_path)),  # "1_3"
    ecg_number = as.integer(sub("_[^_]+$", "", beat_name)),      # 1
    beat_index = as.integer(sub("^.*_", "", beat_name))          # 3
  )

  beats |>
    dplyr::inner_join(ecg_ids, by = "ecg_number") |>  # inner = genetics-matched only
    dplyr::filter(!is.na(broad_id)) |>                # drop ECGs w/o genetic ID
    dplyr::relocate(beat_name, beat_index, beat_path, ecg_number) |>
    dplyr::arrange(ecg_number, beat_index)
}

# List the WFDB record base names available in a directory.
# Each record is identified by its `.hea` file (e.g. "21.hea" -> "21").
list_ecg_records <- function(wfdb_dir) {
  hea <- fs::dir_ls(wfdb_dir, glob = "*.hea")
  sort(fs::path_ext_remove(fs::path_file(hea)))
}

# Split a vector of record names into a list of evenly sized batches.
# Each batch becomes one dynamic branch, keeping the branch count manageable
# (e.g. ~15k records at batch_size = 100 -> ~150 branches instead of ~15k).
batch_records <- function(records, batch_size = 100L) {
  groups <- ceiling(seq_along(records) / batch_size)
  unname(split(records, groups))
}

# Window one annotated ECG record into standardized individual sinus beats and
# write each beat out as its own WFDB record (`<record>_<n>.dat` + `.hea`) in
# `beat_dir`. No annotations are written. Annotations are per-lead (12
# channels), so `channel` picks the lead that defines the beat boundaries, and
# each beat is time-normalized to `target_samples`, aligned on the QRS peak.
#
# Returns the paths of the `.dat` files written, or `character(0)` if the
# record has no detectable sinus beats or fails to read (warned, not errored,
# so one bad ECG does not stop the batch).
#
# If beats for `record` already exist (passed in `existing`, the `.dat` paths
# already on disk for this record), the record is skipped entirely (no WFDB read
# or windowing) and those paths are returned. The caller computes `existing`
# once per batch to avoid rescanning the (large) beat directory per record.
make_beats_for_record <- function(record,
                                  wfdb_dir,
                                  beat_dir,
                                  annotator = "ann",
                                  channel = 2L,
                                  target_samples = 500L,
                                  align_feature = "N",
                                  existing = character(0)) {

  if (length(existing) > 0) {
    return(as.character(existing))
  }

  beats <- tryCatch({
    ecg <- EGM::read_wfdb(record, record_dir = wfdb_dir, annotator = annotator)
    windows <- EGM::window(
      ecg,
      window_method = "rhythm",
      rhythm_type = "sinus",
      channel_criteria = channel
    )
    EGM::standardize_windows(
      windows,
      standardization_method = "time_normalize",
      target_samples = target_samples,
      align_feature = align_feature,
      preserve_class = TRUE
    )
  }, error = function(e) {
    warning("Skipping record ", record, ": ", conditionMessage(e), call. = FALSE)
    list()
  })

  beat_records <- paste0(record, "_", seq_along(beats))
  for (i in seq_along(beats)) {
    EGM::write_wfdb(beats[[i]], record = beat_records[i], record_dir = beat_dir)
  }

  as.character(fs::path(beat_dir, paste0(beat_records, ".dat")))
}

# Window a batch of ECG records into standardized individual beats (branched
# target). `ecg_list` is a character vector of record base names. Returns the
# paths of all `.dat` files written across the batch.
#
# When `overwrite = FALSE`, records that already have beats on disk are skipped
# (no WFDB read or windowing) and their existing paths returned. The beat
# directory is scanned once per batch and indexed by parent record, so per-record
# lookups don't rescan the (large) beat directory.
make_individual_beats <- function(ecg_list,
                                  wfdb_dir,
                                  beat_dir,
                                  annotator = "ann",
                                  channel = 2L,
                                  target_samples = 500L,
                                  align_feature = "N",
                                  overwrite = FALSE) {

  fs::dir_create(beat_dir)

  # Index existing beats by parent record (everything before the final "_<n>").
  existing_by_record <- list()
  if (!overwrite) {
    done <- fs::dir_ls(beat_dir, glob = "*.dat")
    if (length(done) > 0) {
      names <- fs::path_ext_remove(fs::path_file(done))
      parent <- sub("_[^_]+$", "", names)
      existing_by_record <- split(as.character(done), parent)
    }
  }

  paths <- lapply(
    ecg_list,
    function(record) {
      make_beats_for_record(
        record,
        wfdb_dir = wfdb_dir,
        beat_dir = beat_dir,
        annotator = annotator,
        channel = channel,
        target_samples = target_samples,
        align_feature = align_feature,
        existing = existing_by_record[[record]]
      )
    }
  )

  unlist(paths, use.names = FALSE)
}


