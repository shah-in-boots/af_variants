# Model registry / provenance log.
#
# Every trained model gets a sidecar `<name>.json` "card" written next to its
# `.keras` file (see R/train.R), recording exactly how it was made -- the
# architecture, every hyperparameter, the fit settings, the case definition, and
# a fingerprint of the data it was trained / validated / tested on (beat and
# patient counts, case counts). read_model_card() then turns each card back into
# one tidy row, and the `model_log` target stacks them into a table you can sort
# and compare in batch.
#
# Cards are per-model files (not one shared CSV) on purpose: training branches
# run in parallel across crew workers, and one-file-per-worker sidesteps the
# race you would hit appending rows to a single log.

# A short, stable fingerprint of the *dataset version* a model is trained on:
# the beats, their labels, and their train/val/test assignment. Folding this into
# the model path (model_dir/<data_id>/...) means a change to the case definition
# or the split lands in a new folder and forces a retrain, instead of silently
# reusing a model that was trained on different labels.
make_data_id <- function(split_dat, n = 10L) {
  key <- split_dat |>
    dplyr::select(beat_name, label, split) |>
    dplyr::arrange(beat_name)
  substr(rlang::hash(key), 1L, n)
}

# Collapse a hyperparameter / fit list so every value is a single scalar: a
# length>1 value (e.g. tcn `dilations`) becomes a comma-joined string, so each
# card is exactly one row and the value is still readable in the log.
scalarize_params <- function(x) {
  lapply(x, function(v) if (length(v) > 1) paste(v, collapse = ",") else v)
}

# One-row tibble of per-split counts -- beats and patients, total and cases --
# flattened to columns like `train_beats` / `test_case_patients` so it slots
# straight into the card and the log table. Patient counts (distinct `broad_id`)
# are reported per split for provenance; because the split is now at the beat
# level (R/splits.R), a patient can contribute to more than one split, so these
# patient counts overlap across sets and need not sum to the total.
summarize_split <- function(split_dat) {
  split_dat |>
    dplyr::group_by(split) |>
    dplyr::summarise(
      beats = dplyr::n(),
      case_beats = sum(label == 1L),
      patients = dplyr::n_distinct(broad_id),
      case_patients = dplyr::n_distinct(broad_id[label == 1L]),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = split,
      values_from = c(beats, case_beats, patients, case_patients),
      names_glue = "{split}_{.value}"
    )
}

# Pull the bits worth keeping from a keras fit() history: how long it actually
# trained (early stopping may cut it short) and the best validation scores.
summarize_history <- function(history) {
  m <- history$metrics
  out <- list(epochs_run = length(m$loss))
  if (!is.null(m$val_loss)) {
    best <- which.min(m$val_loss)
    out$best_epoch <- best
    out$best_val_loss <- m$val_loss[best]
    if (!is.null(m$val_auc)) {
      out$best_val_auc <- m$val_auc[best]
    }
  }
  out
}

# Write one model's provenance card as pretty JSON next to its `.keras` file, and
# return the path. Hyperparameters and fit settings are prefixed (`hp_`, `fit_`)
# so they never collide with the data/history columns.
write_model_card <- function(
  card_path,
  model,
  architecture,
  data_id,
  case_definition,
  hp,
  fit,
  balance_classes,
  seed,
  split_dat,
  history
) {
  card <- c(
    list(
      model = model,
      architecture = architecture,
      data_id = data_id,
      case_definition = case_definition,
      balance_classes = balance_classes,
      seed = seed
    ),
    stats::setNames(scalarize_params(hp), paste0("hp_", names(hp))),
    stats::setNames(scalarize_params(fit), paste0("fit_", names(fit))),
    as.list(summarize_split(split_dat)),
    summarize_history(history),
    list(
      keras_version = as.character(utils::packageVersion("keras3")),
      # Guarantee a length-1 string: tf_version() can return NULL when no Python
      # session is up, and a length-0 value would serialize to `[]` (then read
      # back as a 0-row card).
      tf_version = {
        v <- tryCatch(as.character(tensorflow::tf_version()), error = function(e) "")
        if (length(v) == 1L) v else ""
      },
      trained_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
  )
  jsonlite::write_json(card, card_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  as.character(card_path)
}

# Read one model card back into a single tidy row. Branches of the `model_log`
# target are row-bound by targets, filling NA where a column is absent (e.g. a
# cnn card has no `hp_dilations`), so the table is naturally sparse-but-aligned.
read_model_card <- function(card_path) {
  obj <- jsonlite::read_json(card_path, simplifyVector = TRUE)
  # A JSON null / empty array reads back as a length-0 element, which would make
  # the whole row vanish; coerce any such field to a single NA so the card is
  # always exactly one row.
  obj <- lapply(obj, function(v) if (length(v) == 0L) NA else v)
  tibble::as_tibble(obj)
}

# Write a table to <dir>/<name>.csv (creating dir) and return the path, so a
# `format = "file"` target can track the export.
write_table_csv <- function(dat, dir, name) {
  fs::dir_create(dir)
  path <- fs::path(dir, name, ext = "csv")
  readr::write_csv(dat, path)
  as.character(path)
}
