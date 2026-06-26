# Train an ECG-beat model and serialize it to the model directory.
#
# This is the plumbing around the architectures in R/models.R. It:
#   1. turns the labeled beat split into (n, 500, 12) tensors (train rows only),
#   2. builds the requested architecture, with `hp` overriding its defaults,
#   3. fits it -- class-weighted, to handle the case/control imbalance,
#   4. writes one self-contained `<name>.keras` file into `model_dir`, named
#      after the architecture + hyperparameters,
# and returns that path so the target can track it as a file.
#
# The held-out "test" rows are intentionally left untouched here -- they are
# reserved for the later evaluation / activation phase.

train_ecg_model <- function(
  split_dat,
  model_dir,
  architecture = "cnn",
  hp = list(),
  fit = list(epochs = 30L, batch_size = 64L, validation_split = 0.15),
  balance_classes = TRUE,
  patience = 8L,
  seed = 1234L
) {
  # Resolve the on-disk name *first*. The file name encodes the full spec
  # (architecture + hp + fit), so an existing `<name>.keras` IS this exact model:
  # skip the expensive tensor build + fit and just hand back the path. This makes
  # the skip robust even when targets' own cache is gone (or the file came from a
  # different run / was dropped in by hand). Delete the file (or change a
  # hyperparameter) to force a retrain. NOTE: the name encodes hyperparameters,
  # not the architecture *source*, so editing a build_ecg_*() body without
  # changing hp/fit reuses the old file -- delete it to pick the change up.
  fs::dir_create(model_dir)
  name <- make_model_name(architecture, hp, fit)
  path <- fs::path(model_dir, name, ext = "keras")
  if (fs::file_exists(path)) {
    return(as.character(path))
  }

  # Reproducible weight init / shuffling.
  keras3::set_random_seed(seed)

  # Resolve the architecture builder and build a compiled model. Anything in
  # `hp` overrides that builder's defaults, so the target fully specifies it.
  builders <- model_builder()
  if (!architecture %in% names(builders)) {
    stop(
      "Unknown architecture '",
      architecture,
      "'. Available: ",
      paste(names(builders), collapse = ", ")
    )
  }
  model <- do.call(builders[[architecture]], hp)

  # Training tensors: (n, 500, 12) array + 0/1 label vector, train rows only.
  train_tbl <- dplyr::filter(split_dat, split == "train")
  arrays <- build_beat_arrays(train_tbl)

  # Down-weight the majority (control) class so the rare cases are not ignored.
  class_weight <- if (balance_classes) compute_class_weights(arrays$y) else NULL

  # Early stopping only makes sense when we are holding out a validation slice.
  val_split <- if (is.null(fit$validation_split)) 0 else fit$validation_split
  callbacks <- list()
  if (!is.null(patience) && patience > 0 && val_split > 0) {
    callbacks <- list(callback_early_stopping(
      monitor = "val_loss",
      patience = patience,
      restore_best_weights = TRUE
    ))
  }

  do.call(
    keras3::fit,
    c(
      list(
        object = model,
        x = arrays$x,
        y = arrays$y,
        class_weight = class_weight,
        callbacks = callbacks,
        verbose = 2L
      ),
      fit
    )
  )

  # Serialize: one self-contained .keras file (name/path were resolved up top for
  # the skip check).
  keras3::save_model(model, path, overwrite = TRUE)

  as.character(path)
}

# Inverse-frequency class weights as a keras-friendly named list keyed by the
# integer class label ("0"/"1"), so the two classes contribute equal total
# weight regardless of how imbalanced the counts are.
compute_class_weights <- function(y) {
  counts <- table(y)
  weights <- as.list(length(y) / (length(counts) * as.numeric(counts)))
  names(weights) <- names(counts)
  weights
}
