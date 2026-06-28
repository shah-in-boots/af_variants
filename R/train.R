# Train an ECG-beat model and serialize it to the model directory.
#
# This is the plumbing around the architectures in R/models.R. It:
#   1. turns the labeled beat split into (n, 500, 12) tensors (train rows only),
#   2. builds the requested architecture, with `hp` overriding its defaults,
#   3. fits it on the train split -- class-weighted to handle the case/control
#      imbalance, early-stopped / LR-scheduled on the held-out val split,
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
  fit = list(epochs = 30L, batch_size = 128L),
  case_definition = NA_character_,
  balance_classes = TRUE,
  patience = 8L,
  seed = 1234L
) {
  # Resolve the on-disk name *first*. Identity has two parts: a `data_id` (a hash
  # of the labels + split, see make_data_id) that names a per-dataset SUBFOLDER,
  # and `name` (the architecture + hp + fit) that names the file within it. So an
  # existing `<data_id>/<name>.keras` IS this exact model trained on this exact
  # data: skip the expensive tensor build + fit and hand back the path. Putting
  # the data in the path is what stops a changed case definition from silently
  # reusing a model trained on the old labels. The skip survives cache loss (the
  # identity lives in the path, not targets' cache). Delete the file (or change a
  # hyperparameter, or the data) to force a retrain. NOTE: the name encodes
  # hyperparameters, not the architecture *source*, so editing a build_ecg_*()
  # body without changing hp/fit reuses the old file -- delete it to pick it up.
  data_id <- make_data_id(split_dat)
  out_dir <- fs::path(model_dir, data_id)
  fs::dir_create(out_dir)
  name <- make_model_name(architecture, hp, fit)
  path <- fs::path(out_dir, name, ext = "keras")
  card_path <- fs::path(out_dir, name, ext = "json")
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

  # Explicit, label-stratified validation set (carved by make_split_data). We
  # pass it as `validation_data` rather than keras's `validation_split`, which
  # would otherwise hold out an unshuffled, unstratified tail of the training
  # array.
  val_tbl <- dplyr::filter(split_dat, split == "val")
  validation_data <- if (nrow(val_tbl) > 0) {
    val_arrays <- build_beat_arrays(val_tbl)
    list(val_arrays$x, val_arrays$y)
  } else {
    NULL
  }

  # Down-weight the majority (control) class so the rare cases are not ignored.
  class_weight <- if (balance_classes) compute_class_weights(arrays$y) else NULL

  # Callbacks need a validation signal to monitor, so only attach them when we
  # actually have a validation set. Early stopping restores the best weights; the
  # LR schedule eases the optimizer down once val_loss plateaus.
  callbacks <- list()
  if (!is.null(patience) && patience > 0 && !is.null(validation_data)) {
    callbacks <- list(
      keras3::callback_early_stopping(
        monitor = "val_loss",
        patience = patience,
        restore_best_weights = TRUE
      ),
      keras3::callback_reduce_lr_on_plateau(
        monitor = "val_loss",
        factor = 0.5,
        patience = max(1L, patience %/% 2L)
      )
    )
  }

  history <- do.call(
    keras3::fit,
    c(
      list(
        object = model,
        x = arrays$x,
        y = arrays$y,
        validation_data = validation_data,
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

  # Provenance card next to the model: the full spec + the dataset fingerprint it
  # was trained on. This is what the `model_log` target collects for comparison.
  write_model_card(
    card_path,
    model = name,
    architecture = architecture,
    data_id = data_id,
    case_definition = case_definition,
    hp = hp,
    fit = fit,
    balance_classes = balance_classes,
    seed = seed,
    split_dat = split_dat,
    history = history
  )

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
