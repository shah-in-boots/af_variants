# Train / predict / evaluate for the beat classifier.
#
# keras models are not cleanly RDS-serializable, so fit_model writes the trained
# model to a `.keras` file and returns its path (the targets target is a file
# target). Prediction/evaluation reload from that path.

# Fit one model on the training beats of a labeled+split table.
# Returns the path to the saved `.keras` model.
fit_model <- function(labeled_split, model_name, out_dir, seed = 1234L) {
  spec <- model_registry[[model_name]]
  if (is.null(spec)) stop("Unknown model: ", model_name)

  keras3::set_random_seed(seed)

  train_tbl <- dplyr::filter(labeled_split, split == "train")
  arrays <- build_beat_arrays(train_tbl)

  model <- spec$build_fn(input_shape = c(dim(arrays$x)[2], dim(arrays$x)[3]))

  # Class weights so rare cases are not ignored.
  n <- length(arrays$y)
  n_pos <- sum(arrays$y == 1)
  n_neg <- sum(arrays$y == 0)
  class_weight <- list(
    "0" = n / (2 * max(n_neg, 1)),
    "1" = n / (2 * max(n_pos, 1))
  )

  keras3::fit(
    model,
    x = arrays$x,
    y = arrays$y,
    epochs = spec$epochs,
    batch_size = spec$batch_size,
    class_weight = class_weight,
    verbose = 2
  )

  fs::dir_create(out_dir)
  out_path <- fs::path(out_dir, paste0("fit_", model_name, ".keras"))
  keras3::save_model(model, out_path, overwrite = TRUE)
  as.character(out_path)
}

# Beat-level predictions on the test split. Returns a tibble of
# (beat_name, pt_number, label, .pred) for held-out patients.
predict_beats <- function(model_path, labeled_split) {
  model <- keras3::load_model(model_path)
  test_tbl <- dplyr::filter(labeled_split, split == "test")
  arrays <- build_beat_arrays(test_tbl)

  preds <- as.numeric(predict(model, arrays$x, verbose = 0))

  tibble::tibble(
    beat_name  = test_tbl$beat_name,
    pt_number  = test_tbl$pt_number,
    label      = factor(test_tbl$label, levels = c(1L, 0L)),
    .pred      = preds
  )
}

# Beat-level metrics on the held-out predictions (ROC-AUC, PR-AUC).
evaluate_beats <- function(preds) {
  yardstick::metric_set(yardstick::roc_auc, yardstick::pr_auc)(
    preds,
    truth = label,
    .pred
  )
}
