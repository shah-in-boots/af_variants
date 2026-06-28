# Evaluate trained ECG-beat models on the held-out TEST split.
#
# This is the read-only counterpart to R/train.R. Training deliberately leaves
# the `test` rows untouched; here we score each serialized `<name>.keras` on
# those rows and emit one row of statistics per model, so the pipeline grows a
# table you can scan to see which architecture worked and which did not.
#
# Two constraints shape the design:
#   * Low memory  -- we never build the whole test tensor. Beats are streamed
#     through the model in chunks (see predict_logits()), so peak memory is one
#     (chunk, 500, 12) array regardless of how many test beats there are, and
#     only one model is loaded at a time.
#   * No retraining -- we only ever load `.keras` files and predict. Re-running
#     the pipeline re-scores nothing that hasn't changed (targets caches each
#     branch); adding a model just appends a row.

# Stream a table of beats through a model and return the raw logits, one per
# row of `tbl` (in row order). We do our own outer batching over `chunk_size`
# rows -- build_beat_arrays() materializes a (chunk_size, 500, 12) array, we
# predict on it, keep only the scalar logits, and move on. keras3 batches
# internally too, but that needs the full `x` in memory; this keeps the array
# itself bounded so the test set can be arbitrarily large.
predict_logits <- function(model, tbl, chunk_size = 256L) {
  n <- nrow(tbl)
  logits <- numeric(n)
  if (n == 0) {
    return(logits)
  }

  starts <- seq.int(1L, n, by = chunk_size)
  for (s in starts) {
    e <- min(s + chunk_size - 1L, n)
    chunk <- tbl[s:e, , drop = FALSE]
    x <- build_beat_arrays(chunk)$x
    # Models output a single linear logit (sigmoid lives in the loss), so
    # predict() returns an (m, 1) array of logits; flatten to a vector.
    logits[s:e] <- as.numeric(predict(model, x, verbose = 0L))
  }

  logits
}

# Recover the architecture name from a model file name. The file is named
# `<arch>_<hp...>_<fit...>`, and `arch` may itself contain an underscore
# (e.g. "cnn_lstm"), so we match against the registered builder names and take
# the longest one the file name starts with ("cnn_lstm" wins over "cnn").
parse_architecture <- function(model_name, archs = names(model_builder())) {
  hit <- archs[vapply(
    archs,
    function(a) model_name == a || startsWith(model_name, paste0(a, "_")),
    logical(1)
  )]
  if (length(hit) == 0) {
    return(NA_character_)
  }
  hit[which.max(nchar(hit))]
}

# Score one serialized model on the test split and return a one-row tibble of
# statistics. `model_path` is a path to a `<name>.keras` file (the `models`
# target hands these in); `split_dat` is the labeled, split beat table.
#
# Headline metrics are threshold-free (ROC-AUC, PR-AUC) -- the right lens for a
# rare-case problem and for ranking architectures. The remaining metrics use a
# 0.5 probability cut (== logit 0, what training optimized) to summarize the
# confusion matrix. Cases (label 1) are the event, so sensitivity is case
# recall and precision is case PPV.
score_model <- function(
  model_path,
  split_dat,
  chunk_size = 256L,
  threshold = 0.5
) {
  # One model in, one model out: free the backend graph + memory when we leave,
  # however we leave (error included), so a crew worker scoring many models in
  # sequence does not accumulate graphs.
  on.exit({
    keras3::clear_session()
    gc(verbose = FALSE)
  })

  test_tbl <- dplyr::filter(split_dat, split == "test")
  model_name <- fs::path_ext_remove(fs::path_file(model_path))
  # Models live in model_dir/<data_id>/<name>.keras, so the parent folder names
  # the dataset version -- carry it so metrics join unambiguously to the log.
  data_id <- fs::path_file(fs::path_dir(model_path))

  # compile = FALSE: we only predict, so skip rebuilding the optimizer.
  model <- keras3::load_model(model_path, compile = FALSE)
  prob <- stats::plogis(predict_logits(model, test_tbl, chunk_size))

  # yardstick wants factors; put "case" first so it is the event level.
  lvl <- c("case", "control")
  eval_tbl <- tibble::tibble(
    truth = factor(dplyr::if_else(test_tbl$label == 1L, "case", "control"), levels = lvl),
    prob_case = prob,
    pred = factor(dplyr::if_else(prob >= threshold, "case", "control"), levels = lvl)
  )

  # Threshold-free ranking metrics.
  roc <- yardstick::roc_auc(eval_tbl, truth, prob_case, event_level = "first")$.estimate
  pr <- yardstick::pr_auc(eval_tbl, truth, prob_case, event_level = "first")$.estimate

  # Confusion-matrix metrics at the fixed threshold. These can be NA (with a
  # warning) when the threshold predicts no positives -- common for a rare
  # class, and informative rather than an error.
  hard_metrics <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::sensitivity,
    yardstick::specificity,
    yardstick::precision,
    yardstick::f_meas,
    yardstick::mcc
  )
  hard <- hard_metrics(eval_tbl, truth = truth, estimate = pred, event_level = "first") |>
    dplyr::select(.metric, .estimate) |>
    tidyr::pivot_wider(names_from = .metric, values_from = .estimate)

  n_beats <- nrow(test_tbl)
  n_case <- sum(test_tbl$label == 1L)

  tibble::tibble(
    model = model_name,
    data_id = data_id,
    arch = parse_architecture(model_name),
    n_beats = n_beats,
    n_case = n_case,
    prevalence = n_case / n_beats,
    roc_auc = roc,
    pr_auc = pr,
    threshold = threshold,
    !!!hard,
    evaluated_at = Sys.time()
  )
}

# Write the combined metrics table out as a browsable CSV (sorted best-first by
# ROC-AUC) and return the path, so a `format = "file"` target can track it. The
# in-pipeline `model_metrics` target is the source of truth; this is the copy
# you open in a spreadsheet / read_csv outside the pipeline.
write_metrics_table <- function(model_metrics, metrics_dir) {
  fs::dir_create(metrics_dir)
  path <- fs::path(metrics_dir, "model_metrics", ext = "csv")
  model_metrics |>
    dplyr::arrange(dplyr::desc(roc_auc)) |>
    readr::write_csv(path)
  as.character(path)
}
