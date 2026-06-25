# Keras 3 architectures for classifying a single ECG beat as coming from a
# carrier of a pathogenic TTN variant (case) vs not (control).
#
# INPUT  : one beat = 500 samples x 12 leads, i.e. input shape (500, 12).
# OUTPUT : a single sigmoid unit = P(beat is a case).
#
# Each architecture lives in its own build_ecg_*() function written in the plain,
# layer-by-layer Keras style so it is easy to read and adjust. Every knob you are
# likely to turn (filter counts, kernel size, depth, dropout, learning rate, ...)
# is a named argument with a sensible default, so a model is fully described by
# its builder plus the hyperparameter list you pass from _targets.R. Each builder
# returns a COMPILED model.
#
# To add an architecture: write a build_ecg_*() that takes (input_shape, ...,
# learning_rate), returns a compiled model, and register it in
# ecg_model_builders() so the pipeline can dispatch to it by name.
#
# The default input shape references TARGET_SAMPLES and ECG_LEADS from
# R/tensors.R. Because it is a default argument it is evaluated at call time, so
# the order in which tar_source() loads these files does not matter.


# --- Shared compile step -----------------------------------------------------

# Compile a model for binary case/control classification. Kept separate so each
# build_ecg_*() function below can focus purely on architecture.
compile_ecg_model <- function(model, learning_rate = 1e-3) {
  model |>
    compile(
      optimizer = optimizer_adam(learning_rate = learning_rate),
      loss = "binary_crossentropy",
      metrics = list(
        metric_binary_accuracy(name = "accuracy"),
        metric_auc(name = "auc")
      )
    )
  model
}


# --- 1D CNN ------------------------------------------------------------------

# A straightforward 1D convolutional network: a stack of
# Conv -> BatchNorm -> ReLU -> MaxPool blocks that shrink the 500-sample time
# axis while doubling the channel width, then global pooling into a small dense
# head. A solid, fast baseline for beat morphology.
build_ecg_cnn <- function(input_shape = c(TARGET_SAMPLES, length(ECG_LEADS)),
                          filters = 32,
                          kernel_size = 7,
                          n_blocks = 3,
                          pool_size = 2,
                          dense_units = 64,
                          dropout = 0.3,
                          learning_rate = 1e-3) {

  model <- keras_model_sequential(input_shape = input_shape)

  # Convolutional feature extractor. Filters double each block
  # (e.g. 32 -> 64 -> 128) so deeper layers see coarser, more abstract features.
  for (b in seq_len(n_blocks)) {
    model |>
      layer_conv_1d(
        filters = filters * 2^(b - 1),
        kernel_size = kernel_size,
        padding = "same"
      ) |>
      layer_batch_normalization() |>
      layer_activation("relu") |>
      layer_max_pooling_1d(pool_size = pool_size)
  }

  # Classification head.
  model |>
    layer_global_average_pooling_1d() |>
    layer_dense(units = dense_units, activation = "relu") |>
    layer_dropout(rate = dropout) |>
    layer_dense(units = 1, activation = "sigmoid")

  compile_ecg_model(model, learning_rate)
}


# --- 1D ResNet ---------------------------------------------------------------

# One residual block: two Conv -> BN layers with a skip connection added back in.
# When the block downsamples (stride > 1) the shortcut is projected with a 1x1
# conv so the shapes line up before adding. (If you make `filters` vary between
# blocks, also project on that width change.)
residual_block_1d <- function(x, filters, kernel_size, stride = 1) {
  shortcut <- x

  y <- x |>
    layer_conv_1d(filters = filters, kernel_size = kernel_size,
                  strides = stride, padding = "same") |>
    layer_batch_normalization() |>
    layer_activation("relu") |>
    layer_conv_1d(filters = filters, kernel_size = kernel_size,
                  padding = "same") |>
    layer_batch_normalization()

  if (stride != 1) {
    shortcut <- shortcut |>
      layer_conv_1d(filters = filters, kernel_size = 1,
                    strides = stride, padding = "same") |>
      layer_batch_normalization()
  }

  layer_add(list(y, shortcut)) |>
    layer_activation("relu")
}

# A residual 1D CNN (the family of architectures behind most deep ECG models).
# A conv stem feeds a stack of residual blocks, with every other block halving
# the time axis. Heavier than the plain CNN but trains stably when deep.
build_ecg_resnet <- function(input_shape = c(TARGET_SAMPLES, length(ECG_LEADS)),
                             filters = 64,
                             kernel_size = 7,
                             n_blocks = 4,
                             dense_units = 64,
                             dropout = 0.3,
                             learning_rate = 1e-3) {

  inputs <- layer_input(shape = input_shape)

  # Stem.
  x <- inputs |>
    layer_conv_1d(filters = filters, kernel_size = kernel_size, padding = "same") |>
    layer_batch_normalization() |>
    layer_activation("relu")

  # Residual stack; downsample on even-numbered blocks.
  for (b in seq_len(n_blocks)) {
    stride <- if (b %% 2 == 0) 2 else 1
    x <- residual_block_1d(x, filters = filters, kernel_size = kernel_size,
                           stride = stride)
  }

  outputs <- x |>
    layer_global_average_pooling_1d() |>
    layer_dense(units = dense_units, activation = "relu") |>
    layer_dropout(rate = dropout) |>
    layer_dense(units = 1, activation = "sigmoid")

  model <- keras_model(inputs, outputs)
  compile_ecg_model(model, learning_rate)
}


# --- CNN + LSTM --------------------------------------------------------------

# A hybrid: a couple of conv blocks compress the beat and pull out local
# morphology, then an LSTM reads the resulting short sequence to capture how
# those features are ordered through the beat.
build_ecg_cnn_lstm <- function(input_shape = c(TARGET_SAMPLES, length(ECG_LEADS)),
                               filters = 32,
                               kernel_size = 7,
                               n_conv_blocks = 2,
                               lstm_units = 64,
                               dense_units = 32,
                               dropout = 0.3,
                               learning_rate = 1e-3) {

  model <- keras_model_sequential(input_shape = input_shape)

  for (b in seq_len(n_conv_blocks)) {
    model |>
      layer_conv_1d(filters = filters * 2^(b - 1), kernel_size = kernel_size,
                    padding = "same") |>
      layer_batch_normalization() |>
      layer_activation("relu") |>
      layer_max_pooling_1d(pool_size = 2)
  }

  model |>
    layer_lstm(units = lstm_units) |>
    layer_dense(units = dense_units, activation = "relu") |>
    layer_dropout(rate = dropout) |>
    layer_dense(units = 1, activation = "sigmoid")

  compile_ecg_model(model, learning_rate)
}


# --- Registry + naming -------------------------------------------------------

# Map architecture name -> builder. train_ecg_model() dispatches through this, and
# _targets.R refers to architectures by these names.
ecg_model_builders <- function() {
  list(
    cnn      = build_ecg_cnn,
    resnet   = build_ecg_resnet,
    cnn_lstm = build_ecg_cnn_lstm
  )
}

# Build a filesystem-friendly model name that encodes the architecture and the
# parameters that produced it, e.g.
#   cnn__filters32_kernel7_nblocks3_denseunits64_dropout0.3_learningrate0.001__epochs30_batchsize64
# so the .keras file on disk is self-describing and two runs with different
# hyperparameters never collide.
make_model_name <- function(architecture, hp = list(), fit = list()) {
  fmt <- function(v) {
    v <- if (length(v) > 1) paste(v, collapse = "-") else v
    gsub("[^0-9A-Za-z.-]", "", format(v, scientific = FALSE, trim = TRUE))
  }
  kv <- function(lst) {
    if (length(lst) == 0) return(character(0))
    paste0(names(lst), vapply(lst, fmt, character(1)))
  }

  parts <- c(
    architecture,
    paste(kv(hp), collapse = "_"),
    paste(kv(fit), collapse = "_")
  )
  parts <- parts[nzchar(parts)]
  paste(parts, collapse = "__")
}
