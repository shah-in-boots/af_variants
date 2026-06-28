# Keras 3 architectures for classifying a single ECG beat as coming from a
# carrier of a pathogenic TTN variant (case) vs not (control).
#
# INPUT  : one beat = 500 samples x 12 leads, i.e. input shape (500, 12).
# OUTPUT : a single linear logit; P(beat is a case) = sigmoid(logit). The
# sigmoid lives in the loss (from_logits = TRUE) and is applied at predict /
# activation time, which keeps gradients clean for the saliency maps in
# R/activation.R.
#
# Each architecture lives in its own build_ecg_*() function written in the
# plain, layer-by-layer Keras style so it is easy to read and adjust. Every knob
# you are likely to turn (filter counts, kernel size, depth, dropout, learning
# rate, ...) is a named argument with a sensible default, so a model is fully
# described by its builder plus the hyperparameter list you pass from
# _targets.R. Each builder returns a COMPILED model.
#
# To add an architecture: write a build_ecg_*() that takes (input_shape, ...,
# learning_rate), returns a compiled model, and register it in
# model_builders() so the pipeline can dispatch to it by name.
#
# The default input shape references TARGET_SAMPLES and ECG_LEADS from
# R/tensors.R. Because it is a default argument it is evaluated at call time, so
# the order in which tar_source() loads these files does not matter.

# Compiling step ----

# Compile a model for binary case/control classification. Kept separate so each
# build_ecg_*() function below can focus purely on architecture. Models emit a
# raw logit (no final sigmoid), so the loss and AUC are told `from_logits =
# TRUE` and accuracy thresholds at 0 (logit 0 == probability 0.5). Keeping the
# output a logit makes the Grad-CAM / integrated-gradient maps in R/activation.R
# cleaner.
compile_ecg_model <- function(
  model,
  learning_rate = 1e-3,
  loss = "bce",
  weight_decay = 0
) {
  # Imbalance-handling loss. "bce" relies on the class weights passed at fit time
  # (R/train.R); "focal" down-weights easy examples so the rare cases dominate
  # the gradient. Both keep `from_logits = TRUE` since the head emits a raw logit.
  loss_fn <- switch(
    loss,
    bce = keras3::loss_binary_crossentropy(from_logits = TRUE),
    focal = keras3::loss_binary_focal_crossentropy(gamma = 2, from_logits = TRUE),
    stop("Unknown loss '", loss, "'. Use \"bce\" or \"focal\".")
  )

  # AdamW (decoupled weight decay) when weight_decay > 0, otherwise plain Adam.
  optimizer <- if (weight_decay > 0) {
    keras3::optimizer_adam_w(
      learning_rate = learning_rate,
      weight_decay = weight_decay
    )
  } else {
    keras3::optimizer_adam(learning_rate = learning_rate)
  }

  model |>
    keras3::compile(
      optimizer = optimizer,
      loss = loss_fn,
      metrics = list(
        keras3::metric_binary_accuracy(name = "accuracy", threshold = 0),
        keras3::metric_auc(name = "auc", from_logits = TRUE)
      )
    )
  model
}


# CNN ----

# Straightforward CNN model (wasn't very good originally)
# Conv -> BatchNorm -> ReLU -> MaxPool blocks that shrink the 500-sample time
# axis while doubling the channel width, then global pooling into a small dense
# head. Good for testing.
build_ecg_cnn <- function(
  input_shape = c(TARGET_SAMPLES, length(ECG_LEADS)),
  filters = 32,
  kernel_size = 7,
  n_blocks = 3,
  pool_size = 2,
  dense_units = 64,
  dropout = 0.3,
  learning_rate = 1e-3,
  loss = "bce",
  weight_decay = 0
) {
  model <- keras3::keras_model_sequential(input_shape = input_shape)

  # Convolutional feature extractor. Filters double each block
  # (e.g. 32 -> 64 -> 128) so deeper layers see coarser, more abstract features.
  for (b in seq_len(n_blocks)) {
    # Name the last block's activation so the Grad-CAM code in R/activation.R
    # can grab this feature map by name regardless of how many blocks there are.
    relu_name <- if (b == n_blocks) "final_conv" else NULL
    model |>
      keras3::layer_conv_1d(
        filters = filters * 2^(b - 1),
        kernel_size = kernel_size,
        padding = "same"
      ) |>
      keras3::layer_batch_normalization() |>
      keras3::layer_activation("relu", name = relu_name) |>
      keras3::layer_max_pooling_1d(pool_size = pool_size)
  }

  # Classification head. Linear logit output (sigmoid lives in the loss).
  model |>
    keras3::layer_global_average_pooling_1d() |>
    keras3::layer_dense(units = dense_units, activation = "relu") |>
    keras3::layer_dropout(rate = dropout) |>
    keras3::layer_dense(units = 1, name = "logit")

  compile_ecg_model(model, learning_rate, loss = loss, weight_decay = weight_decay)
}


# ResNet ----

# One residual block: two Conv -> BN layers with a skip connection added back
# in.  When the block downsamples (stride > 1) the shortcut is projected with a
# 1x1 conv so the shapes line up before adding. (If you make `filters` vary
# between blocks, also project on that width change.) Pass `name` to label the
# block's output activation -- used to mark the final block as the Grad-CAM
# feature map.
residual_block_1d <- function(
  x,
  filters,
  kernel_size,
  stride = 1,
  name = NULL
) {
  shortcut <- x

  y <- x |>
    keras3::layer_conv_1d(
      filters = filters,
      kernel_size = kernel_size,
      strides = stride,
      padding = "same"
    ) |>
    keras3::layer_batch_normalization() |>
    keras3::layer_activation("relu") |>
    keras3::layer_conv_1d(
      filters = filters,
      kernel_size = kernel_size,
      padding = "same"
    ) |>
    keras3::layer_batch_normalization()

  if (stride != 1) {
    shortcut <- shortcut |>
      keras3::layer_conv_1d(
        filters = filters,
        kernel_size = 1,
        strides = stride,
        padding = "same"
      ) |>
      keras3::layer_batch_normalization()
  }

  keras3::layer_add(list(y, shortcut)) |>
    keras3::layer_activation("relu", name = name)
}

# A residual 1D CNN (the family of architectures behind most deep ECG models).
# A conv stem feeds a stack of residual blocks, with every other block halving
# the time axis. Heavier than the plain CNN but trains stably when deep.
build_ecg_resnet <- function(
  input_shape = c(TARGET_SAMPLES, length(ECG_LEADS)),
  filters = 64,
  kernel_size = 7,
  n_blocks = 4,
  dense_units = 64,
  dropout = 0.3,
  learning_rate = 1e-3,
  loss = "bce",
  weight_decay = 0
) {
  inputs <- keras3::layer_input(shape = input_shape)

  # Stem.
  x <- inputs |>
    keras3::layer_conv_1d(
      filters = filters,
      kernel_size = kernel_size,
      padding = "same"
    ) |>
    keras3::layer_batch_normalization() |>
    keras3::layer_activation("relu")

  # Residual stack; downsample on even-numbered blocks.
  for (b in seq_len(n_blocks)) {
    stride <- if (b %% 2 == 0) 2 else 1
    # Label the last block's output so activation.R can tap it for Grad-CAM.
    block_name <- if (b == n_blocks) "final_conv" else NULL
    x <- residual_block_1d(
      x,
      filters = filters,
      kernel_size = kernel_size,
      stride = stride,
      name = block_name
    )
  }

  outputs <- x |>
    keras3::layer_global_average_pooling_1d() |>
    keras3::layer_dense(units = dense_units, activation = "relu") |>
    keras3::layer_dropout(rate = dropout) |>
    keras3::layer_dense(units = 1, name = "logit")

  model <- keras3::keras_model(inputs, outputs)
  compile_ecg_model(model, learning_rate, loss = loss, weight_decay = weight_decay)
}


# CNN-LSTM (prediction-only baseline) ----

# CNN front end feeding an LSTM (the original model we trialed). NOTE: kept only
# as a prediction baseline -- it is NOT usable for the activation maps.
# layer_lstm() returns just its final hidden state, which collapses the
# 500-sample time axis, so there is no "final_conv" feature map for Grad-CAM and
# gradients routed through the recurrence smear temporal attribution. Use cnn /
# resnet / tcn for maps.
build_ecg_cnn_lstm <- function(
  input_shape = c(TARGET_SAMPLES, length(ECG_LEADS)),
  filters = 32,
  kernel_size = 7,
  n_conv_blocks = 2,
  lstm_units = 64,
  dense_units = 32,
  dropout = 0.3,
  learning_rate = 1e-3,
  loss = "bce",
  weight_decay = 0
) {
  model <- keras3::keras_model_sequential(input_shape = input_shape)

  for (b in seq_len(n_conv_blocks)) {
    model |>
      keras3::layer_conv_1d(
        filters = filters * 2^(b - 1),
        kernel_size = kernel_size,
        padding = "same"
      ) |>
      keras3::layer_batch_normalization() |>
      keras3::layer_activation("relu") |>
      keras3::layer_max_pooling_1d(pool_size = 2)
  }

  model |>
    keras3::layer_lstm(units = lstm_units) |>
    keras3::layer_dense(units = dense_units, activation = "relu") |>
    keras3::layer_dropout(rate = dropout) |>
    keras3::layer_dense(units = 1, name = "logit")

  compile_ecg_model(model, learning_rate, loss = loss, weight_decay = weight_decay)
}

# TCN (dilated causal convolutions) ----

# One dilated-causal residual block: a dilated conv keeps the full 500-sample
# time axis (no pooling) while the dilation widens the receptive field, then a
# skip connection adds the input back in. Channel width is held constant across
# blocks (the stem projects to `filters`) so the residual add never needs a
# projection. Pass `name` to label the block output as the Grad-CAM feature map.
tcn_block_1d <- function(
  x,
  filters,
  kernel_size,
  dilation_rate,
  dropout = 0,
  name = NULL
) {
  shortcut <- x

  y <- x |>
    keras3::layer_conv_1d(
      filters = filters,
      kernel_size = kernel_size,
      padding = "causal",
      dilation_rate = dilation_rate
    ) |>
    keras3::layer_batch_normalization() |>
    keras3::layer_activation("relu")

  # SpatialDropout drops whole channels -- the usual regularizer for conv stacks.
  if (dropout > 0) {
    y <- keras3::layer_spatial_dropout_1d(y, rate = dropout)
  }

  keras3::layer_add(list(y, shortcut)) |>
    keras3::layer_activation("relu", name = name)
}

# A temporal convolutional network of exponentially dilated causal convolutions,
# matching the architecture family van de Leur et al. used for this exact task.
# Because nothing downsamples, the final feature map is still 500 samples long,
# so Grad-CAM lands at ~sample-level resolution -- the finest activation maps of
# the four architectures. Stem -> dilated residual blocks -> global pool -> head.
build_ecg_tcn <- function(
  input_shape = c(TARGET_SAMPLES, length(ECG_LEADS)),
  filters = 32,
  kernel_size = 7,
  dilations = c(1, 2, 4, 8, 16, 32),
  dense_units = 64,
  dropout = 0.3,
  learning_rate = 1e-3,
  loss = "bce",
  weight_decay = 0
) {
  inputs <- keras3::layer_input(shape = input_shape)

  # Stem: 1x1 conv projects the 12 leads up to `filters` channels at full length.
  x <- inputs |>
    keras3::layer_conv_1d(filters = filters, kernel_size = 1, padding = "same") |>
    keras3::layer_batch_normalization() |>
    keras3::layer_activation("relu")

  # Dilated residual stack; dilation doubles each block so the receptive field
  # grows exponentially while the time axis stays at 500 samples.
  n <- length(dilations)
  for (i in seq_len(n)) {
    # Mark the last block's output as the Grad-CAM feature map.
    block_name <- if (i == n) "final_conv" else NULL
    x <- tcn_block_1d(
      x,
      filters = filters,
      kernel_size = kernel_size,
      dilation_rate = dilations[i],
      dropout = dropout,
      name = block_name
    )
  }

  outputs <- x |>
    keras3::layer_global_average_pooling_1d() |>
    keras3::layer_dense(units = dense_units, activation = "relu") |>
    keras3::layer_dropout(rate = dropout) |>
    keras3::layer_dense(units = 1, name = "logit")

  model <- keras3::keras_model(inputs, outputs)
  compile_ecg_model(model, learning_rate, loss = loss, weight_decay = weight_decay)
}


# Registering names ----

# train_ecg_model() utilizes this to build out a model 
# _targets.R refers to architectures by these names
# Architecture, can be expanded
model_builder <- function() {
  list(
    cnn = build_ecg_cnn,
    resnet = build_ecg_resnet,
    tcn = build_ecg_tcn,
    cnn_lstm = build_ecg_cnn_lstm
  )
}

# Build a filesystem-friendly model name that encodes the architecture and the
# parameters that produced it, e.g.
# cnn__filters32_kernel7_nblocks3_denseunits64_dropout0.3_learningrate0.001__epochs30_batchsize64
# so the .keras file on disk is self-describing and two runs with different
# hyperparameters never collide.
make_model_name <- function(architecture, hp = list(), fit = list()) {
  fmt <- function(v) {
    v <- if (length(v) > 1) paste(v, collapse = "_") else v
    gsub("[^0-9A-Za-z._-]", "", format(v, scientific = FALSE, trim = TRUE))
  }
  kv <- function(lst) {
    if (length(lst) == 0) {
      return(character(0))
    }
    paste0(names(lst), vapply(lst, fmt, character(1)))
  }

  parts <- c(
    architecture,
    paste(kv(hp), collapse = "_"),
    paste(kv(fit), collapse = "_")
  )
  parts <- parts[nzchar(parts)]
  paste(parts, collapse = "_")
}
