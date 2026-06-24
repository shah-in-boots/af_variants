# keras3 model registry.
#
# v1 keeps ONE tiny, fast, plug-and-play architecture so the pipeline runs
# end-to-end. Each entry is a build function:
#   build_fn(input_shape = c(500, 12)) -> compiled keras3 model (sigmoid output).
# Adding architectures (biRNN, CNN->biRNN) later is just another registry entry.

# Small 1D CNN over the 12-lead beat: two conv blocks, global pooling, one dense
# head. Deliberately minimal -- correctness and speed over accuracy.
build_cnn_small <- function(input_shape = c(500L, 12L)) {
  keras3::keras_model_sequential(input_shape = input_shape) |>
    keras3::layer_conv_1d(filters = 16, kernel_size = 7, activation = "relu",
                          padding = "same") |>
    keras3::layer_max_pooling_1d(pool_size = 4) |>
    keras3::layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu",
                          padding = "same") |>
    keras3::layer_global_average_pooling_1d() |>
    keras3::layer_dense(units = 16, activation = "relu") |>
    keras3::layer_dense(units = 1, activation = "sigmoid") |>
    (\(m) {
      keras3::compile(
        m,
        optimizer = keras3::optimizer_adam(),
        loss = "binary_crossentropy",
        metrics = c("AUC")
      )
      m
    })()
}

# Registry: name -> list(build_fn, epochs, batch_size).
model_registry <- list(
  cnn_small = list(
    build_fn   = build_cnn_small,
    epochs     = 5L,
    batch_size = 64L
  )
)
