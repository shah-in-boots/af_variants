# Build the list of model specs the pipeline branches over, from hyperparameter
# grids.
#
# `grid` is a tibble with one row per model: an `architecture` column plus one
# column per hyperparameter. Build it with tidyr::expand_grid() so that listing
# several values for a column sweeps their Cartesian product -- e.g.
# expand_grid(architecture = "resnet", n_blocks = c(4L, 6L), loss = c("bce",
# "focal")) is four models. A list-column carries a vector-valued hyperparameter
# (e.g. tcn `dilations`); expand_grid keeps the whole vector as one value.
#
# Each row becomes one spec -- list(architecture, hp, fit) -- where `hp` is every
# non-architecture column of that row and `fit` (epochs/batch_size) is shared
# across all models. Keep one grid per architecture and concatenate the results
# (see _targets.R), so each architecture carries only its own hyperparameters and
# you never have to reconcile cnn's `n_blocks` with tcn's `dilations`.
grid_to_specs <- function(grid, fit) {
  purrr::pmap(grid, function(architecture, ...) {
    list(architecture = architecture, hp = list(...), fit = fit)
  })
}
