# Train/test split.
#
# v1 treats beats as independent observations and splits at the BEAT level
# (stratified on label so both classes appear in each side). This deliberately
# ignores that a patient contributes many beats across ECGs/time points -- a
# simplification to get the pipeline working. Patient-grouped splitting later
#
# Returns the labeled beat table with a `split` column ("train"/"test").
make_split_data <- function(labeled_dat, prop = 0.8, seed = 1234L) {
  set.seed(seed)

  sp <- rsample::initial_split(labeled_dat, prop = prop, strata = label)
  train_idx <- sp$in_id  # row indices of the training rows in labeled_dat

  # Force out the split so we can break the tidymodels workflow
  split_dat <-
    labeled_dat |>
    dplyr::mutate(
      split = if_else(dplyr::row_number() %in% train_idx, "train", "test")
    )

  # Return
  split_dat
}
