# Train/test split.
#
# v1 treats beats as independent observations and splits at the BEAT level
# (stratified on label so both classes appear in each side). This deliberately
# ignores that a patient contributes many beats across ECGs/time points -- a
# simplification to get the pipeline working. Patient-grouped splitting (no
# patient in both sides) is a later, more correct extension.
#
# Returns the labeled beat table with a `split` column ("train"/"test").
assign_split <- function(labeled_beats, prop = 0.8, seed = 1234L) {
  set.seed(seed)

  sp <- rsample::initial_split(labeled_beats, prop = prop, strata = label)
  train_idx <- sp$in_id  # row indices of the training rows in labeled_beats

  labeled_beats |>
    dplyr::mutate(
      split = ifelse(dplyr::row_number() %in% train_idx, "train", "test")
    )
}
