# Train/validation/test split.
#
# We split at the BEAT level, stratified only on the case/control `label`. Beats
# from the same patient may land in different sets, and that is fine here: the
# goal is to learn the morphology/properties of ECG beats, not to identify
# individual patients, so patient overlap across sets is acceptable. The only
# things this split controls are the set proportions and -- via stratification --
# keeping the (rare) case beats represented in all three sets.
#
# Three disjoint sets (by beat):
#   * train -- model fitting (R/train.R)
#   * val   -- early stopping / LR scheduling, built explicitly here instead of
#              keras's `validation_split`, which would otherwise carve an
#              unshuffled, unstratified tail out of the training array
#   * test  -- final scoring (R/evaluate.R); never seen during training
#
# Returns the labeled beat table with a `split` column ("train"/"val"/"test").
make_split_data <- function(
  labeled_dat,
  prop_train = 0.7,
  prop_val = 0.15,
  seed = 1234L
) {
  set.seed(seed)

  # A temporary row id makes the split assignment robust to any row identity, and
  # `.strata` stratifies on the (rare) case label so cases land in all three sets.
  dat <- labeled_dat |>
    dplyr::mutate(.row = dplyr::row_number(), .strata = factor(label))

  # Peel off the test beats first, then split the remainder into train/val.
  prop_trainval <- prop_train + prop_val
  test_split <- rsample::initial_split(
    dat,
    prop = prop_trainval,
    strata = .strata
  )
  trainval <- rsample::training(test_split)
  test <- rsample::testing(test_split)

  val_split <- rsample::initial_split(
    trainval,
    prop = prop_train / prop_trainval,
    strata = .strata
  )
  train <- rsample::training(val_split)
  val <- rsample::testing(val_split)

  # Map each beat's split assignment back onto the original table.
  labeled_dat |>
    dplyr::mutate(
      split = dplyr::case_when(
        dplyr::row_number() %in% train$.row ~ "train",
        dplyr::row_number() %in% val$.row ~ "val",
        dplyr::row_number() %in% test$.row ~ "test"
      )
    )
}
