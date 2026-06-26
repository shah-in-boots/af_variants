# Case/control labeling for ECG beat classification.
#
# A case/control definition is a set of variant filters written as ordinary R
# expressions (passed as strings). A patient is a CASE if they carry >=1 variant
# in `variant_dat` that passes ALL of the filters; every other genotyped patient
# (a `broad_id` present in `beat_table`) is a CONTROL.
#
# Filters are variadic (`...`) and evaluated against the variant columns with
# dplyr data-masking, so anything that works inside dplyr::filter() works here.
# Convention:
#   * each `...` argument (and each `;`/newline-separated clause) is AND-combined
#   * use `|`, `&`, %in%, str_detect(), grepl(), <, ... freely *within* a clause
#
# Equality is exact: `IMPACT == "HIGH|MODERATE"` matches the literal string and
# never fires. For set or regex matching use one of:
#   IMPACT %in% c("HIGH", "MODERATE")
#   str_detect(IMPACT, "HIGH|MODERATE")
#
# str_detect() on an NA cell returns NA, so for *negation* filters guard the NA
# (else those rows are dropped), e.g. !str_detect(coalesce(CLIN_SIG, ""), "benign").
#
# Examples (pass ttn_all_dat -- the UNfiltered, gene-scoped annotations -- so the
# whole definition lives in these arguments):
#   assign_case_control(beats, ttn_all_dat, 'LoF == "HC"')
#   assign_case_control(beats, ttn_all_dat, 'IMPACT %in% c("HIGH","MODERATE")', 'MAX_AF < 0.01')
#   assign_case_control(beats, ttn_all_dat, 'str_detect(Consequence, "frameshift|stop_gained")')
#   assign_case_control(beats, ttn_all_dat, 'LoF == "HC" | str_detect(CLIN_SIG, "pathogenic")')

# Add a binary `label` to every beat (1 = case, 0 = control) using the variant
# filters above. A patient is a CASE if they carry >=1 variant passing ALL
# filters; every other genotyped patient is a CONTROL.
label_case_control_status <- function(
  beat_table,
  variant_dat,
  ...
) {
  filters <- c(...) # character vector of filter expressions (may be empty)

  # parse_exprs() returns an expression, splitting on new lines
  # We place the new lines to split the quoted dot argument
  if (length(filters)) {
    exprs <-
      filters |>
      paste(collapse = "\n") |>
      rlang::parse_exprs()
    variant_dat <- dplyr::filter(variant_dat, !!!exprs)
  }

  # Get the Broad IDs to identify cases
  case_ids <- unique(variant_dat$broad_id)

  labeled_beat_table <- 
    beat_table |>
    mutate(label = as.integer(broad_id %in% case_ids))

  # Return
  labeled_beat_table
}
