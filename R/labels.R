# Case/control labeling for ECG beat classification.
#
# A case/control definition is just a set of variant-filter arguments. A patient
# is a CASE if they carry at least one variant in `variant_dat` that passes ALL
# of the supplied criteria; every other genotyped patient (those with a
# `broad_id` in `beat_table`) is a CONTROL. Turning a criterion off (its default)
# makes it a no-op, so definitions compose freely, e.g.
#
#   assign_case_control(beat_table, ttn_dat, lof = TRUE)
#   assign_case_control(beat_table, ttn_dat, impact = "HIGH", max_af = 0.01)
#   assign_case_control(beat_table, ttn_dat, pathogenic = TRUE, consequence = "frameshift")
#
# `variant_dat` should be the gene-scoped, UNfiltered annotations (e.g.
# ttn_all_dat) so the definition lives entirely in these arguments rather than
# in a pre-baked filter.

# Return the broad_ids of patients who carry >=1 variant passing all criteria.
define_case_ids <- function(variant_dat,
                            canonical = TRUE,    # restrict to canonical transcript
                            lof = FALSE,         # LOFTEE high-confidence LoF (LoF == "HC")
                            pathogenic = FALSE,  # CLIN_SIG contains "pathogenic"
                            impact = NULL,       # keep IMPACT %in% impact, e.g. c("HIGH","MODERATE")
                            consequence = NULL,  # keep if Consequence matches any of these terms
                            max_af = NULL,       # keep MAX_AF < max_af (NA == absent == kept)
                            exclude_benign = FALSE) {
  v <- variant_dat

  if (canonical)            v <- dplyr::filter(v, CANONICAL == "YES")
  if (lof)                  v <- dplyr::filter(v, !is.na(LoF) & LoF == "HC")
  if (pathogenic)           v <- dplyr::filter(v, stringr::str_detect(dplyr::coalesce(CLIN_SIG, ""), "pathogenic"))
  if (exclude_benign)       v <- dplyr::filter(v, !stringr::str_detect(dplyr::coalesce(CLIN_SIG, ""), "benign"))
  if (!is.null(impact))     v <- dplyr::filter(v, IMPACT %in% impact)
  if (!is.null(consequence)) v <- dplyr::filter(v, stringr::str_detect(Consequence, paste(consequence, collapse = "|")))
  if (!is.null(max_af))     v <- dplyr::filter(v, is.na(MAX_AF) | MAX_AF < max_af)

  unique(stats::na.omit(v$broad_id))
}

# Add a binary `label` to every beat (1 = case, 0 = control) using the variant
# criteria above. Cases are variant carriers; controls are the remainder.
assign_case_control <- function(beat_table, variant_dat, ...) {
  case_ids <- define_case_ids(variant_dat, ...)
  dplyr::mutate(beat_table, label = as.integer(broad_id %in% case_ids))
}
