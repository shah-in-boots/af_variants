clean_id_files <- function(id_files) {
  # Reconcile the three families of ID files into a single tidy roster
  # keyed on the UIC DNA ID (pattern "UIC####"):
  #
  #   1. broad-study-ids.xlsx  - master roster with DNA ID, study_id, mrn,
  #      and demographics. Authoritative source for MRN lookup — every
  #      sequenced UIC#### has a row here.
  #   2. SK-*.xls              - Broad Institute plate manifests. Provide
  #      the Broad "Sample ID" (SM-*) <-> UIC collaborator ID mapping, one
  #      row per well. Row 1 is the real header, row 2 a descriptor sub-
  #      header, data begins row 3. Three of the nine columns share the
  #      label "Alias" and must be renamed on read.
  #   3. redcap-ids.csv        - REDCap export with numeric dna_id padded
  #      here into UIC####. Used as a cross-check only: a handful of dna_id
  #      values map to two different MRNs in REDCap, so it is not safe to
  #      fall back on for the authoritative join.

  fnames <- basename(id_files)
  broad_master_file <- id_files[fnames == "broad-study-ids.xlsx"]
  redcap_file <- id_files[fnames == "redcap-ids.csv"]
  sk_files <- id_files[grepl("^SK-.*\\.xls$", fnames)]

  # Broad master roster: has MRN keyed on UIC DNA ID
  broad_master <- readxl::read_excel(broad_master_file) |>
    dplyr::rename(dna_id = `DNA ID`) |>
    dplyr::mutate(
      dna_id = stringr::str_trim(dna_id),
      mrn = as.character(mrn)
    )

  # REDCap export: pad numeric dna_id -> UIC####. Collapse to one row per
  # dna_id; drop rows where REDCap disagrees with itself on MRN so we do
  # not silently pick a wrong mapping.
  redcap <- vroom::vroom(redcap_file, show_col_types = FALSE) |>
    dplyr::transmute(
      dna_id = sprintf("UIC%04d", dna_id),
      redcap_study_id = study_id,
      redcap_mrn = as.character(mrn)
    ) |>
    dplyr::group_by(dna_id) |>
    dplyr::filter(dplyr::n_distinct(redcap_mrn) == 1) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  # Broad plate manifests: Sample ID (SM-*) <-> collaborator UIC DNA ID.
  sk_cols <- c(
    "position", "sample_id",
    "collab_participant_id", "collab_sample_id", "mass",
    "collab_sample_id_2", "gender", "sample_type",
    "collected_after_2015"
  )
  read_sk <- function(f) {
    readxl::read_excel(f, skip = 2, col_names = sk_cols) |>
      dplyr::filter(!is.na(sample_id), !is.na(collab_sample_id)) |>
      dplyr::transmute(
        plate = fs::path_ext_remove(basename(f)),
        position,
        sample_id,
        dna_id = stringr::str_trim(collab_sample_id),
        gender,
        sample_type
      )
  }
  sk <- purrr::map_dfr(sk_files, read_sk)

  # Merge: SK is the denominator (every sample actually sequenced by Broad,
  # including lab controls). Pull MRN and demographics from the master
  # roster; attach REDCap study_id as a cross-reference.
  ids <-
    sk |>
    dplyr::left_join(broad_master, by = "dna_id") |>
    dplyr::left_join(redcap, by = "dna_id") |>
    dplyr::select(
      dna_id, sample_id, mrn, study_id, redcap_study_id, redcap_mrn,
      plate, position, gender, sex, ethnicity, age, sample_type
    ) |>
    dplyr::arrange(dna_id) |>
    dplyr::select(dna_id, broad_id = sample_id, plate, position, mrn, redcap_id = study_id)

  # Return
  ids
}
