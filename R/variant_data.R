# Read in the data from the annotation files
# Should be in CSV format from reading them in from VCF files
# Built from card::read_vep_data()
# I/O = file/VEP data
read_in_annotated_vep_data <- function(file_name) {
  # Get number of lines to help with column typing
  n_lines <-
    vroom::vroom_lines(file_name) |>
    length()

  # Read in file
  dat <-
    vroom::vroom(
      file_name,
      guess_max = round(n_lines * 0.10, digits = 0)
    ) |>
    select(
      # Identifiers
      broad_id = sample_id,
      Location,
      SYMBOL,
      Gene,
      Feature,
      # Allele information
      Allele,
      Amino_acids,
      Protein_position,
      # Consequence and clinical significance
      Consequence,
      IMPACT,
      LoF,
      CLIN_SIG,
      SIFT,
      PolyPhen,
      # Population frequencies (if available)
      gnomADg_AF,
      gnomADe_AF,
      MAX_AF,
      # Other helpful annotations
      MANE_SELECT, # MANE prefered to CANONICAL
      CANONICAL
    ) |>
    # Trim down to DNA ID
    dplyr::mutate(broad_id = stringr::str_replace(broad_id, pattern = "CCDG_Broad_CVD_AF_Darbar_UIC_Cases-", replacement = "")) 

  # Return
  dat

}

filter_high_risk_variants <- function(vep_dat) {

  # Filter to canonical transcript to reduce per-transcript duplication
  dat <-
    vep_dat |>
    filter(CANONICAL == "YES") |>
    filter(
      !is.na(LoF) |
        IMPACT %in% c("HIGH", "MODERATE") |
        str_detect(CLIN_SIG, "pathogenic") |
        str_detect(
          Consequence,
          "stop_gained|missense_variant|frameshift_variant|splice_donor_variant|splice_acceptor_variant|start_lost"
        ) |
        str_detect(SIFT, "deleterious") |
        str_detect(PolyPhen, "damaging")
    ) |>
    # Keep rare variants; NA MAX_AF means absent from population databases
    # (i.e. ultra-rare), which we want to keep, not drop.
    filter(is.na(MAX_AF) | MAX_AF < 0.01) |>
    # Drop only variants explicitly flagged benign; keep unannotated (NA) ones.
    # str_detect(NA, ...) is NA and would otherwise be dropped, so coalesce to "".
    filter(!str_detect(coalesce(CLIN_SIG, ""), "benign"))

  # Return variant data
  dat
}


filter_by_gene <- function(vep_dat, gene = "all") {

  # Filter by gene
  # Could be pre-filtered by high risk or full dataset
  dat <-
    vep_dat |>
    # Now filter genes if specified. Default is "all" to keep all genes.
    (\(.x) {
      if (gene != "all") {
        filter(.x, SYMBOL %in% gene)
      } else {
        .x
      }
    }
    )()

  dat
}