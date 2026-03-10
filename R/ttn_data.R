# Read in the data from the annotation files
# Should be in CSV format from reading them in from VCF files
# Built from card::read_vep_data()
# I/O = file/VEP data
read_in_vep_data <- function(file_name) {
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
      sample_id,
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
    )

  # Filter to canonical transcript to reduce per-transcript duplication
  variant_dat <-
    dat |>
    filter(CANONICAL == "YES") |>
    filter(
      !is.na(LoF) |
        IMPACT == "HIGH|MODERATE" |
        str_detect(CLIN_SIG, "pathogenic") |
        str_detect(
          Consequence,
          "stop_gained|missense_variant|frameshift_variant|splice_donor_variant|splice_acceptor_variant|start_lost"
        ) |
        str_detect(SIFT, "deleterious") |
        str_detect(PolyPhen, "damaging")
    ) |>
    filter(MAX_AF < 0.01) |>
    filter(!str_detect(CLIN_SIG, "benign"))


  # Return variant data
  variant_dat
}