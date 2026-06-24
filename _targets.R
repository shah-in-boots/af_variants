# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)

# Source the target files in the R folder
tar_source()

# Set target-specific options such as packages.
tar_option_set(
	packages = c(
		# Infrastructure
		"targets",
		"tarchetypes",
		"crew",
		"here",
    "fs",
		"arrow",
    # Basic packages
    "tidyverse",
		"vroom",
    # Modeling
		"rsample",
		"yardstick",
		"keras3",
		"tensorflow",
    # Personal
		"card",
		"EGM"
	),
	controller = crew_controller_local(
		workers = 6,
		seconds_idle = 3000
	),
	garbage_collection = TRUE,
	workspace_on_error = TRUE,
	storage = "worker",
	retrieval = "worker"
)


# Target list
list(
	# Data ----

	# Data path
	tar_file(data_dir, fs::path(fs::path_home(), "data", "af_variants")),

	# Genetics folder
	tar_file(genetics_dir, fs::path(data_dir, "genetic_data")),
	

	# ECG datasets
	tar_file(ecg_dir, fs::path(data_dir, "ecg_data")),
	tar_file(wfdb_dir, fs::path(ecg_dir, "raw")),
	tar_file(beat_dir, fs::path(ecg_dir, "beats")),

	# Where trained keras models are written
	tar_file(model_dir, fs::path(data_dir, "models")),

	# Clinical and ID information folder
	# ID reconciliation 
	# Has a table of ECG information that is contained in the ecg_data/raw folder
	tar_file(id_dir, fs::path(data_dir, "id_data")),
	tar_files(name = id_files, command = fs::dir_ls(id_dir)),
	tar_target(ids, clean_id_files(id_files)),

	# VEP annotations ----

	# VEP files
	tar_file(
		uic_first_batch_file,
		fs::path(genetics_dir, "uic_first_batch", "vep_annotations.csv")
	),
	tar_file(
		uic_second_batch_file,
		fs::path(genetics_dir, "uic_second_batch", "vep_annotations.csv")
	),

	# VEP data (branched)
	tar_target(
		vep_files,
		c(uic_first_batch_file, uic_second_batch_file)
	),

	# Creates a single data file of ALL annotations
	# Can also filter down by high risk variants
	tar_target(
		vep_batch_dat,
		read_in_annotated_vep_data(vep_files),
		pattern = map(vep_files)
	),
	tar_target(vep_dat, dplyr::bind_rows(vep_batch_dat)),
	tar_target(variant_dat, filter_high_risk_variants(vep_dat)),

	# TTN data
	tar_target(ttn_all_dat, filter_by_gene(vep_dat, gene = "TTN")),
	tar_target(ttn_var_dat, filter_high_risk_variants(ttn_all_dat)),

	# ECG data ----

	# Enumerate the WFDB records available in the raw directory (one per ECG)
	tar_target(ecg_list, list_ecg_records(wfdb_dir)),

	# Group records into evenly sized batches so the pipeline branches over a
	# few hundred batches instead of ~15k individual records. `iteration =
	# "list"` makes each branch receive one batch (a character vector).
	tar_target(
		ecg_batches,
		batch_records(ecg_list, batch_size = 1000L),
		iteration = "list"
	),

	# Window each ECG into standardized individual sinus beats, branched per
	# batch. Each branch processes its batch of ECGs, writes their beats to
	# beat_dir, and returns a manifest; targets row-binds the branches.
	tar_target(
		beat_paths,
		make_individual_beats(ecg_batches, wfdb_dir, beat_dir),
		pattern = map(ecg_batches)
	),

	# Match each ECG to the genetic data and de-identified numbers
	tar_target(
		ecg_ids,
		match_ecg_to_genetics(ids, ecg_dir)
	),

	# One row per written beat, named and joined to its parent ECG metadata and
	# the patient's genetic/ID information (genetics-matched beats only)
	tar_target(
		beat_table,
		build_beat_table(beat_paths, ecg_ids)
	),

	# Modeling ----


	# Case/control label per beat. Cases = carriers of a TTN variant matching the
	# supplied criteria; controls = remainder. Pass `ttn_all_dat` (unfiltered) so
	# the definition lives entirely in these arguments -- tune them to trade off
	# case count vs. stringency (e.g. lof=TRUE is strictest but yields ~1 case
	# with beats; the default below gives ~46 case patients).
	tar_target(
		labeled_beats,
		assign_case_control(
			beat_table, ttn_all_dat,
			impact = c("HIGH", "MODERATE"), max_af = 0.01, canonical = TRUE
		)
	),

	# Beat-level train/test split (v1 treats beats as independent; no patient
	# grouping yet)
	tar_target(
		beat_split,
		assign_split(labeled_beats)
	),

	# Fit the tiny CNN; returns the path to the saved .keras model (file target)
	tar_target(
		fit_cnn,
		fit_model(beat_split, model_name = "cnn_small", out_dir = model_dir),
		format = "file"
	),

	# Beat-level out-of-sample predictions on the held-out patients
	tar_target(
		pred_cnn,
		predict_beats(fit_cnn, beat_split)
	),

	# Beat-level metrics (ROC-AUC, PR-AUC)
	tar_target(
		metrics_cnn,
		evaluate_beats(pred_cnn)
	)
)
