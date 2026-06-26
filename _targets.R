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
	# These are all targets and not tracked files to save time
	# Do not need to read each file
	tar_target(data_dir, fs::path(fs::path_home(), "data", "af_variants")),

	# Genetics folder
	tar_target(genetics_dir, fs::path(data_dir, "genetic_data")),

	# ECG datasets
	tar_target(ecg_dir, fs::path(data_dir, "ecg_data")),
	tar_target(wfdb_dir, fs::path(ecg_dir, "raw")),
	tar_target(beat_dir, fs::path(ecg_dir, "beats")),

	# Trained keras models storage folder
	tar_target(model_dir, fs::path(data_dir, "models")),

	# Where the evaluation phase writes the browsable metrics table
	tar_target(metrics_dir, fs::path(data_dir, "metrics")),

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
		label_case_control_status(
			beat_table, 
			variant_dat = ttn_var_dat,
			'IMPACT == "HIGH"'
		)
	),

	# Beat-level train/test split (v1 treats beats as independent; no patient
	# grouping yet)
	tar_target(
		split_dat,
		make_split_data(labeled_beats)
	),

	# Models ----
	#
	# Every model to train is one entry in the list below -- edit its
	# `architecture` / `hp` / `fit` right here in the pipeline. We dynamically
	# branch over the list, so each entry trains as its own branch (in parallel
	# across the crew workers) and writes a self-contained `<name>.keras` (named
	# after its full spec) into `model_dir`. train_ecg_model() returns early when
	# that `.keras` already exists, so `tar_make()` only trains entries you have
	# added or whose hyperparameters you changed -- add a model, rerun, and just
	# the new one builds. To force a retrain, delete its file from `model_dir`.

	tar_target(model_epochs, 30L),
	tar_target(model_batch_size, 128L),

	tar_target(
		model_specs,
		list(
			list(
				architecture = "cnn",
				hp = list(
					filters = 32, 
					kernel_size = 7, 
					n_blocks = 7,
					dense_units = 64, 
					dropout = 0.3, 
					learning_rate = 1e-3
				),
				fit = list(
					epochs = model_epochs, 
					batch_size = model_batch_size, 
					validation_split = 0.15
				)
			),
			list(
				architecture = "resnet",
				hp = list(
					filters = 32, 
					kernel_size = 7, 
					n_blocks = 7,
					dense_units = 64, 
					dropout = 0.3, 
					learning_rate = 1e-3
				),
				fit = list(
					epochs = model_epochs, 
					batch_size = model_batch_size, 
					validation_split = 0.15
				)
			),
			list(
				architecture = "cnn_lstm",
				hp = list(
					filters = 64, 
					kernel_size = 7, 
					n_conv_blocks = 2,
					lstm_units = 64, 
					dense_units = 64, 
					dropout = 0.3,
					learning_rate = 1e-3
				),
				fit = list(
					epochs = model_epochs, 
					batch_size = model_batch_size, 
					validation_split = 0.15
				)
			),
			list(
				architecture = "tcn",
				hp = list(
					filters = 32,
					kernel_size = 7,
					dilations = c(1, 2, 4, 8, 16, 32, 64),
					dense_units = 64,
					dropout = 0.3,
					learning_rate = 1e-3
				),
				fit = list(
					epochs = model_epochs,
					batch_size = model_batch_size,
					validation_split = 0.15
				)
			)
		),
		iteration = "list"
	),

	tar_target(
		models,
		train_ecg_model(
			split_dat,
			model_dir,
			architecture = model_specs$architecture,
			hp = model_specs$hp,
			fit = model_specs$fit
		),
		pattern = map(model_specs),
		format = "file"
	),

	# Evaluation ----
	#
	# Enumerate EVERY `.keras` in the model directory -- not just the ones in
	# `model_specs` -- so the table covers older runs and hand-dropped models too,
	# matching "evaluate the models stored in the model directory". Referencing
	# `models` makes this re-scan after training, so a model trained this run is
	# discovered this run. tar_files tracks each file by content hash, so eval
	# below only re-runs for files that actually changed.
	tar_files(
		model_files,
		{
			models # depend on training so freshly trained models are picked up now
			as.character(fs::dir_ls(model_dir, glob = "*.keras"))
		}
	),

	# Score every model on the held-out TEST split. This phase only ever LOADS
	# `.keras` files and predicts -- it never retrains. One branch per model file
	# (in parallel across the crew workers); targets caches each branch, so adding
	# a model just grows the table by a row. score_model() streams test beats
	# through the model in chunks, so peak memory is one (chunk, 500, 12) array no
	# matter how large the test set is.
	tar_target(
		model_scores,
		score_model(model_files, split_dat),
		pattern = map(model_files)
	),

	# The growing table of model statistics -- one row per model, best ROC-AUC
	# first. Inspect with `tar_read(model_metrics)`.
	tar_target(
		model_metrics,
		dplyr::arrange(model_scores, dplyr::desc(roc_auc))
	),

	# A browsable CSV copy of the table (for spreadsheets / read_csv outside the
	# pipeline). The in-pipeline `model_metrics` target above is the source of
	# truth; this is just a convenience export.
	tar_file(
		model_metrics_file,
		write_metrics_table(model_metrics, metrics_dir)
	)

)
