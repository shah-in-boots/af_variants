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
    # Basic packages
    "tidyverse",
		"vroom",
    # Personal
		"card"
	),
	controller = crew_controller_local(
		workers = 6,
		seconds_idle = 3000
	),
	workspace_on_error = TRUE,
	storage = "worker",
	retrieval = "worker"
)


# Target list
list(
	# Data ----
	# Paths
	tar_file(data_dir, fs::path(fs::path_home(), "data")),
	tar_file(genetics_dir, fs::path(data_dir, "genetics")),

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
		vep_file,
		c(uic_first_batch_file, uic_second_batch_file)
	),

	tar_target(
		vep_batch_dat,
		read_in_annotated_vep_data(vep_file),
		pattern = map(vep_file)
	),

	# Creates a single data file of high riskk annotations
	tar_target(vep_dat, dplyr::bind_rows(vep_batch_dat))
)
