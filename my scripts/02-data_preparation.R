# This script prepares the analysis dataset by merging cleaned LFS data with treatment indicators
# and creating subgroup variables for heterogeneous effects analysis.
# It ensures that the final dataset is ready for regression analysis and can be easily loaded in subsequent scripts.

library(tidyverse)

source("my scripts/06-utilities.R")

ANALYSIS_RDS <- "data/02-processed/analysis_dataset.rds"
FINAL_RDS <- "data/03-analysis_data/processed/analysis_data_stacked.rds"

### Main function that runs the data preparation steps.
### Set force = TRUE to re-run all steps and overwrite existing files.

run_prepare_data <- function(force = FALSE) {

  # Load cleaned LFS data and treatment dates
  dat <- load_data()
  prov_map <- get_province_code_map()
  treat_sel <- process_treatments(dat$treatment, prov_map)

  # Merge LFS with treatment indicators
  analysis_data <- join_and_create_variables(dat$lfs, treat_sel)
  final_analysis <- select_final_columns(analysis_data)

  # Save basic analysis dataset
  get_or_run_rds(ANALYSIS_RDS, function() {
    dir.create(dirname(ANALYSIS_RDS), recursive = TRUE, showWarnings = FALSE)
    final_analysis
  }, force = force)

  # Add subgroup indicators
  analysis_with_subgroups <- create_subgroup_indicators(final_analysis)

  # Save final stacked analysis dataset
  get_or_run_rds(FINAL_RDS, function() {
    dir.create(dirname(FINAL_RDS), recursive = TRUE, showWarnings = FALSE)
    analysis_with_subgroups
  }, force = force)

  invisible(analysis_with_subgroups)
}
