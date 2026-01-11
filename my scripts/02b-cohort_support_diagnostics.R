# In this script, we compute cohort-support diagnostics for the stacked DID analysis.
# Specifically, we count how many treatment cohorts contribute data at each relative time period.
# In more basic terms, for each relative time period (e.g., -6 months, +3 months), we count how many
# provinces have data at that relative time (i.e., their treatment date + relative time falls within the observed date range).
# This helps assess how well-supported each relative time period is in the stacked DID design.

library(tidyverse) 

setwd("..") # set your own working directory here

dynamic_path <- "output/results/stacked_did_dynamic_effects.csv"
out_dir <- "output/results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Function to extract cohort counts when n_g column is available (from stacked DiD)
extract_counts_with_ng <- function(dyn, rel_col) {
  dyn %>%
    select(all_of(rel_col), n_g) %>% 
    distinct() %>%
    rename(rel_time = all_of(rel_col))
}

# Function to extract cohort counts as fallback (count non-missing estimates)
extract_counts_fallback <- function(dyn, rel_col) {
  dyn %>%
    group_by(across(all_of(rel_col))) %>%
    summarise(n_g = sum(!is.na(estimate)), .groups = "drop") %>%
    rename(rel_time = all_of(rel_col))
}

# Main function to extract cohort counts, choosing method based on data
extract_cohort_counts_from_dynamic <- function(dyn, rel_col) {
  if("n_g" %in% names(dyn)){
    extract_counts_with_ng(dyn, rel_col)
  } else {
    extract_counts_fallback(dyn, rel_col)
  }
}

# Function to read dynamic effects data
read_dynamic_data <- function(dynamic_path) {
  read_csv(dynamic_path, show_col_types = FALSE)
}

# Function to process dynamic file and save cohort counts
process_dynamic_file <- function(dynamic_path, out_dir) {
  dyn <- read_dynamic_data(dynamic_path)
  rel_col <- "rel_time_num"
  cohort_counts <- extract_cohort_counts_from_dynamic(dyn, rel_col)
  write_csv(cohort_counts, file.path(out_dir, "cohort_support_counts.csv"))
}

load_treatment_dates <- function(treat_path) {
  treats <- read_csv(treat_path, show_col_types = FALSE)
  treats <- treats %>% mutate(treat_date = as.Date(treatment_date))
  treats}

get_date_range <- function(analysis_data_path) {
  df <- readRDS(analysis_data_path)
  min_date <- min(as.Date(df$date))
  max_date <- max(as.Date(df$date))
  list(min_date = min_date, max_date = max_date)}

count_provinces_at_rel_time <- function(treats, min_date, max_date, rt) {
  count <- sum((treats$treat_date + months(rt) >= min_date) & 
               (treats$treat_date + months(rt) <= max_date), na.rm = TRUE)
  data.frame(rel_time = rt, n_g = count)}

# Function to generate the sequence of relative time periods (-60 to 60 months)
generate_relative_time_sequence <- function() {
  seq(-60, 60)
}

# Function to compute counts for all relative times
compute_all_counts <- function(rel_times, treats, min_date, max_date) {
  counts <- lapply(rel_times, function(rt) {
    count_provinces_at_rel_time(treats, min_date, max_date, rt)
  })
  bind_rows(counts)
}

# Main function to compute cohort counts from treatment dates and date range
compute_cohort_counts_from_dates <- function(treats, min_date, max_date) {
  rel_times <- generate_relative_time_sequence()
  compute_all_counts(rel_times, treats, min_date, max_date)
}

# Function to load and process treatment dates
load_and_process_treatments <- function(treat_path) {
  load_treatment_dates(treat_path)
}

# Function to get date range from analysis dataset
get_analysis_date_range <- function(analysis_data_path) {
  get_date_range(analysis_data_path)
}

# Function to compute and save cohort counts from dates
compute_from_dates <- function(out_dir) {
  treat_path <- "data/01-raw_data/raw/treatment_dates.csv"
  treats <- load_and_process_treatments(treat_path)
  
  analysis_data_path <- "data/02-analysis_data/processed/analysis_dataset.rds"
  date_range <- get_analysis_date_range(analysis_data_path)
  
  counts_df <- compute_cohort_counts_from_dates(treats, date_range$min_date, date_range$max_date)
  write_csv(counts_df, file.path(out_dir, "cohort_support_counts_from_dates.csv"))
}

process_dynamic_file(dynamic_path, out_dir)