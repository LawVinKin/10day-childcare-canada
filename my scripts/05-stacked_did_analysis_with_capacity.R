# this script performs the stacked DiD but does so with
# childcare capacity subgroups included (these are simply "high capacity" and "low capacity")
library(did)
library(tidyverse)

source("my scripts/04-stacked_did_analysis.R")

load_and_prepare_data <- function() {
  analysis_data <- read_rds("data/03-analysis_data/processed/analysis_data_stacked_with_capacity.rds")
  analysis_data <- analysis_data %>%
    mutate(
      period_seq = yyyymm_to_month_num(period),
      treatment_period_cohort_seq = yyyymm_to_month_num(treatment_period_cohort)
    )
  analysis_data
}

run_all_subgroup_analyses <- function(data) {
  bind_rows(
    run_stacked_did(data, "Full Sample"),
    run_stacked_did(data %>% filter(lone_parent == 1), "Lone Parent"),
    run_stacked_did(data %>% filter(lone_parent == 0), "Not Lone Parent"),
    run_stacked_did(data %>% filter(educ_high == 1), "High Education"),
    run_stacked_did(data %>% filter(educ_high == 0), "Low Education"),
    run_stacked_did(data %>% filter(age_younger == 1), "Younger (25-34)"),
    run_stacked_did(data %>% filter(age_younger == 0), "Older (35-44)"),
    run_stacked_did(data %>% filter(baseline_capacity_cat == "Low Capacity (2019)"), "Low Capacity (2019)"),
    run_stacked_did(data %>% filter(baseline_capacity_cat == "High Capacity (2019)"), "High Capacity (2019)")
  )
}
# this script runs stacked DiD analyses for various subgroups,
# including childcare capacity subgroups ("high capacity" and "low capacity")
analysis_data <- load_and_prepare_data()
all_stacked_results_with_capacity <- run_all_subgroup_analyses(analysis_data)

write_csv(all_stacked_results_with_capacity, "output/results/stacked_did_results_summary_with_capacity.csv")