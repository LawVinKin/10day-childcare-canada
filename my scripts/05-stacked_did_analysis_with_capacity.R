library(did)
library(tidyverse)

source("my scripts/04-stacked_did_analysis.R")

analysis_data <- read_rds("data/02-analysis_data/processed/analysis_data_stacked_with_capacity.rds")

analysis_data <- analysis_data %>%
  mutate(
    period_seq = yyyymm_to_month_num(period),
    treatment_period_cohort_seq = yyyymm_to_month_num(treatment_period_cohort)
  )

all_stacked_results_with_capacity <- bind_rows(
  run_stacked_did(analysis_data, "Full Sample"),
  run_stacked_did(analysis_data %>% filter(lone_parent == 1), "Lone Parent"),
  run_stacked_did(analysis_data %>% filter(lone_parent == 0), "Not Lone Parent"),
  run_stacked_did(analysis_data %>% filter(educ_high == 1), "High Education"),
  run_stacked_did(analysis_data %>% filter(educ_high == 0), "Low Education"),
  run_stacked_did(analysis_data %>% filter(age_younger == 1), "Younger (25-34)"),
  run_stacked_did(analysis_data %>% filter(age_younger == 0), "Older (35-44)"),
  run_stacked_did(analysis_data %>% filter(baseline_capacity_cat == "Low Capacity (2019)"), "Low Capacity (2019)"),
  run_stacked_did(analysis_data %>% filter(baseline_capacity_cat == "High Capacity (2019)"), "High Capacity (2019)")
)

write_csv(all_stacked_results_with_capacity, "output/results/stacked_did_results_summary_with_capacity.csv")