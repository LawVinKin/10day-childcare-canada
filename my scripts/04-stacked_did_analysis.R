# install.packages("did") # Uncomment if 'did' package is not installed
library(did)
library(tidyverse)

analysis_data <- read_rds("data/02-analysis_data/processed/analysis_data_stacked.rds")

yyyymm_to_month_num <- function(yyyymm) {
  year <- as.integer(yyyymm %/% 100)
  month <- as.integer(yyyymm %% 100)
  (year - 2019) * 12 + month
}

analysis_data <- analysis_data %>%
  mutate(
    period_seq = yyyymm_to_month_num(period),
    treatment_period_cohort_seq = yyyymm_to_month_num(treatment_period_cohort)
  )

run_stacked_did <- function(data, subgroup_name = "Full Sample") {
  att_results <- att_gt(
    yname = "in_lfp",
    tname = "period_seq",
    idname = "unit_id",
    gname = "treatment_period_cohort_seq",
    data = data,
    est_method = "dr",
    control_group = "notyettreated",
    panel = FALSE,
    clustervar = "prov"
  )
  
  att_agg_simple <- aggte(att_results, type = "simple")
  
  estimate_val <- att_agg_simple$overall.att
  se_val <- att_agg_simple$overall.se
  t_stat <- estimate_val / se_val
  p_val <- 2 * pnorm(-abs(t_stat))
  
  tibble(
    subgroup = subgroup_name,
    estimate = estimate_val,
    se = se_val,
    p_value = p_val,
    ci_lower = estimate_val - 1.96 * se_val,
    ci_upper = estimate_val + 1.96 * se_val,
    n_observations = nrow(data)
  )
}

run_stacked_did_dynamic <- function(data) {
  att_results <- att_gt(
    yname = "in_lfp",
    tname = "period_seq",
    idname = "unit_id",
    gname = "treatment_period_cohort_seq",
    data = data,
    est_method = "dr",
    control_group = "notyettreated",
    panel = FALSE,
    clustervar = "prov"
  )
  
  att_agg_dynamic <- aggte(att_results, type = "dynamic")
  
  tibble(
    rel_time_num = att_agg_dynamic$egt,
    estimate = att_agg_dynamic$att.egt,
    se = att_agg_dynamic$se.egt
  ) %>%
    mutate(
      t_stat = estimate / se,
      p_value = 2 * pnorm(-abs(t_stat)),
      conf.low = estimate - 1.96 * se,
      conf.high = estimate + 1.96 * se
    ) %>%
    select(-t_stat)
}

all_stacked_results <- bind_rows(
  run_stacked_did(analysis_data, "Full Sample"),
  run_stacked_did(analysis_data %>% filter(lone_parent == 1), "Lone Parent"),
  run_stacked_did(analysis_data %>% filter(lone_parent == 0), "Not Lone Parent"),
  run_stacked_did(analysis_data %>% filter(educ_high == 1), "High Education"),
  run_stacked_did(analysis_data %>% filter(educ_high == 0), "Low Education"),
  run_stacked_did(analysis_data %>% filter(age_younger == 1), "Younger (25-34)"),
  run_stacked_did(analysis_data %>% filter(age_younger == 0), "Older (35-44)")
)

dynamic_effects <- run_stacked_did_dynamic(analysis_data)

dir.create("output/results", showWarnings = FALSE, recursive = TRUE)
write_csv(all_stacked_results, "output/results/stacked_did_results_summary.csv")
write_csv(dynamic_effects, "output/results/stacked_did_dynamic_effects.csv")