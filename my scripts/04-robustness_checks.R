# This script runs all robustness checks reported in the paper:
# 1. Placebo test (June 2020 fake treatment date)
# 2. Placebo test excluding Quebec
# 3. Cohort 39 sensitivity analysis

library(tidyverse)
library(did)
library(fixest)
library(broom)

source("my scripts/06-utilities.R")

### Main function

run_robustness_checks <- function(force = FALSE) {

  # Load analysis data
  df <- readr::read_rds("data/03-analysis_data/processed/analysis_data_stacked.rds")
  df <- prepare_did_data(df)
  
   
  # Placebo test: June 2020 fake treatment date
  # Tests if we detect "effects" when using a fake pre-treatment date
  
  placebo_path <- "output/results/placebo_tests_summary.csv"
  
  placebo_res <- get_or_run_csv(placebo_path, function() {
    
    # Keep only pre-treatment period
    df_placebo <- df %>%
      filter(date < as.Date("2021-11-01")) %>%
      mutate(
        fake_treatment_date = as.Date("2020-06-01"),
        fake_post = if_else(date >= fake_treatment_date, 1, 0),
        fake_treatment_dummy = as.integer(is_treated_province) * fake_post
      )
    
    # Run TWFE regression with province FE
    model_placebo <- fixest::feols(
      in_lfp ~ fake_treatment_dummy | prov,
      data = df_placebo,
      cluster = ~prov
    )
    
    # Extract results
    tibble(
      test_type = "Pre-treatment placebo (2020-06)",
      estimate = coef(model_placebo)["fake_treatment_dummy"],
      se = sqrt(vcov(model_placebo)["fake_treatment_dummy", "fake_treatment_dummy"]),
      p_value = summary(model_placebo)$coeftable["fake_treatment_dummy", "Pr(>|t|)"],
      interpretation = ifelse(
        summary(model_placebo)$coeftable["fake_treatment_dummy", "Pr(>|t|)"] > 0.05,
        "PASS: No significant pre-trend",
        "FAIL: Pre-trend detected"
      )
    )
    
  }, force = force)

  # Placebo test excluding Quebec
  # Tests if placebo effect disappears when Quebec is removed
  
  placebo_qc_path <- "output/results/placebo_excl_quebec.csv"
  
  placebo_qc_res <- get_or_run_csv(placebo_qc_path, function() {
    
    # Remove Quebec and keep pre-treatment period
    df_placebo_qc <- df %>%
      filter(prov != 24, date < as.Date("2021-11-01")) %>%
      mutate(
        fake_treatment_date = as.Date("2020-06-01"),
        fake_post = if_else(date >= fake_treatment_date, 1, 0),
        fake_treatment_dummy = as.integer(is_treated_province) * fake_post
      )
    
    # Run TWFE regression
    model_placebo_qc <- fixest::feols(
      in_lfp ~ fake_treatment_dummy | prov,
      data = df_placebo_qc,
      cluster = ~prov
    )
    
    # Extract results
    tibble(
      test_type = "Placebo excl. Quebec (2020-06)",
      estimate = coef(model_placebo_qc)["fake_treatment_dummy"],
      se = sqrt(vcov(model_placebo_qc)["fake_treatment_dummy", "fake_treatment_dummy"]),
      p_value = summary(model_placebo_qc)$coeftable["fake_treatment_dummy", "Pr(>|t|)"],
      interpretation = ifelse(
        summary(model_placebo_qc)$coeftable["fake_treatment_dummy", "Pr(>|t|)"] > 0.05,
        "PASS: Not significant",
        "FAIL: Significant"
      )
    )
    
  }, force = force)

  # Cohort 39 sensitivity (with vs without March 2022 cohort)
  # Tests if results are driven by the March 2022 cohort
  
  # Full sample dynamic effects
  dynamic_full_path <- "output/results/stacked_did_dynamic_effects.csv"
  
  dynamic_full <- get_or_run_csv(dynamic_full_path, function() {
    
    att <- att_gt(
      yname = "in_lfp",
      tname = "period_seq",
      idname = "unit_id",
      gname = "treatment_period_cohort_seq",
      data = df,
      est_method = "dr",
      control_group = "notyettreated",
      panel = FALSE,
      clustervar = "prov"
    )
    
    ad <- aggte(att, type = "dynamic")
    
    tibble(
      rel_time_num = ad$egt,
      estimate = ad$att.egt,
      se = ad$se.egt,
      p_value = 2 * pnorm(-abs(estimate / se)),
      conf.low = estimate - 1.96 * se,
      conf.high = estimate + 1.96 * se
    )
    
  }, force = force)
  
  # Dynamic effects excluding cohort 39 (March 2022)
  dynamic_no39_path <- "output/results/stacked_did_dynamic_effects_no_cohort39.csv"
  
  dynamic_no39 <- get_or_run_csv(dynamic_no39_path, function() {
    
    att <- att_gt(
      yname = "in_lfp",
      tname = "period_seq",
      idname = "unit_id",
      gname = "treatment_period_cohort_seq",
      data = filter(df, treatment_period_cohort_seq != 39),
      est_method = "dr",
      control_group = "notyettreated",
      panel = FALSE,
      clustervar = "prov"
    )
    
    ad <- aggte(att, type = "dynamic")
    
    tibble(
      rel_time_num = ad$egt,
      estimate = ad$att.egt,
      se = ad$se.egt,
      p_value = 2 * pnorm(-abs(estimate / se)),
      conf.low = estimate - 1.96 * se,
      conf.high = estimate + 1.96 * se
    )
    
  }, force = force)
  
  # Compare months 9, 10, 11 (key post-treatment periods)
  cohort39_comparison <- dynamic_full %>%
    filter(rel_time_num %in% c(9, 10, 11)) %>%
    select(rel_time_num, estimate_with = estimate, se_with = se) %>%
    left_join(
      dynamic_no39 %>%
        filter(rel_time_num %in% c(9, 10, 11)) %>%
        select(rel_time_num, estimate_without = estimate, se_without = se),
      by = "rel_time_num"
    ) %>%
    mutate(
      diff_estimate = estimate_with - estimate_without,
      diff_se = se_with - se_without
    )
  
  write_csv(cohort39_comparison, "output/results/cohort39_months_9_10_11.csv")

  # Return all results as a list
  invisible(list(
    placebo = placebo_res,
    placebo_excl_qc = placebo_qc_res,
    cohort39_comparison = cohort39_comparison
  ))

}

# The following line runs the robustness checks when this script is sourced. Set force = TRUE to re-run all checks even if results already exist.
invisible(run_robustness_checks(force = FALSE))
