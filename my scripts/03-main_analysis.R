# This script performs the main analysis of the study, estimating treatment effects of
# the childcare subsidy on maternal labour market outcomes using a stacked difference-in-differences approach.

library(did)
library(tidyverse)

source("my scripts/06-utilities.R")

# Output paths
DYN_PATH <- "output/results/stacked_did_dynamic_effects.csv"
DYN_NO39_PATH <- "output/results/stacked_did_dynamic_effects_no_cohort39.csv"
OUTCOMES_PATH <- "output/results/multiple_outcomes_results.csv"
SUBGROUP_PATH <- "output/results/expanded_subgroup_results.csv"
COHORT_COUNTS_PATH <- "output/results/cohort_support_counts.csv"


### Main function

run_main_analysis <- function(force = FALSE) {

  # Load prepared data
  analysis_data <- read_rds("data/03-analysis_data/processed/analysis_data_stacked.rds")
  analysis_data <- prepare_did_data(analysis_data)

  # Define outcomes to analyze
  outcome_list <- list(
    "in_lfp" = "Labor Force Participation",
    "in_employed" = "Employment",
    "in_ft_employed" = "Full-Time Employment",
    "in_pt_employed" = "Part-Time Employment",
    "hours_usual" = "Usual Hours Worked",
    "hours_total" = "Total Hours Worked",
    "earnings_hourly" = "Hourly Earnings"
  )

  # Define subgroups for heterogeneous effects
  subgroup_list <- list(
    "lone_parent" = list(name = "Family Structure", groups = list("1" = "Lone Parent", "0" = "Not Lone Parent")),
    "educ_high" = list(name = "Education", groups = list("1" = "High Education", "0" = "Low Education")),
    "age_younger" = list(name = "Age Group", groups = list("1" = "Younger (25-34)", "0" = "Older (35-44)")),
    "student_status" = list(name = "Student Status", groups = list("Non-student" = "Non-Student", "Full-time student" = "Full-time Student", "Part-time student" = "Part-time Student")),
    "class_of_worker" = list(name = "Class of Worker", groups = list("Public sector" = "Public Sector", "Private sector" = "Private Sector", "Self-employed" = "Self-Employed")),
    "immigrant_status" = list(name = "Immigrant Status", groups = list("Non-Immigrant" = "Non-Immigrant", "Immigrant" = "Immigrant")),
    "marital_status" = list(name = "Marital Status", groups = list("Married/Common-law" = "Married/Common-law", "Separated/Divorced/Widowed" = "Separated/Divorced/Widowed", "Single (never married)" = "Single"))
  )
  
  # Main treatment effects (7 outcomes)
  
  outcomes_results <- map2_df(names(outcome_list), outcome_list, function(var, label) {
    
    df <- filter(analysis_data, !is.na(.data[[var]]))
    
    if (nrow(df) < 1000) {
      return(tibble(
        outcome = label,
        estimate = NA_real_,
        se = NA_real_,
        p_value = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        n_observations = nrow(df),
        note = "Insufficient observations"
      ))
    }
    
    # Run stacked DiD for this outcome
    att <- att_gt(
      yname = var,
      tname = "period_seq",
      idname = "unit_id",
      gname = "treatment_period_cohort_seq",
      data = df,
      est_method = "dr",
      control_group = "notyettreated",
      panel = FALSE,
      clustervar = "prov"
    )
    
    ag <- aggte(att, type = "simple", na.rm = TRUE)
    
    # Calculate p-value from t-statistic
    t_stat <- ag$overall.att / ag$overall.se
    p_val <- 2 * pnorm(-abs(t_stat))
    
    tibble(
      outcome = label,
      estimate = ag$overall.att,
      se = ag$overall.se,
      p_value = p_val,
      ci_lower = ag$overall.att - 1.96 * ag$overall.se,
      ci_upper = ag$overall.att + 1.96 * ag$overall.se,
      n_observations = nrow(df),
      note = "OK"
    )
    
  })

  write_csv(outcomes_results, OUTCOMES_PATH)

  # Dynamic event-study effects
  
  dynamic_effects <- get_or_run_csv(DYN_PATH, function() {
    
    att <- att_gt(
      yname = "in_lfp",
      tname = "period_seq",
      idname = "unit_id",
      gname = "treatment_period_cohort_seq",
      data = analysis_data,
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
      p_value = 2 * pnorm(-abs(ad$att.egt / ad$se.egt)),
      conf.low = ad$att.egt - 1.96 * ad$se.egt,
      conf.high = ad$att.egt + 1.96 * ad$se.egt
    )
    
  }, force = force)
  
  # Dynamic effects excluding cohort 39 (March 2022)
  dynamic_effects_no_cohort39 <- get_or_run_csv(DYN_NO39_PATH, function() {
    
    att <- att_gt(
      yname = "in_lfp",
      tname = "period_seq",
      idname = "unit_id",
      gname = "treatment_period_cohort_seq",
      data = filter(analysis_data, treatment_period_cohort_seq != 39),
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
      p_value = 2 * pnorm(-abs(ad$att.egt / ad$se.egt)),
      conf.low = ad$att.egt - 1.96 * ad$se.egt,
      conf.high = ad$att.egt + 1.96 * ad$se.egt
    )
    
  }, force = force)

  subgroup_results <- map_dfr(names(subgroup_list), function(sv) {
    
    info <- subgroup_list[[sv]]
    
    # Run DiD for each subgroup category
    map_dfr(unique(na.omit(analysis_data[[sv]])), function(g) {
      
      if (is.na(g) || g %in% c("Unknown", "Other")) return(NULL)
      
      label <- ifelse(as.character(g) %in% names(info$groups), info$groups[[as.character(g)]], as.character(g))
      
      dsub <- filter(analysis_data, .data[[sv]] == g)
      
      if (nrow(dsub) < 1000) return(NULL)
      
      att <- att_gt(
        yname = "in_lfp",
        tname = "period_seq",
        idname = "unit_id",
        gname = "treatment_period_cohort_seq",
        data = dsub,
        est_method = "dr",
        control_group = "notyettreated",
        panel = FALSE,
        clustervar = "prov"
      )
      
      ag <- aggte(att, type = "simple", na.rm = TRUE)
      
      t_stat <- ag$overall.att / ag$overall.se
      p_val <- 2 * pnorm(-abs(t_stat))
      
      tibble(
        outcome = sprintf("%s: %s", info$name, label),
        estimate = ag$overall.att,
        se = ag$overall.se,
        p_value = p_val,
        ci_lower = ag$overall.att - 1.96 * ag$overall.se,
        ci_upper = ag$overall.att + 1.96 * ag$overall.se,
        n_observations = nrow(dsub),
        note = "OK"
      )
      
    })
    
  })

  write_csv(subgroup_results, SUBGROUP_PATH)

  # Extract number of cohorts contributing to each relative time period
  if ("n_g" %in% names(dynamic_effects)) {
    cohort_counts <- dynamic_effects %>%
      select(rel_time_num, n_g) %>%
      rename(rel_time = rel_time_num)
  } else {
    cohort_counts <- dynamic_effects %>%
      group_by(rel_time_num) %>%
      summarise(n_g = sum(!is.na(estimate)), .groups = "drop") %>%
      rename(rel_time = rel_time_num)
  }

  write_csv(cohort_counts, COHORT_COUNTS_PATH)

  invisible(list(
    outcomes = outcomes_results,
    dynamic = dynamic_effects,
    subgroups = subgroup_results,
    cohort = cohort_counts
  ))

}


# The following line runs the main analysis when this script is sourced. Set force = TRUE to re-run all analyses.
invisible(run_main_analysis(force = FALSE))
