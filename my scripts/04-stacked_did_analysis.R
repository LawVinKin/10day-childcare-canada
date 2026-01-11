# If you do not have the 'did' package install, install it via: install.packages("did")
library(did)
library(tidyverse)

# Use the processed stacked dataset (includes capacity and subgroup indicators)
analysis_data <- read_rds("data/03-analysis_data/processed/analysis_data_stacked_with_capacity.rds")

# Create period and treatment cohort vars from date/treatment_date if missing
analysis_data <- analysis_data %>%
  mutate(
    period = as.integer(format(as.Date(date), "%Y")) * 100 + as.integer(format(as.Date(date), "%m")),
    treatment_period_cohort = ifelse(is.na(treatment_date), NA_integer_, as.integer(format(as.Date(treatment_date), "%Y")) * 100 + as.integer(format(as.Date(treatment_date), "%m"))),
    unit_id = row_number()
  )

# in the code below, we convert the yyyymm format to a 
# numeric month number since January 2019
yyyymm_to_month_num <- function(yyyymm) {
  # Return NA for missing inputs, otherwise compute month index since Jan 2019
  ifelse(is.na(yyyymm), NA_integer_, {
    year <- as.integer(yyyymm %/% 100)
    month <- as.integer(yyyymm %% 100)
    (year - 2019) * 12 + month
  })
}

analysis_data <- analysis_data %>%
  mutate(
    period_seq = yyyymm_to_month_num(period),
    treatment_period_cohort_seq = yyyymm_to_month_num(treatment_period_cohort))

# the following loop runs the stacked DID analysis for various subgroups
# the difference in code with the stacked and regular analysis is that
# we use period_seq and treatment_period_cohort_seq instead of date variables
# this ensures that the time periods are aligned correctly in the stacked DID design
# the stacked did design requires these numeric period variables
run_stacked_did <- function(data, subgroup_name = "Full Sample") {
  att_results <- att_gt( # the att_gt function runs the stacked DID estimation
    yname = "in_lfp",
    tname = "period_seq",
    idname = "unit_id",
    gname = "treatment_period_cohort_seq",
    data = data,
    est_method = "dr", # "dr" is the doubly-robust estimator, which is helpful because of covariate adjustment
    control_group = "notyettreated", 
    panel = FALSE,
    clustervar = "prov"
  )
  
  att_agg_simple <- aggte(att_results, type = "simple") # this aggregates the ATT results into a single overall estimate
  
  estimate_val <- att_agg_simple$overall.att 
  se_val <- att_agg_simple$overall.se
  t_stat <- estimate_val / se_val
  p_val <- 2 * pnorm(-abs(t_stat))
  
  tibble( # this tibble summarizes the results
    subgroup = subgroup_name,
    estimate = estimate_val,
    se = se_val,
    p_value = p_val,
    ci_lower = estimate_val - 1.96 * se_val,
    ci_upper = estimate_val + 1.96 * se_val,
    n_observations = nrow(data)
  )
}

# the following function runs dynamic effects estimation, which
# estiamtes effects at each relative time period
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
  
  # the following aggregates the dynamic effects over cohorts
  att_agg_dynamic <- aggte(att_results, type = "dynamic")
  
  # the following tibble summarizes the dynamic effects results
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

# the following runs the stacked DiD analysis for the heterogeneous subgroups
# Build subgroup runs dynamically to avoid errors when subgroup vars are missing
stacked_results_list <- list()

stacked_results_list[[length(stacked_results_list) + 1]] <- run_stacked_did(analysis_data, "Full Sample")

if ("lone_parent" %in% names(analysis_data) && length(unique(na.omit(analysis_data$lone_parent))) > 1) {
  stacked_results_list[[length(stacked_results_list) + 1]] <- run_stacked_did(analysis_data %>% filter(lone_parent == 1), "Lone Parent")
  stacked_results_list[[length(stacked_results_list) + 1]] <- run_stacked_did(analysis_data %>% filter(lone_parent == 0), "Not Lone Parent")
} else {
  message("Skipping lone_parent subgroup: variable absent or no variation.")
}

if ("educ_high" %in% names(analysis_data) && length(unique(na.omit(analysis_data$educ_high))) > 1) {
  stacked_results_list[[length(stacked_results_list) + 1]] <- run_stacked_did(analysis_data %>% filter(educ_high == 1), "High Education")
  stacked_results_list[[length(stacked_results_list) + 1]] <- run_stacked_did(analysis_data %>% filter(educ_high == 0), "Low Education")
} else {
  message("Skipping educ_high subgroup: variable absent or no variation.")
}

if ("age_younger" %in% names(analysis_data) && length(unique(na.omit(analysis_data$age_younger))) > 1) {
  stacked_results_list[[length(stacked_results_list) + 1]] <- run_stacked_did(analysis_data %>% filter(age_younger == 1), "Younger (25-34)")
  stacked_results_list[[length(stacked_results_list) + 1]] <- run_stacked_did(analysis_data %>% filter(age_younger == 0), "Older (35-44)")
} else {
  message("Skipping age_younger subgroup: variable absent or no variation.")
}

all_stacked_results <- bind_rows(stacked_results_list)

# finally, we run the dynamic effects estimation
dynamic_effects <- run_stacked_did_dynamic(analysis_data)

dir.create("output/results", showWarnings = FALSE, recursive = TRUE)
write_csv(all_stacked_results, "output/results/stacked_did_results_summary.csv")
write_csv(dynamic_effects, "output/results/stacked_did_dynamic_effects.csv")
