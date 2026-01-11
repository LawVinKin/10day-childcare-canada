# In this script, we run the main stacked difference-in-differences analysis
# using the 'did' package to estimate treatment effects on maternal labor force participation

# Install 'did' package if needed: install.packages("did")
library(did)
library(tidyverse)

setwd("..")

# Load the prepared dataset with capacity and subgroups
analysis_data <- read_rds("data/03-analysis_data/processed/analysis_data_stacked_with_capacity.rds")

# Create numeric period and cohort variables required by the did package
analysis_data <- analysis_data %>%
  mutate(
    period = as.integer(format(as.Date(date), "%Y")) * 100 + as.integer(format(as.Date(date), "%m")),  # yyyymm format
    treatment_period_cohort = ifelse(is.na(treatment_date), NA_integer_, as.integer(format(as.Date(treatment_date), "%Y")) * 100 + as.integer(format(as.Date(treatment_date), "%m"))),  # treatment cohort in yyyymm
    unit_id = row_number()  # unique ID for each observation
  )

# Function to convert yyyymm to month number since January 2019 (for did package)
yyyymm_to_month_num <- function(yyyymm) {
  # Return NA for missing inputs, otherwise compute month index since Jan 2019
  ifelse(is.na(yyyymm), NA_integer_, {
    year <- as.integer(yyyymm %/% 100)
    month <- as.integer(yyyymm %% 100)
    (year - 2019) * 12 + month
  })
}

# Add sequential period and cohort variables
analysis_data <- analysis_data %>%
  mutate(
    period_seq = yyyymm_to_month_num(period),  # sequential month number
    treatment_period_cohort_seq = yyyymm_to_month_num(treatment_period_cohort))  # sequential treatment month

# Function to run stacked DiD for a subgroup
run_stacked_did <- function(data, subgroup_name = "Full Sample") {
  att_results <- att_gt(  # estimate group-time ATT using did package
    yname = "in_lfp",  # outcome: labor force participation
    tname = "period_seq",  # time variable
    idname = "unit_id",  # unit ID
    gname = "treatment_period_cohort_seq",  # treatment cohort
    data = data,
    est_method = "dr",  # doubly-robust estimator
    control_group = "notyettreated",  # control group type
    panel = FALSE,  # repeated cross-section
    clustervar = "prov"  # cluster SEs by province
  )
  
  att_agg_simple <- aggte(att_results, type = "simple")  # aggregate to overall ATT
  
  estimate_val <- att_agg_simple$overall.att  # extract estimate
  se_val <- att_agg_simple$overall.se  # extract SE
  t_stat <- estimate_val / se_val  # t-statistic
  p_val <- 2 * pnorm(-abs(t_stat))  # p-value
  
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
  print("Skipping lone_parent subgroup: variable absent or no variation.")
}

if ("educ_high" %in% names(analysis_data) && length(unique(na.omit(analysis_data$educ_high))) > 1) {
  stacked_results_list[[length(stacked_results_list) + 1]] <- run_stacked_did(analysis_data %>% filter(educ_high == 1), "High Education")
  stacked_results_list[[length(stacked_results_list) + 1]] <- run_stacked_did(analysis_data %>% filter(educ_high == 0), "Low Education")
} else {
  print("Skipping educ_high subgroup: variable absent or no variation.")
}

if ("age_younger" %in% names(analysis_data) && length(unique(na.omit(analysis_data$age_younger))) > 1) {
  stacked_results_list[[length(stacked_results_list) + 1]] <- run_stacked_did(analysis_data %>% filter(age_younger == 1), "Younger (25-34)")
  stacked_results_list[[length(stacked_results_list) + 1]] <- run_stacked_did(analysis_data %>% filter(age_younger == 0), "Older (35-44)")
} else {
  print("Skipping age_younger subgroup: variable absent or no variation.")
}

all_stacked_results <- bind_rows(stacked_results_list)

# finally, we run the dynamic effects estimation
dynamic_effects <- run_stacked_did_dynamic(analysis_data)

dir.create("output/results", showWarnings = FALSE, recursive = TRUE)
write_csv(all_stacked_results, "output/results/stacked_did_results_summary.csv")
write_csv(dynamic_effects, "output/results/stacked_did_dynamic_effects.csv")
