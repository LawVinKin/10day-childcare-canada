# In this script, we run work-from-home (WFH) robustness checks
# to test if WFH-amenable occupations confound the childcare policy effects

library(dplyr)
library(fixest)
library(broom)
library(readr)

setwd("..")

# Read the analysis dataset
df <- readRDS("data/02-processed/analysis_dataset.rds")

# Ensure treatment_dummy variable exists (indicator for treated province post-treatment)
if(!"treatment_dummy" %in% names(df)){
  if(all(c("is_treated_province", "post") %in% names(df))){
    df <- df %>% mutate(treatment_dummy = as.integer(is_treated_province & post))  # 1 if treated province and post-period
  } else {
    stop("treatment_dummy missing and cannot be constructed")
  }
}

# Build WFH flag based on occupation major groups (heuristic for WFH amenability)
if(!"occ_major" %in% names(df) & !"occ_code" %in% names(df)){
  df$wfh_flag <- NA_integer_  # set to NA if no occupation data
} else {
  df <- df %>% mutate(wfh_flag = ifelse(occ_major %in% c("Managers", "Professionals"), 1L, 0L))  # 1 for managers/professionals
}

# Create year-month variable for fixed effects
if(!"ym" %in% names(df)) df <- df %>% mutate(ym = format(as.Date(date), "%Y-%m"))
df <- df %>% mutate(prov = as.factor(prov))  # ensure province is factor

dir.create('output/results', showWarnings = FALSE, recursive = TRUE)

# Create summary of WFH proportions by province
wfh_by_prov <- df %>% group_by(prov) %>% summarise(n = n(), prop_wfh = mean(wfh_flag, na.rm = TRUE), n_missing = sum(is.na(wfh_flag)), .groups = 'drop')
write_csv(wfh_by_prov, 'output/results/wfh_by_prov_summary.csv')

# Check if there's within-province variation in WFH (needed for triple-diff)
has_within_variation <- any(wfh_by_prov$prop_wfh > 0 & wfh_by_prov$prop_wfh < 1, na.rm = TRUE)

# If variation exists, run triple-difference model: treatment * WFH interaction
if(has_within_variation){
  td_fit <- tryCatch(
    feols(in_lfp ~ treatment_dummy * wfh_flag + age_12 + educ | prov + ym, cluster = ~prov, data = df),  # triple-diff with controls
    error = function(e) e,
    warning = function(w) w
  )

  if(inherits(td_fit, 'error')){
    write_lines(as.character(td_fit), 'output/results/triple_diff_wfh_error.txt')
  } else {
    saveRDS(td_fit, file = 'output/results/triple_diff_wfh_feols.rds')
    write_csv(broom::tidy(td_fit, conf.int = TRUE), 'output/results/triple_diff_wfh_feols_tidy.csv')
  }

  # Event study per WFH group (separate samples)
  for(g in c(0L,1L)){
    sub <- df %>% filter(wfh_flag == g)
    if(!"rel_time" %in% names(sub)) next
    if(nrow(sub) < 50) next
    est <- tryCatch(feols(in_lfp ~ i(rel_time, ref = -1) | prov + ym, cluster = ~prov, data = sub), error = function(e) e)
    if(inherits(est, 'error')){
      write_lines(as.character(est), sprintf('output/results/event_study_wfh_%d_error.txt', g))
    } else {
      saveRDS(est, file = sprintf('output/results/event_study_wfh_%d.rds', g))
      write_csv(broom::tidy(est, conf.int = TRUE), sprintf('output/results/event_study_wfh_%d_tidy.csv', g))
    }
  }

}
