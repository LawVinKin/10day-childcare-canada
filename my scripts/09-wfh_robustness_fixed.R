# Clean, runnable WFH robustness script
library(data.table)
library(dplyr)
library(fixest)
library(broom)
library(readr)

# Read data
df <- readRDS("data/02-processed/analysis_dataset.rds")

# Construct treatment_dummy if absent
if(!"treatment_dummy" %in% names(df)){
  if(all(c("is_treated_province", "post") %in% names(df))){
    df <- df %>% mutate(treatment_dummy = as.integer(is_treated_province & post))
  } else {
    stop("treatment_dummy missing and cannot be constructed")
  }
}

# Build WFH flag (heuristic)
if(!"occ_major" %in% names(df) & !"occ_code" %in% names(df)){
  df$wfh_flag <- NA_integer_
} else {
  df <- df %>% mutate(wfh_flag = ifelse(occ_major %in% c("Managers", "Professionals"), 1L, 0L))
}

if(!"ym" %in% names(df)) df <- df %>% mutate(ym = format(as.Date(date), "%Y-%m"))
df <- df %>% mutate(prov = as.factor(prov))

dir.create('output/results', showWarnings = FALSE, recursive = TRUE)

# Summary of WFH by province
wfh_by_prov <- df %>% group_by(prov) %>% summarise(n = n(), prop_wfh = mean(wfh_flag, na.rm = TRUE), n_missing = sum(is.na(wfh_flag)), .groups = 'drop')
write_csv(wfh_by_prov, 'output/results/wfh_by_prov_summary.csv')

# Determine whether triple-diff is feasible (some within-province variation)
has_within_variation <- any(wfh_by_prov$prop_wfh > 0 & wfh_by_prov$prop_wfh < 1, na.rm = TRUE)

if(has_within_variation){
  # Try triple-diff and capture warnings/errors
  td_fit <- tryCatch(
    feols(in_lfp ~ treatment_dummy * wfh_flag + age_12 + educ | prov + ym, cluster = ~prov, data = df),
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

} else {
  # Fallback: early vs late pretrend comparison
  prov_treat <- df %>% select(prov, treatment_date) %>% distinct()
  prov_treat <- prov_treat %>% mutate(treat_date = as.Date(treatment_date))
  median_date <- median(prov_treat$treat_date, na.rm = TRUE)
  early_provs <- prov_treat %>% filter(treat_date <= median_date) %>% pull(prov)
  df <- df %>% mutate(early_group = ifelse(prov %in% early_provs, 1L, 0L))

  if(!"rel_time" %in% names(df)){
    df <- df %>% mutate(rel_time = as.integer(round(difftime(as.Date(date), as.Date(treatment_date), units = 'weeks')/4.345)))
  }

  pre <- df %>% filter(rel_time < 0)
  pre_agg <- pre %>% group_by(prov, rel_time) %>% summarise(rate = mean(in_lfp, na.rm = TRUE), .groups = 'drop')
  pre_agg <- left_join(pre_agg, prov_treat, by = 'prov') %>% mutate(early = ifelse(prov %in% early_provs, 1L, 0L))
  m_pre <- tryCatch(feols(rate ~ rel_time * early | prov, data = pre_agg), error = function(e) e)
  if(inherits(m_pre, 'error')){
    write_lines(as.character(m_pre), 'output/results/pretrend_early_vs_late_error.txt')
  } else {
    saveRDS(m_pre, file = 'output/results/pretrend_early_vs_late.rds')
    write_csv(broom::tidy(m_pre, conf.int = TRUE), 'output/results/pretrend_early_vs_late_tidy.csv')
  }

}
