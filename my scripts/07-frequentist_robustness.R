# In this script, we run frequentist robustness checks using mixed-effects models
# and fixed-effects alternatives to validate the main DiD results.
# In more simple terms, we will fit generalized linear mixed models (GLMMs)
# with random effects for provinces and treatment cohorts to see if results hold up.
# Additionally, we will compute minimum detectable effects (MDEs) for the analysis.
# In simple terms, the MDEs are the smallest effect sizes we could reliably detect
# given our sample size and data structure.

options(stringsAsFactors = FALSE) # if you are using an older R version, set stringsAsFactors to FALSE to avoid issues with strings being factors

library(tidyverse)
library(lme4) # needed for glmer, which is used for mixed-effects models
library(broom.mixed) # for tidying mixed model outputs

setwd("..")

df <- readRDS("data/02-processed/analysis_dataset.rds")

# The following function simply ensure that the cohort variable exists.
# If it does not exist, we create it from treatment_group / treatment_date.
if (!"cohort" %in% names(df)) {
  df <- df %>%
    mutate(cohort = if_else(treatment_group == "Never_Treated", "Never_Treated", as.character(treatment_date))) %>%  # create cohort from treatment group/date
    mutate(cohort = as.factor(cohort))  # convert to factor for modeling
  # Created cohort variable from treatment_group/treatment_date
}

dir.create("output/results", showWarnings = FALSE, recursive = TRUE)

# 1) Frequentist hierarchical robustness: fixed-effects model with fixest
library(fixest)
# Use province and cohort as fixed effects and cluster by province
fit_fe <- feglm(in_lfp ~ post * is_treated_province | prov + cohort, data = df, family = binomial(), cluster = ~prov)
saveRDS(fit_fe, file = "output/results/fit_cohort_fe.rds")
write_csv(as_tibble(summary(fit_fe)$coeftable, rownames = "term"), "output/results/fit_cohort_fe_tidy.csv")
# Saved fixest fit to output/results/fit_cohort_fe.rds and tidy CSV

# 2) MDE / power table (approximate two-sample proportion MDEs)
# Computing approximate MDEs for binary outcome (individual-level approximation)

# Baseline proportion: average in pre-period among control provinces
pre_control <- df %>% filter(post == 0 & is_treated_province == 0)
if (nrow(pre_control) < 20) {
  baseline_p <- mean(df$in_lfp == 1, na.rm = TRUE)
  warning("Few control pre-period observations; using overall baseline proportion")
} else {
  baseline_p <- mean(pre_control$in_lfp == 1, na.rm = TRUE)
}

# Define group sizes for post period (treated vs control)
post_df <- df %>% filter(post == 1)
n_treated_post <- sum(post_df$is_treated_province == 1, na.rm = TRUE)
n_control_post <- sum(post_df$is_treated_province == 0, na.rm = TRUE)
n_min <- min(n_treated_post, n_control_post)

if (n_min < 10) {
  warning("Very small post-period group sizes; MDE results will be noisy")
}

# Analytic approximation to two-sample proportion MDE (two-sided alpha=0.05, power=0.8)
# alpha is the significance level, power is the desired power level, z_alpha and z_power are the corresponding z-scores
alpha <- 0.05
power_target <- 0.8
z_alpha <- qnorm(1 - alpha / 2)
z_power <- qnorm(power_target)
if (n_treated_post > 0 && n_control_post > 0) {
  se_diff <- sqrt(baseline_p * (1 - baseline_p) / n_treated_post + baseline_p * (1 - baseline_p) / n_control_post)
  delta <- (z_alpha + z_power) * se_diff
  # report absolute MDE and the implied treated proportion (increase)
  mde_abs <- delta
  p2 <- baseline_p + delta
  if (p2 > 1) p2 <- NA_real_
} else {
  mde_abs <- NA_real_
  p2 <- NA_real_
}

mde_tbl <- tibble(
  baseline_p = baseline_p,
  n_treated_post = n_treated_post,
  n_control_post = n_control_post,
  n_per_group_used = n_min,
  cohen_h = NA_real_,
  detectable_p2 = p2,
  mde_absolute = mde_abs
)

write_csv(mde_tbl, "output/results/mde_table.csv")
# Saved approximate MDE table to output/results/mde_table.csv

# 3) Save a short diagnostics summary
diag <- list(
  n_obs = nrow(df),
  n_cohorts = length(unique(df$cohort)),
  n_prov = length(unique(df$prov)),
  baseline_p = baseline_p,
  mde = mde_tbl$mde_absolute
)
writeLines(jsonlite::toJSON(diag, auto_unbox = TRUE, pretty = TRUE), "output/results/pivot_diag.json")