# In this script, we run frequentist robustness checks using mixed-effects models
# and fixed-effects alternatives to validate the main DiD results

#!/usr/bin/env Rscript
options(stringsAsFactors = FALSE)
print("Starting lower-cost pivot analyses: glmer robustness + MDEs")

library(tidyverse)
library(lme4)
library(broom.mixed)

setwd("..")

df <- readRDS("data/02-processed/analysis_dataset.rds")

# Ensure `cohort` variable exists for mixed-effects models
if (!"cohort" %in% names(df)) {
  df <- df %>%
    mutate(cohort = if_else(treatment_group == "Never_Treated", "Never_Treated", as.character(treatment_date))) %>%  # create cohort from treatment group/date
    mutate(cohort = as.factor(cohort))  # convert to factor for modeling
  print("Created cohort variable from treatment_group/treatment_date")
}

dir.create("output/results", showWarnings = FALSE, recursive = TRUE)

# 1) Frequentist hierarchical robustness: glmer mixed-effects model
fit_glmer <- NULL
print("Attempting to fit glmer mixed-effects logistic model (may take a minute)...")
glmer_err <- NULL
tryCatch({
  fit_glmer <- glmer(  # generalized linear mixed model
    in_lfp ~ post * is_treated_province + (1 | prov) + (1 + post | cohort),  # random intercepts for province and cohort, random slope for post in cohort
    data = df,
    family = binomial(link = "logit"),  # logistic regression for binary outcome
    control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 2e5))  # robust optimizer
  )
  saveRDS(fit_glmer, file = "output/results/fit_cohort_glmer.rds")
  print("Saved glmer fit to output/results/fit_cohort_glmer.rds")
  tidy_glmer <- broom.mixed::tidy(fit_glmer, effects = "fixed")  # extract fixed effects
  write_csv(tidy_glmer, "output/results/fit_cohort_glmer_tidy.csv")
  print("Saved tidy glmer summary to output/results/fit_cohort_glmer_tidy.csv")
}, error = function(e) {
  glmer_err <<- conditionMessage(e)
  print("glmer failed: ", glmer_err)
})

# If glmer fails, fall back to fixed-effects model
if (is.null(fit_glmer)) {
  print("Falling back to a fixed-effects logistic approximation.")
  if (requireNamespace("fixest", quietly = TRUE)) {
    print("Using fixest::feglm as fallback (clustered SEs)")
    library(fixest)
    # Use province and cohort as fixed effects and cluster by province
    fit_fe <- feglm(in_lfp ~ post * is_treated_province | prov + cohort, data = df, family = binomial(), cluster = ~prov)
    saveRDS(fit_fe, file = "output/results/fit_cohort_fe.rds")
    write_csv(as_tibble(summary(fit_fe)$coeftable, rownames = "term"), "output/results/fit_cohort_fe_tidy.csv")
    print("Saved fixest fallback fit to output/results/fit_cohort_fe.rds and tidy CSV")
  } else {
    print("fixest not available: falling back to GLM with province and cohort dummies (may be large).")
    # Build formula with factor dummies (may be heavy); use glm as last resort
    df$prov_f <- factor(df$prov)
    df$cohort_f <- factor(df$cohort)
    formula_glm <- as.formula("in_lfp ~ post * is_treated_province + prov_f + cohort_f")
    fit_glm_fe <- glm(formula_glm, data = df, family = binomial())
    saveRDS(fit_glm_fe, file = "output/results/fit_cohort_glm_fe.rds")
    tidy_glm_fe <- broom::tidy(fit_glm_fe)
    write_csv(tidy_glm_fe, "output/results/fit_cohort_glm_fe_tidy.csv")
    print("Saved glm fallback fit to output/results/fit_cohort_glm_fe.rds and tidy CSV")
  }
}

# 2) MDE / power table (approximate two-sample proportion MDEs)
print("Computing approximate MDEs for binary outcome (individual-level approximation)")

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
print("Saved approximate MDE table to output/results/mde_table.csv")

# 3) Save a short diagnostics summary
diag <- list(
  n_obs = nrow(df),
  n_cohorts = length(unique(df$cohort)),
  n_prov = length(unique(df$prov)),
  baseline_p = baseline_p,
  mde = mde_tbl$mde_absolute
)
writeLines(jsonlite::toJSON(diag, auto_unbox = TRUE, pretty = TRUE), "output/results/pivot_diag.json")
print("Wrote diagnostics to output/results/pivot_diag.json")

print("Lower-cost pivot analyses complete. Results in output/results/")
