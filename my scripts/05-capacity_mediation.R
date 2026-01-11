# In this script, we perform capacity mediation analysis to test if childcare capacity changes
# mediate the effect of the policy on maternal labor force participation.
# We estimate point effects and bootstrap for inference.

library(dplyr)
library(fixest)
library(broom)
library(readr)

setwd("..")

# Load data
df <- readRDS("data/02-processed/analysis_dataset.rds")
cap <- read_csv("data/01-raw_data/raw/provincial_capacity_2019_2021_2023.csv", show_col_types = FALSE)

# Normalize column names
if('prov_code' %in% names(cap)) cap <- rename(cap, prov = prov_code)
if('province' %in% names(cap)) cap <- rename(cap, prov = province)

# Prepare capacity data
cap_baseline <- cap %>% filter(year == 2019) %>% select(prov, centre_spaces_0_to_5) %>% rename(cap_2019 = centre_spaces_0_to_5)
cap_latest <- cap %>% filter(year == 2023) %>% select(prov, centre_spaces_0_to_5) %>% rename(cap_2023 = centre_spaces_0_to_5)
cap_merge <- left_join(cap_baseline, cap_latest, by = 'prov') %>% mutate(cap_change_ratio = (cap_2023 - cap_2019) / pmax(cap_2019, 1))

# Aggregate to province-month level
df_agg <- df %>% mutate(date = as.Date(date), ym = format(date, '%Y-%m'))
agg <- df_agg %>%
  group_by(prov, ym) %>%
  summarise(N = n(), successes = sum(in_lfp), rate = successes / N, treatment_date = first(treatment_date), .groups = 'drop') %>%
  mutate(treatment_date = as.Date(treatment_date),
         ym_date = as.Date(paste0(ym, '-01')),
         treated_post = as.integer(!is.na(treatment_date) & ym_date >= treatment_date))

# Merge capacity onto aggregated data
agg2 <- left_join(agg, cap_merge, by = 'prov')

# Model 1: Aggregate level (first-stage like)
m1 <- feols(rate ~ treated_post + treated_post:cap_change_ratio | prov + ym, data = agg2)

# Prepare micro data for Model 2
micro <- left_join(df_agg, cap_merge, by = 'prov') %>%
  mutate(treatment_date = as.Date(treatment_date), ym = format(as.Date(date), '%Y-%m'),
         ym_date = as.Date(paste0(ym, '-01')), treated_post = as.integer(!is.na(treatment_date) & ym_date >= treatment_date))

# Coerce labelled variables
micro <- micro %>%
  mutate(
    age_12 = suppressWarnings(as.character(haven::as_factor(age_12))),
    educ = suppressWarnings(as.character(haven::as_factor(educ))),
    in_lfp = case_when(is.logical(in_lfp) ~ as.numeric(in_lfp), is.numeric(in_lfp) ~ in_lfp, TRUE ~ suppressWarnings(as.numeric(as.character(in_lfp))))
  )

# Model 2: Micro level with interaction
m2 <- feols(in_lfp ~ treated_post + treated_post:cap_change_ratio + factor(age_12) + factor(educ) | prov + ym, cluster = ~prov, data = micro)

# Compute effect at median capacity
median_cap <- median(cap_merge$cap_change_ratio, na.rm = TRUE)
coef_m2 <- coef(m2)
med_effect <- coef_m2['treated_post']
int_effect <- coef_m2['treated_post:cap_change_ratio']
effect_at_median <- med_effect + int_effect * median_cap

# Bootstrap function
compute_effect <- function(data){
  m2_boot <- feols(in_lfp ~ treated_post + treated_post:cap_change_ratio + factor(age_12) + factor(educ) | prov + ym, cluster = ~prov, data = data)
  coefs <- coef(m2_boot)
  med_effect <- coefs['treated_post']
  int_effect <- coefs['treated_post:cap_change_ratio']
  return(med_effect + int_effect * median_cap)
}

# Run bootstrap
set.seed(2026)
B <- 500
provs <- unique(micro$prov)
boot_est <- numeric(B)
for(b in seq_len(B)){
  sampled_provs <- sample(provs, length(provs), replace = TRUE)
  boot_data <- do.call(rbind, lapply(sampled_provs, function(p) micro[micro$prov == p, , drop = FALSE]))
  boot_est[b] <- tryCatch(compute_effect(boot_data), error = function(e) NA_real_)
}

# Summaries
est_mean <- mean(boot_est, na.rm = TRUE)
est_se <- sd(boot_est, na.rm = TRUE)
ci_low <- quantile(boot_est, 0.025, na.rm = TRUE)
ci_high <- quantile(boot_est, 0.975, na.rm = TRUE)

# Save results
dir.create('output/results', showWarnings = FALSE, recursive = TRUE)

# Point estimates
tidy_m1 <- tidy(m1, conf.int = TRUE) %>%
  mutate(term = case_when(
    term == "treated_post" ~ "Direct effect",
    term == "treated_post:cap_change_ratio" ~ "Capacity mediation",
    TRUE ~ term
  ))
write_csv(tidy_m1, 'output/results/capacity_mediation_m1_summary.csv')

tidy_m2 <- tidy(m2, conf.int = TRUE) %>%
  mutate(term = case_when(
    term == "treated_post" ~ "Direct effect",
    term == "treated_post:cap_change_ratio" ~ "Capacity",
    term == "factor(age_12)3" ~ "25-29",
    term == "factor(age_12)4" ~ "30-34",
    term == "factor(age_12)5" ~ "35-39",
    term == "factor(age_12)6" ~ "40-44",
    str_detect(term, "^factor\\(educ\\)") ~ str_replace(term, "factor\\(educ\\)", "Education: "),
    TRUE ~ term
  ))
write_csv(tidy_m2, 'output/results/capacity_mediation_m2_summary.csv')

write_csv(data.frame(effect_at_median = effect_at_median, median_cap = median_cap), 'output/results/capacity_mediation_summary.csv')

# Bootstrap results
write_csv(tibble(boot_est = boot_est), 'output/results/capacity_mediation_bootstrap_estimates.csv')
write_csv(tibble(est_mean = est_mean, est_se = est_se, ci_low = ci_low, ci_high = ci_high), 'output/results/capacity_mediation_bootstrap_summary.csv')

# Capacity mediation analysis completed: point estimates and bootstrap results saved to output/results/