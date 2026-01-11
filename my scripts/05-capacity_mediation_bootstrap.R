# Consolidated bootstrap for capacity mediation
# Reads canonical analysis dataset and writes bootstrap estimates and summary CSVs
library(dplyr)
library(fixest)
library(readr)
library(stringr)

data_path <- "data/02-processed/analysis_dataset.rds"
if(!file.exists(data_path)) stop(sprintf("Required file not found: %s", data_path))
df <- readRDS(data_path)

cap_path <- "data/01-raw_data/raw/provincial_capacity_2019_2021_2023.csv"
if(!file.exists(cap_path)) stop(sprintf("Capacity CSV not found: %s", cap_path))
cap <- read_csv(cap_path, show_col_types = FALSE)

if('prov_code' %in% names(cap)) cap <- rename(cap, prov = prov_code)
if('province' %in% names(cap)) cap <- rename(cap, prov = province)

cap_baseline <- cap %>% filter(year == 2019) %>% select(prov, centre_spaces_0_to_5) %>% rename(cap_2019 = centre_spaces_0_to_5)
cap_latest <- cap %>% filter(year == 2023) %>% select(prov, centre_spaces_0_to_5) %>% rename(cap_2023 = centre_spaces_0_to_5)
cap_merge <- left_join(cap_baseline, cap_latest, by = 'prov') %>% mutate(cap_change_ratio = (cap_2023 - cap_2019) / pmax(cap_2019, 1))

micro <- left_join(df, cap_merge, by = 'prov')
micro <- micro %>% mutate(treatment_date = as.Date(treatment_date), ym = format(as.Date(date), '%Y-%m'),
                           ym_date = as.Date(paste0(ym, '-01')), treated_post = as.integer(!is.na(treatment_date) & ym_date >= treatment_date))
micro <- micro %>% mutate(age_12 = suppressWarnings(as.character(haven::as_factor(age_12))),
                           educ = suppressWarnings(as.character(haven::as_factor(educ))),
                           in_lfp = case_when(is.logical(in_lfp) ~ as.numeric(in_lfp), is.numeric(in_lfp) ~ in_lfp, TRUE ~ suppressWarnings(as.numeric(as.character(in_lfp)))))

median_cap <- median(cap_merge$cap_change_ratio, na.rm = TRUE)

compute_effect <- function(data){
  m2 <- feols(in_lfp ~ treated_post + treated_post:cap_change_ratio + factor(age_12) + factor(educ) | prov + ym, cluster = ~prov, data = data)
  coefs <- coef(m2)
  med_effect <- coefs['treated_post']
  int_effect <- coefs['treated_post:cap_change_ratio']
  return(med_effect + int_effect * median_cap)
}

set.seed(2026)
B <- 500
provs <- unique(micro$prov)
boot_est <- numeric(B)
for(b in seq_len(B)){
  sampled_provs <- sample(provs, length(provs), replace = TRUE)
  boot_data <- do.call(rbind, lapply(sampled_provs, function(p) micro[micro$prov == p, , drop = FALSE]))
  boot_est[b] <- tryCatch(compute_effect(boot_data), error = function(e) NA_real_)
}

dir.create('output/results', showWarnings = FALSE, recursive = TRUE)
write_csv(tibble(boot_est = boot_est), file = 'output/results/capacity_mediation_bootstrap_estimates.csv')

est_mean <- mean(boot_est, na.rm = TRUE)
est_se <- sd(boot_est, na.rm = TRUE)
ci_low <- quantile(boot_est, 0.025, na.rm = TRUE)
ci_high <- quantile(boot_est, 0.975, na.rm = TRUE)
write_csv(tibble(est_mean = est_mean, est_se = est_se, ci_low = ci_low, ci_high = ci_high), file = 'output/results/capacity_mediation_bootstrap_summary.csv')

# Also generate the model summary for the table with cleaned term names
m2 <- feols(in_lfp ~ treated_post + treated_post:cap_change_ratio + factor(age_12) + factor(educ) | prov + ym, cluster = ~prov, data = micro)
tidy_m2 <- broom::tidy(m2, conf.int = TRUE) %>%
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

message("Bootstrap completed: wrote estimates and summary to output/results/")
