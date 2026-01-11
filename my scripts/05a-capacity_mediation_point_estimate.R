# 05a-capacity_mediation.R
# Capacity mediation / continuous-treatment analysis
# Steps:
# 1) Read processed analysis dataset (microdata) and provincial capacity CSV
# 2) Aggregate to province-month level: compute rate (in_lfp), N, and merge capacity
# 3) Compute capacity_change (e.g., capacity_2023 / capacity_2019 - 1) or change since baseline
# 4) First-stage: capacity_change ~ treatment + prov FE + month FE + pre-trends
# 5) Second-stage: in_lfp ~ treatment + capacity_change + controls + prov FE + month FE
# 6) Compute indirect effect = coef_treat_in_first * coef_capacity_in_second; bootstrap for SE

library(dplyr)
library(fixest)
library(broom)

df <- readRDS("data/02-processed/analysis_dataset.rds")
cap <- read.csv("data/01-raw_data/raw/provincial_capacity_2019_2021_2023.csv", stringsAsFactors = FALSE)

# Expect `cap` to have columns: prov_code/prov, year, centre_spaces_0_to_5 (or similar)
# Normalize names
if('prov_code' %in% names(cap)) cap <- rename(cap, prov = prov_code)
if('province' %in% names(cap)) cap <- rename(cap, prov = province)

# Create capacity baseline and latest measures (2019 baseline and 2023 latest if present)
cap_baseline <- cap %>% filter(year == 2019) %>% select(prov, centre_spaces_0_to_5) %>% rename(cap_2019 = centre_spaces_0_to_5)
cap_latest <- cap %>% filter(year == 2023) %>% select(prov, centre_spaces_0_to_5) %>% rename(cap_2023 = centre_spaces_0_to_5)
cap_merge <- left_join(cap_baseline, cap_latest, by = 'prov')
cap_merge <- cap_merge %>% mutate(cap_change_ratio = (cap_2023 - cap_2019) / pmax(cap_2019, 1))

# Aggregate microdata to province-month and compute a time-varying post indicator
df <- df %>% mutate(date = as.Date(date), ym = format(date, '%Y-%m'))
agg <- df %>%
	group_by(prov, ym) %>%
	summarise(N = n(), successes = sum(in_lfp), rate = successes / N, treatment_date = first(treatment_date), .groups = 'drop') %>%
	mutate(treatment_date = as.Date(treatment_date),
				 ym_date = as.Date(paste0(ym, '-01')),
				 treated_post = as.integer(!is.na(treatment_date) & ym_date >= treatment_date))

# Merge capacity (prov-level) onto agg
agg2 <- left_join(agg, cap_merge, by = 'prov')

# First-stage: province-month capacity change is time-invariant in our cap_merge (only 2019 & 2023 available)
# So treat cap_change_ratio as province-level moderator. We test whether treatment interacts with cap_change_ratio.

# Model 1: use time-varying post indicator and interaction with capacity change.
# Note: `cap_change_ratio` is province-level and will be collinear with `prov` FE,
# so we omit its main effect and keep the interaction with the time-varying post indicator.
m1 <- feols(rate ~ treated_post + treated_post:cap_change_ratio | prov + ym, data = agg2)
summary(m1)

# Model 2: micro-level: use time-varying post indicator and interaction with capacity change
# Merge cap_change_ratio to microdata
micro <- left_join(df, cap_merge, by = 'prov') %>%
	mutate(treatment_date = as.Date(treatment_date), ym = format(as.Date(date), '%Y-%m'),
				 ym_date = as.Date(paste0(ym, '-01')), treated_post = as.integer(!is.na(treatment_date) & ym_date >= treatment_date))
# Coerce labelled/labelled_spss variables to plain types for fixest
micro <- micro %>%
	mutate(
		age_12 = suppressWarnings(as.character(haven::as_factor(age_12))),
		educ = suppressWarnings(as.character(haven::as_factor(educ))),
		in_lfp = dplyr::case_when(
			is.logical(in_lfp) ~ as.numeric(in_lfp),
			is.numeric(in_lfp) ~ in_lfp,
			TRUE ~ suppressWarnings(as.numeric(as.character(in_lfp)))
		)
	)
# Controls: age_12 and educ exist in processed dataset
m2 <- feols(in_lfp ~ treated_post + treated_post:cap_change_ratio + factor(age_12) + factor(educ) | prov + ym, cluster = ~prov, data = micro)
summary(m2)

# Estimate indirect effect approx (first-stage * effect of capacity in m2)
coef_m1 <- coef(m1)
coef_m2 <- coef(m2)
# Compute effect of treatment at the median capacity change using the interaction terms
med_effect <- coef_m2['treated_post']
int_effect <- coef_m2['treated_post:cap_change_ratio']
median_cap <- median(agg2$cap_change_ratio, na.rm = TRUE)
effect_at_median <- med_effect + int_effect * median_cap

# Save summaries
dir.create('output/results', showWarnings = FALSE)
tidy_m1 <- tidy(m1, conf.int = TRUE) %>%
  mutate(term = case_when(
    term == "treated_post" ~ "Direct effect",
    term == "treated_post:cap_change_ratio" ~ "Capacity mediation",
    TRUE ~ term
  ))
write.csv(tidy_m1, file = 'output/results/capacity_mediation_m1_summary.csv', row.names = FALSE)
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
write.csv(tidy_m2, file = 'output/results/capacity_mediation_m2_summary.csv', row.names = FALSE)

write.csv(data.frame(effect_at_median = effect_at_median, median_cap = median_cap), file = 'output/results/capacity_mediation_summary.csv', row.names = FALSE)
