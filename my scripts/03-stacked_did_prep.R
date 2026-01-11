library(tidyverse)
library(haven)

# We first input data
analysis_data_stacked <- read_rds("data/02-processed/analysis_dataset.rds")
capacity_data <- read_csv("data/01-raw_data/raw/provincial_capacity_2019_2021_2023.csv")

# We add capacity data to the stacked DID analysis data
analysis_with_capacity <- analysis_data_stacked %>%
  mutate(obs_year = as.integer(format(as.Date(date), '%Y'))) %>%
  left_join(capacity_data, by = c("prov" = "prov_code", "obs_year" = "year"))

# We set 2019 as a base year for capacity categorization
baseline_capacity <- capacity_data %>%
  filter(year == 2019) %>%
  select(prov_code, centre_spaces_0_to_5_baseline = centre_spaces_0_to_5)

# We add the baseline and categorize provinces based on median split (low capacity, high capacity)
final_data <- analysis_with_capacity %>%
  left_join(baseline_capacity, by = c("prov" = "prov_code")) %>%
  mutate(
    median_baseline = median(unique(centre_spaces_0_to_5_baseline), na.rm = TRUE),
    baseline_capacity_cat = case_when(
      centre_spaces_0_to_5_baseline <= median_baseline ~ "Low Capacity (2019)",
      centre_spaces_0_to_5_baseline > median_baseline ~ "High Capacity (2019)",
      TRUE ~ NA_character_),
    capacity_ratio_to_baseline = ifelse(
      !is.na(centre_spaces_0_to_5_baseline) & centre_spaces_0_to_5_baseline != 0,
      centre_spaces_0_to_5 / centre_spaces_0_to_5_baseline,
      NA_real_)) %>%
  select(-median_baseline)  # removes the temporary column that is no longer needed

# Derive subgroup indicators expected by stacked DID analysis scripts
# - `lone_parent`: TRUE if family type label indicates a lone / single parent
# - `educ_high`: TRUE if education label indicates bachelor's degree or higher
# - `age_younger`: TRUE if respondent age group label corresponds to 25-34
final_data <- final_data %>%
  mutate(
    efamtype_lbl = suppressWarnings(as.character(as_factor(efamtype))),
    educ_lbl = suppressWarnings(as.character(as_factor(educ))),
    age_12_lbl = suppressWarnings(as.character(as_factor(age_12))),
    lone_parent = if_else(
      grepl("lone|single|one parent", efamtype_lbl, ignore.case = TRUE),
      1L, 0L),
    educ_high = if_else(
      grepl("bachelor|university|degree|post[- ]?secondary|college|undergrad", educ_lbl, ignore.case = TRUE),
      1L, 0L),
    age_younger = if_else(
      grepl("25|25-34|25 to 34|25 to 34 years", age_12_lbl, ignore.case = TRUE),
      1L, 0L)
  ) %>%
  select(-efamtype_lbl, -educ_lbl, -age_12_lbl)

write_rds(final_data, "data/03-analysis_data/processed/analysis_data_stacked_with_capacity.rds")
