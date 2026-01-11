# In this script, we prepare the data for stacked DiD analysis by adding childcare capacity data
# and creating subgroup indicators for heterogeneity analysis

library(tidyverse)
library(haven)

setwd("..")

# Load the main analysis dataset and provincial capacity data
analysis_data_stacked <- read_rds("data/02-processed/analysis_dataset.rds")  # main dataset from previous script
capacity_data <- read_csv("data/01-raw_data/raw/provincial_capacity_2019_2021_2023.csv")  # capacity by province and year

# Merge capacity data onto the analysis data by province and year
analysis_with_capacity <- analysis_data_stacked %>%
  mutate(obs_year = as.integer(format(as.Date(date), '%Y'))) %>%  # extract year from date
  left_join(capacity_data, by = c("prov" = "prov_code", "obs_year" = "year"))  # join capacity

# Create baseline capacity from 2019 data for categorization
baseline_capacity <- capacity_data %>%
  filter(year == 2019) %>%  # 2019 as baseline
  select(prov_code, centre_spaces_0_to_5_baseline = centre_spaces_0_to_5)  # rename for clarity

# Add baseline and categorize provinces, plus compute capacity change ratio
final_data <- analysis_with_capacity %>%
  left_join(baseline_capacity, by = c("prov" = "prov_code")) %>%  # add baseline
  mutate(
    median_baseline = median(unique(centre_spaces_0_to_5_baseline), na.rm = TRUE),  # median across provinces
    baseline_capacity_cat = case_when(  # categorize as low/high based on median
      centre_spaces_0_to_5_baseline <= median_baseline ~ "Low Capacity (2019)",
      centre_spaces_0_to_5_baseline > median_baseline ~ "High Capacity (2019)",
      TRUE ~ NA_character_),
    capacity_ratio_to_baseline = ifelse(  # ratio of current to baseline capacity
      !is.na(centre_spaces_0_to_5_baseline) & centre_spaces_0_to_5_baseline != 0,
      centre_spaces_0_to_5 / centre_spaces_0_to_5_baseline,
      NA_real_)) %>%
  select(-median_baseline)  # remove temporary median column

# Create subgroup indicators for heterogeneity analysis
# lone_parent: 1 if single/lone parent, 0 otherwise
# educ_high: 1 if bachelor's or higher education, 0 otherwise  
# age_younger: 1 if age 25-34, 0 otherwise
final_data <- final_data %>%
  mutate(
    efamtype_lbl = suppressWarnings(as.character(as_factor(efamtype))),  # convert labelled to string
    educ_lbl = suppressWarnings(as.character(as_factor(educ))),
    age_12_lbl = suppressWarnings(as.character(as_factor(age_12))),
    lone_parent = if_else(  # indicator for lone parent
      grepl("lone|single|one parent", efamtype_lbl, ignore.case = TRUE),
      1L, 0L),
    educ_high = if_else(  # indicator for high education
      grepl("bachelor|university|degree|post[- ]?secondary|college|undergrad", educ_lbl, ignore.case = TRUE),
      1L, 0L),
    age_younger = if_else(  # indicator for younger age group
      grepl("25|25-34|25 to 34|25 to 34 years", age_12_lbl, ignore.case = TRUE),
      1L, 0L)
  ) %>%
  select(-efamtype_lbl, -educ_lbl, -age_12_lbl)  # remove temporary label columns

write_rds(final_data, "data/03-analysis_data/processed/analysis_data_stacked_with_capacity.rds")  # save prepared dataset
