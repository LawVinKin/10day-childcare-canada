library(tidyverse)

analysis_data_stacked <- read_rds("data/processed/analysis_data_stacked.rds")
capacity_data <- read_csv("data/raw/provincial_capacity_2019_2021_2023.csv")

analysis_data_with_capacity <- analysis_data_stacked %>%
  mutate(obs_year = as.numeric(str_sub(period, 1, 4))) %>%
  left_join(capacity_data, by = c("prov" = "prov_code", "obs_year" = "year"))

baseline_capacity_2019 <- capacity_data %>%
  filter(year == 2019) %>%
  select(prov_code, centre_spaces_0_to_5_baseline = centre_spaces_0_to_5)

analysis_data_with_capacity <- analysis_data_with_capacity %>%
  left_join(baseline_capacity_2019, by = c("prov" = "prov_code"))

median_baseline <- median(analysis_data_with_capacity$centre_spaces_0_to_5_baseline, na.rm = TRUE)
analysis_data_with_capacity <- analysis_data_with_capacity %>%
  mutate(
    baseline_capacity_cat = case_when(
      centre_spaces_0_to_5_baseline <= median_baseline ~ "Low Capacity (2019)",
      centre_spaces_0_to_5_baseline > median_baseline ~ "High Capacity (2019)",
      TRUE ~ NA_character_
    ),
    capacity_ratio_to_baseline = ifelse(
      !is.na(centre_spaces_0_to_5_baseline) & centre_spaces_0_to_5_baseline != 0,
      centre_spaces_0_to_5 / centre_spaces_0_to_5_baseline,
      NA_real_
    )
  )

write_rds(analysis_data_with_capacity, "data/processed/analysis_data_stacked_with_capacity.rds")
