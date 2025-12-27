library(tidyverse)
library(haven)

lfs_data <- read_rds("data/processed/lfs_clean.rds")
treatment_df <- read.csv("data/raw/treatment_dates.csv")

province_code_map <- tribble(
  ~province,                      ~prov_code,
  "Newfoundland and Labrador",    10,
  "Prince Edward Island",         11,
  "Nova Scotia",                  12,
  "New Brunswick",                13,
  "Quebec",                       24,
  "Ontario",                      35,
  "Manitoba",                     46,
  "Saskatchewan",                 47,
  "Alberta",                      48,
  "British Columbia",             59,
  "Yukon",                        61,
  "Northwest Territories",        62,
  "Nunavut",                      63
)

treatment_with_codes <- treatment_df %>%
  left_join(province_code_map, by = "province")

if (nrow(treatment_df) != nrow(treatment_with_codes)) {
  warning("Some provinces in treatment_dates.csv were not found in the code map!")
}

analysis_data <- lfs_data %>%
  left_join(
    treatment_with_codes %>% select(prov_code, treatment_date),
    by = c("prov" = "prov_code")
  ) %>%
  mutate(
    is_treated_province = !is.na(treatment_date),
    post = if_else(is_treated_province, date >= treatment_date, FALSE),
    treatment_group = if_else(is_treated_province, as.character(treatment_date), "Never_Treated")
  )

final_analysis_data <- analysis_data %>%
  select(
    prov, date, lfsstat, in_lfp, weight,
    age_12, marstat, educ,
    efamtype, immig, schooln,
    is_treated_province, post, treatment_date, treatment_group
  )

write_rds(final_analysis_data, "data/processed/analysis_dataset.rds")