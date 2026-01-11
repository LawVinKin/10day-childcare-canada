library(tidyverse)

setwd("..")

load_data <- function() {
  lfs_data <- read_rds("data/02-processed/lfs_clean.rds")
  treatment_df <- read.csv("data/01-raw_data/raw/treatment_dates.csv")
  list(lfs = lfs_data, treatment = treatment_df)
}

get_province_code_map <- function() {
  tribble(
    ~province, ~prov_code,
    "Newfoundland and Labrador", 10,
    "Prince Edward Island", 11,
    "Nova Scotia", 12,
    "New Brunswick", 13,
    "Quebec", 24,
    "Ontario", 35,
    "Manitoba", 46,
    "Saskatchewan", 47,
    "Alberta", 48,
    "British Columbia", 59,
    "Yukon", 61,
    "Northwest Territories", 62,
    "Nunavut", 63)
}

process_treatments <- function(treatment_df, province_code_map) {
  treatment_with_codes <- treatment_df %>%
    left_join(province_code_map, by = "province")
  
  treatment_selected <- treatment_with_codes %>%
    select(prov_code, treatment_date)
  
  treatment_selected
}

join_and_create_variables <- function(lfs_data, treatment_selected) {
  analysis_data <- lfs_data %>%
    left_join(treatment_selected, by = c("prov" = "prov_code")) %>%
    mutate(
      is_treated_province = !is.na(treatment_date),
      post = if_else(is_treated_province, date >= treatment_date, FALSE),
      treatment_group = if_else(is_treated_province, as.character(treatment_date), "Never_Treated"))
  
  analysis_data
}

select_final_columns <- function(analysis_data) {
  final_analysis_data <- analysis_data %>%
    select(
      prov, date, lfsstat, in_lfp, weight,
      age_12, marstat, educ,
      efamtype, immig, schooln,
      is_treated_province, post, treatment_date, treatment_group,
      occ_code, occ_major)
  
  final_analysis_data
}

data <- load_data()
province_code_map <- get_province_code_map()
treatment_selected <- process_treatments(data$treatment, province_code_map)
analysis_data <- join_and_create_variables(data$lfs, treatment_selected)
final_analysis_data <- select_final_columns(analysis_data)

write_rds(final_analysis_data, "data/02-processed/analysis_dataset.rds")