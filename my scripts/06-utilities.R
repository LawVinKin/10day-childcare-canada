# Utility functions for Canada Childcare analysis
# This script contains helper functions used across all analysis scripts

library(tidyverse)


### Cache helpers

# Read .rds if exists, otherwise run function and save
get_or_run_rds <- function(path, fn, force = FALSE) {
  if (file.exists(path) && !isTRUE(force)) {
    return(readr::read_rds(path))
  }
  res <- fn()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_rds(res, path)
  res
}

# Read CSV if exists, otherwise run function and save
get_or_run_csv <- function(path, fn, force = FALSE, read_args = list(), write_args = list()) {
  if (file.exists(path) && !isTRUE(force)) {
    return(do.call(readr::read_csv, c(list(file = path, show_col_types = FALSE), read_args)))
  }
  res <- fn()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  do.call(readr::write_csv, c(list(x = res, file = path), write_args))
  res
}


### Data cleaning helpers

# Convert yyyymm to month number since January 2019
yyyymm_to_month_num <- function(yyyymm) {
  ifelse(is.na(yyyymm), NA_integer_, {
    year <- as.integer(yyyymm %/% 100)
    month <- as.integer(yyyymm %% 100)
    (year - 2019) * 12 + month
  })
}

# Get sex/gender variable name (differs across survey years)
get_sex_var_name <- function(table) {
  if ("GENDER" %in% names(table)) "GENDER" else if ("SEX" %in% names(table)) "SEX" else NA
}


### Data loading helpers

# Province name to code mapping
get_province_code_map <- function() {
  tribble(
    ~province, ~prov_code,
    "Newfoundland and Labrador", 10, "Prince Edward Island", 11, "Nova Scotia", 12,
    "New Brunswick", 13, "Quebec", 24, "Ontario", 35, "Manitoba", 46,
    "Saskatchewan", 47, "Alberta", 48, "British Columbia", 59,
    "Yukon", 61, "Northwest Territories", 62, "Nunavut", 63
  )
}

# Load cleaned LFS data and treatment dates
load_data <- function() {
  lfs_data <- read_rds("data/02-processed/lfs_clean.rds")
  treatment_path <- "data/01-raw_data/raw/treatment_dates.csv"
  if (!file.exists(treatment_path)) {
    stop("Treatment dates file not found at ", treatment_path,
         ". Please create or place the CSV with province and treatment_date columns.")
  }
  treatment_df <- read.csv(treatment_path)
  list(lfs = lfs_data, treatment = treatment_df)
}

# Merge treatment dates with province codes
process_treatments <- function(treatment_df, province_code_map) {
  treatment_df %>%
    left_join(province_code_map, by = "province") %>%
    select(prov_code, treatment_date)
}

# Merge LFS with treatment indicators
join_and_create_variables <- function(lfs_data, treatment_selected) {
  lfs_data %>%
    left_join(treatment_selected, by = c("prov" = "prov_code")) %>%
    mutate(
      is_treated_province = !is.na(treatment_date),
      post = if_else(is_treated_province, date >= treatment_date, FALSE),
      treatment_group = if_else(is_treated_province, as.character(treatment_date), "Never_Treated")
    )
}

# Select final columns for analysis
select_final_columns <- function(analysis_data) {
  analysis_data %>%
    select(
      prov, date, lfsstat,
      in_lfp, in_employed, in_ft_employed, in_pt_employed,
      hours_usual, hours_total, earnings_hourly,
      weight, age_12, marstat, educ, efamtype, immig, schooln,
      is_treated_province, post, treatment_date, treatment_group,
      cowmain
    )
}

# Prepare data for DiD analysis (create period/cohort variables)
prepare_did_data <- function(data) {
  data %>%
    mutate(
      period = as.integer(format(as.Date(date), "%Y")) * 100 + as.integer(format(as.Date(date), "%m")),
      treatment_period_cohort = ifelse(
        is.na(treatment_date),
        NA_integer_,
        as.integer(format(as.Date(treatment_date), "%Y")) * 100 +
          as.integer(format(as.Date(treatment_date), "%m"))
      ),
      unit_id = row_number()
    ) %>%
    mutate(
      period_seq = yyyymm_to_month_num(period),
      treatment_period_cohort_seq = yyyymm_to_month_num(treatment_period_cohort)
    )
}

# Create subgroup indicators for heterogeneous effects
create_subgroup_indicators <- function(data) {
  data %>%
    mutate(
      # Family structure: lone parent (EFAMTYPE codes 14-17)
      lone_parent = if_else(efamtype %in% c(14, 15, 16, 17), 1L, 0L),

      # Education: high (bachelor's or higher, codes 5-6)
      educ_high = if_else(educ %in% c(5, 6), 1L, 0L),

      # Age: younger (25-34, codes 3-4)
      age_younger = if_else(age_12 %in% c(3, 4), 1L, 0L),

      # Student status
      student_status = case_when(
        schooln == 2 ~ "Full-time student",
        schooln == 3 ~ "Part-time student",
        schooln == 1 ~ "Non-student",
        TRUE ~ "Unknown"
      ),

      # Class of worker
      class_of_worker = case_when(
        cowmain == 1 ~ "Public sector",
        cowmain == 2 ~ "Private sector",
        cowmain %in% c(3, 4, 5, 6) ~ "Self-employed",
        TRUE ~ "Unknown"
      ),

      # Immigrant status (immig: 1=Non-immigrant, 2=Immigrant)
      # Note: Further breakdown by years since immigration requires additional variable
      immigrant_status = case_when(
        immig == 1 ~ "Non-Immigrant",
        immig == 2 ~ "Immigrant",
        TRUE ~ "Unknown"
      ),

      # Marital status (marstat: 1=Married, 2=Common-law, 3=Separated, 4=Divorced, 5=Widowed, 6=Single)
      marital_status = case_when(
        marstat %in% c(1, 2) ~ "Married/Common-law",
        marstat %in% c(3, 4, 5) ~ "Separated/Divorced/Widowed",
        marstat == 6 ~ "Single (never married)",
        TRUE ~ "Unknown"
      )
    )
}
