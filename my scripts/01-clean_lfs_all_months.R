# This script cleans the combined raw labour force survey data, creating analysis-ready variables and applying sample restrictions.
# It handles differences in variable names (namely for the sex/gender variable) across survey years and ensures all variables are in a consistent format for analysis.

library(tidyverse)
library(haven)

# Load combined raw data
load("data/01-raw_data/lfs_all_months_combined.RData")

## We need a single numeric sex code.
## Older waves use SEX (1/2), later waves use GENDER (1/2).  We'll prefer
## whichever column has the fewest missing values, and fall back to the other.
get_sex_var <- function(data) {
  has_sex <- "SEX" %in% names(data)
  has_gender <- "GENDER" %in% names(data)
  if (has_sex && has_gender) {
    n_sex <- sum(!is.na(data$SEX))
    n_gen <- sum(!is.na(data$GENDER))
    if (n_gen > n_sex) return("GENDER") else return("SEX")
  }
  if (has_gender) return("GENDER")
  if (has_sex) return("SEX")
  return(NA_character_)
}

sex_var <- get_sex_var(combined)

# Convert labelled variables to numeric
combined <- combined %>%
  mutate(across(where(haven::is.labelled), as.numeric))

# Create analysis variables
combined <- combined %>%
  mutate(
    sex_n = as.numeric(.data[[sex_var]]),
    age_youngest_child = as.numeric(AGYOWNK),
    age_12_n = as.numeric(AGE_12),
    lfsstat_n = as.numeric(LFSSTAT),
    prov_n = as.numeric(PROV),
    date = as.Date(paste(SURVYEAR, SURVMNTH, "01", sep = "-"))
  )

# Apply sample restrictions (females, youngest child under 6, age 25-44)
combined <- combined %>%
  filter(
    sex_n == 2,
    !is.na(age_youngest_child),
    age_youngest_child == 1,
    age_12_n >= 3 & age_12_n <= 6
  )

# Select and rename final variables
cleaned <- combined %>%
  transmute( # the transmute function both selects and renames variables in one step
    prov = prov_n,
    date = date,
    lfsstat = lfsstat_n,
    weight = FINALWT,
    age_12 = age_12_n,
    marstat = MARSTAT,
    educ = EDUC,
    efamtype = EFAMTYPE,
    immig = IMMIG,
    schooln = SCHOOLN,
    ftptmain = FTPTMAIN,
    uhrsmain = UHRSMAIN,
    utothrs = UTOTHRS,
    hrlyearn = HRLYEARN,
    cowmain = COWMAIN
  ) %>%
  mutate( # the if_else function is a vectorized version of if-else statements,
          # allowing us to create new binary variables based on conditions
    in_lfp = if_else(lfsstat %in% c(1, 2, 3), 1L, 0L), 
    in_employed = if_else(lfsstat %in% c(1, 2), 1L, 0L),
    in_ft_employed = if_else(ftptmain == 1, 1L, 0L),
    in_pt_employed = if_else(ftptmain == 2, 1L, 0L),
    hours_usual = as.numeric(uhrsmain) / 10,
    hours_total = as.numeric(utothrs) / 10,
    earnings_hourly = as.numeric(hrlyearn) / 100
  )

# Save cleaned data
dir.create("data/02-processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(cleaned, "data/02-processed/lfs_clean.rds")
