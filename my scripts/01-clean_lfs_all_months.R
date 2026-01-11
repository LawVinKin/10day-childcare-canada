# In this script, we will clean and combine monthly Labour Force Survey (LFS) data files
# to create a single dataset focused on maternal labour force participation and occupation codes

library(tidyverse)
library(haven) # for reading .RData files

setwd("..") # set your own working directory here

# the following code lists all LFS .RData files in the raw data directory and processes them
lfs_files <- list.files("data/01-raw_data/raw", pattern = "^LFS_.*\\.RData$", full.names = TRUE)

# the following function identifies the correct variable name, since across the years, the wording
# of the variable has changed between "GENDER" and "SEX". 
get_sex_var_name <- function(table) {
  if ("GENDER" %in% names(table)) {
    "GENDER"
  } else if ("SEX" %in% names(table)) {
    "SEX"
  } else {
    NA
  }
}
 # the following function extracts occupation-related fields, handling different naming conventions
 # with the same basic logic that we used for the sex and gender variables
extract_occupation_fields <- function(table) {
  # as the aforementioned states, the following three lines extract occupation codes that
  # are needed for our work-from-home analysis. We are using these occupation codes as proxies
  # for possibilities of working from home.
  occ_code <- if ("NOC_10" %in% names(table)) as.character(as_factor(table$NOC_10))
  else if ("NOC_43" %in% names(table)) as.character(as_factor(table$NOC_43))
  else NA_character_
# the following two lines extract occupation major groups (similar to 2-digit codes)
# that we will use for certain subgroup analyses. 
  occ_major <- if ("OCC_MAJ" %in% names(table)) as.character(as_factor(table$OCC_MAJ))
  else if ("OCC_MAJ_2016" %in% names(table)) as.character(as_factor(table$OCC_MAJ_2016))
  else NA_character_
  list(occ_code = occ_code, occ_major = occ_major) # finally, we list the extracted fields
}

# the following function cleans the LFS data for a single month based on our inclusion criteria
# and the relevant variables for our analysis. 
clean_lfs_data <- function(table, sex_var_name) {
  table %>%
    mutate(
      sex_n = as.numeric(.data[[sex_var_name]]),
      agyownk_n = as.numeric(AGYOWNK), # indicator for having own children in household
      age_12_n = as.numeric(AGE_12), # age of youngest child in household
      lfsstat_n = as.numeric(LFSSTAT), # labour force status
      prov_n = as.numeric(PROV), # province code
      date = as.Date(paste(SURVYEAR, SURVMNTH, "01", sep = "-"))) %>% # we create a date variable
    filter(
      sex_n == 2, # female
      !is.na(agyownk_n), # exclude missing agyownk
      agyownk_n == 1, # has own children
      age_12_n >= 3, age_12_n <= 6) # youngest child aged 3-6 (inclusive)
}

# the following function selects and finalizes the relevant variables for analysis
# including creating the in_lfp variable based on lfsstat
select_and_finalize <- function(df, occ_fields) {
  df %>%
    mutate(
      occ_code = occ_fields$occ_code,
      occ_major = occ_fields$occ_major
    ) %>%
    select(
      prov = prov_n,
      lfsstat = lfsstat_n,
      weight = FINALWT,
      date,
      age_12 = age_12_n,
      marstat = MARSTAT,
      educ = EDUC,
      efamtype = EFAMTYPE,
      immig = IMMIG,
      schooln = SCHOOLN,
      occ_code,
      occ_major) %>%
    mutate(
      in_lfp = if_else(lfsstat %in% c(1, 2, 3), 1, 0)) # in_lfp: 1 if employed, unemployed, or on layoff; 0 otherwise
}

# this function cleans a single LFS .RData file and returns the cleaned dataframe
# and we will use it in a loop to process all files and combine them together
clean_one_month <- function(file_path) {
  load(file_path)
  loaded_object_name <- ls()[length(ls())]
  table <- get(loaded_object_name)
  
  sex_var_name <- get_sex_var_name(table)
  df_clean <- clean_lfs_data(table, sex_var_name)
  occ_fields <- extract_occupation_fields(df_clean)
  
  df_clean <- select_and_finalize(df_clean, occ_fields)
  
  df_clean
}

lfs_all <- map_dfr(lfs_files, clean_one_month, .id = NULL)

write_rds(lfs_all, "data/02-processed/lfs_clean.rds")