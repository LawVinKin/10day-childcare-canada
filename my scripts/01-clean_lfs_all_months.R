library(tidyverse)
library(haven)

lfs_files <- list.files("data/01-raw_data/raw", pattern = "^LFS_.*\\.RData$", full.names = TRUE)

get_sex_var_name <- function(table) {
  if ("GENDER" %in% names(table)) {
    "GENDER"
  } else if ("SEX" %in% names(table)) {
    "SEX"
  } else {
    NA
  }
}

extract_occupation_fields <- function(table) {
  occ_code <- if ("NOC_10" %in% names(table)) as.character(as_factor(table$NOC_10)) else if ("NOC_43" %in% names(table)) as.character(as_factor(table$NOC_43)) else NA_character_
  occ_major <- if ("OCC_MAJ" %in% names(table)) as.character(as_factor(table$OCC_MAJ)) else if ("OCC_MAJ_2016" %in% names(table)) as.character(as_factor(table$OCC_MAJ_2016)) else NA_character_
  list(occ_code = occ_code, occ_major = occ_major)
}

clean_lfs_data <- function(table, sex_var_name) {
  table %>%
    mutate(
      sex_n = as.numeric(.data[[sex_var_name]]),
      agyownk_n = as.numeric(AGYOWNK),
      age_12_n = as.numeric(AGE_12),
      lfsstat_n = as.numeric(LFSSTAT),
      prov_n = as.numeric(PROV),
      date = as.Date(paste(SURVYEAR, SURVMNTH, "01", sep = "-"))) %>%
    filter(
      sex_n == 2,
      !is.na(agyownk_n),
      agyownk_n == 1,
      age_12_n >= 3, age_12_n <= 6)
}

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
      in_lfp = if_else(lfsstat %in% c(1, 2, 3), 1, 0))
}

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