library(tidyverse)

lfs_files <- list.files("data/01-raw_data/raw", pattern = "^LFS_.*\\.RData$", full.names = TRUE)

clean_one_month <- function(file_path) {
  load(file_path)
  loaded_object_name <- ls()[length(ls())]
  table <- get(loaded_object_name)
  
  if ("GENDER" %in% names(table)) {
    sex_var_name <- "GENDER"
  } else if ("SEX" %in% names(table)) {
    sex_var_name <- "SEX"
  } else {
    stop("Neither 'GENDER' nor 'SEX' variable found in ", basename(file_path))
  }
  
  df_clean <- table %>%
    mutate(
      sex_n = as.numeric(pull(., all_of(sex_var_name))),
      agyownk_n = as.numeric(AGYOWNK),
      age_12_n = as.numeric(AGE_12),
      lfsstat_n = as.numeric(LFSSTAT),
      prov_n = as.numeric(PROV),
      date = as.Date(paste(SURVYEAR, SURVMNTH, "01", sep = "-"))
    ) %>%
    filter(
      sex_n == 2,
      !is.na(agyownk_n),
      agyownk_n == 1,
      age_12_n >= 3, age_12_n <= 6
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
      schooln = SCHOOLN
    ) %>%
    mutate(
      in_lfp = if_else(lfsstat %in% c(1, 2, 3), 1, 0)
    )
  
  return(df_clean)
}

lfs_all <- map_dfr(lfs_files, clean_one_month, .id = NULL)

write_rds(lfs_all, "data/processed/lfs_clean.rds")