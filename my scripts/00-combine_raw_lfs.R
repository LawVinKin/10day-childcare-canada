# This script combines all the labour force data files into one
# master file for cleaning and analysis. It handles both .RData and .csv formats,
# converts labelled variables to numeric, and ensures all columns are character to avoid type conflicts during binding.

library(tidyverse)
library(haven)

# Get all LFS file paths
files <- list.files(
  "data/01-raw_data/raw",
  pattern = "^LFS_.*\\.(RData|csv)$",
  full.names = TRUE
)

# Helper function: load one LFS file
load_lfs_file <- function(file_path) {
  if (grepl("\\.RData$", file_path)) {
    env <- new.env()
    load(file_path, envir = env)
    df <- get(ls(env)[1], envir = env)
  } else {
    df <- read_csv(file_path, show_col_types = FALSE)
  }
  
  # Convert labelled to numeric
  df <- df %>%
    mutate(across(where(haven::is.labelled), as.numeric))  
  # convert all columns to character to avoid type conflicts across months
  df <- df %>%
    mutate(across(everything(), as.character))
  return(df)
}

# Load all files into a list
all_data <- lapply(files, load_lfs_file)

# Combine all files
combined <- bind_rows(all_data)

# after binding, coerce important numeric columns back to numeric
combined <- combined %>%
  mutate(across(c(SURVYEAR, SURVMNTH, AGE_12, AGYOWNK, LFSSTAT, PROV), as.numeric))

dir.create("data/01-raw_data", showWarnings = FALSE, recursive = TRUE)
save(combined, file = "data/01-raw_data/lfs_all_months_combined.RData")