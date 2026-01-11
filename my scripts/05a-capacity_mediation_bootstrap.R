# In this script, we will run a bootstrap analysis to asses whether
# changes in childcare capacity mediate the effect of treatment on maternal labour force participation.
# In simple terms, if provinces that had larger increases in childcare capacity, 
# would they have seen larger increases in maternal labour force participation?
library(dplyr)
library(fixest)

df <- readRDS("data/02-analysis_data/processed/analysis_dataset.rds")
cap <- read.csv("data/01-raw_data/raw/provincial_capacity_2019_2021_2023.csv") # If you are using an R version < 4.0, add stringsAsFactors = FALSE

if('prov_code' %in% names(cap)) cap <- rename(cap, prov = prov_code)
if('province' %in% names(cap)) cap <- rename(cap, prov = province)

# the following three code chunks prepare the capacity change data
```r
# Wrapper: consolidated bootstrap now lives in `05-capacity_mediation_bootstrap.R`
message("This script has been replaced by '05-capacity_mediation_bootstrap.R'. Sourcing that file now.")
source("my scripts/05-capacity_mediation_bootstrap.R")
```
cap_latest <- cap %>%
