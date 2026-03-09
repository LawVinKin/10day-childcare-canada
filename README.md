# Canada's $10/day Childcare Subsidy

This is the repository of the "Stacked Difference-in-Differences Evaluation of Canada's $10-a-Day Childcare Policy: Policy Impact on Maternal Employment", available on SSRN

For the practitioner's handbook, please see the paper folder, and open the PDF file. Alternatively, click the following link: https://github.com/LawVinKin/10day-childcare-canada/blob/961eb3a56f16b43b616badc85f82519a38094292/paper/practitioners_handbook.pdf 

## Scripts

- `01-clean_lfs_all_months.R`: read monthly LFS files (RData/CSV), harmonize variables, produce `data/02-processed/lfs_clean.rds`.
- `02-data_preparation.R`: merge treatment dates, create DiD-ready variables and cohort/period indices, save processed analysis RDS.
- `03-main_analysis.R`: run stacked DiD (`did::att_gt()` / `aggte()`), compute event‑study profiles, aggregate ATT, output CSVs and figures.
- `04-robustness_checks.R`: placebo tests, capacity mediation regression, other sensitivity checks.
- `05-figures_and_tables.R`: generate the paper figures and CSV tables consumed by `paper/paper.qmd`.
- `06-utilities.R`: shared helper functions (I/O helpers, `get_or_run_*`, variable/indicator helpers).
- `07-diagnostics_analysis.R`: additional diagnostics (cohort support, subgroup failures, data-quality checks).

## Required R packages

Install the main packages used by the analysis:

```r
install.packages(c("tidyverse", "fixest", "broom", "haven", "did"))
```




