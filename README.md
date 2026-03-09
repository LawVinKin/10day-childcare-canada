# CanadaChildcareMaternalLabour

This is the repository of the "Stacked Difference-in-Differences Evaluation of Canada's $10-a-Day Childcare Policy: Policy Impact on Maternal Employment".

For the practitioner's handbook, please see the paper folder, and open the PDF file.

## Repository layout (key files & folders)

```
project_template.Rproj
README.md

data/
	01-raw_data/
		demographics.csv
		econ_indicators.csv
		labour_char_metadata.csv
		labour_char.csv
		LFS_PUMF_EPA_FGMD_codebook.csv
		raw/
			LFS_April_2019.RData
			LFS_April_2020.RData
			... (monthly LFS files, 2019–2025)

	02-processed/
		analysis_dataset.rds
		lfs_clean.rds

	03-analysis_data/
		processed/
			analysis_data_stacked_with_capacity.rds

my scripts/
	01-clean_lfs_all_months.R        # clean monthly LFS -> data/02-processed/lfs_clean.rds
	02-data_preparation.R            # merge treatment dates, create variables -> data/03-analysis_data/...
	03-main_analysis.R               # stacked DiD: outcomes, dynamic/event-study, subgroups
	04-robustness_checks.R           # placebo tests, capacity mediation, WFH tests
	05-figures_and_tables.R          # generate figures and CSV tables for paper
	06-utilities.R                   # shared helpers (read/write, caching, variable helpers)
	07-diagnostics_analysis.R        # diagnostics and data-quality checks

paper/
	paper.qmd                        # manuscript (Quarto)
	references.bib
	figures/  fonts/ styles/

output/
	figures/                          # PNG/PDF figures used in paper
	results/                          # CSVs used by paper and tables
```

## Quick start — reproduce everything (from project root)

Run the steps below in R. Each script will produce missing intermediate files when required; use `force = TRUE` to overwrite saved outputs.

1. Clean raw LFS files
	 - `source("my scripts/01-clean_lfs_all_months.R"); run_clean_lfs(force = TRUE)`

2. Prepare analysis dataset (merge treatments, add variables)
	 - `source("my scripts/02-data_preparation.R"); run_prepare_data(force = TRUE)`

3. Run main stacked DiD analysis (outcomes, dynamic/event-study, subgroups)
	 - `source("my scripts/03-main_analysis.R"); run_main_analysis(force = TRUE)`

4. Robustness checks & diagnostics
	 - `source("my scripts/04-robustness_checks.R"); run_robustness_checks(force = TRUE)`
	 - `source("my scripts/07-diagnostics_analysis.R"); run_diagnostics(force = TRUE)`

5. Generate figures and tables for the manuscript
	 - `source("my scripts/05-figures_and_tables.R"); run_figures_and_tables(force = TRUE)`

Notes:
- The pipeline is idempotent: missing files are created automatically; `force = TRUE` forces recomputation.
- All commands assume you run them from the project root.

## Key outputs (used by the paper)

- `output/results/stacked_did_dynamic_effects.csv` — event‑study estimates (relative time)
- `output/results/multiple_outcomes_results.csv` — ATT for alternate outcomes (employment, hours, earnings)
- `output/results/expanded_subgroup_results.csv` — subgroup ATT estimates
- `output/results/capacity_mediation_summary.csv` — capacity mediation interaction tests
- `output/figures/` — figures saved for inclusion in the manuscript

## Scripts — short descriptions

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

(If you prefer the development version of `did`, install from GitHub.)

## Reproducibility & caching

- Helper functions `get_or_run_rds()` and `get_or_run_csv()` (in `my scripts/06-utilities.R`) make the scripts idempotent. That is, they read saved files when present and only recompute when requested via `force = TRUE`.
- This design ensures the repository can be run from a clean checkout; nothing in the workflow requires pre-existing cached files.

## Where diagnostics feed into the paper

- Diagnostics and robustness CSVs (placebo, capacity mediation, cohort diagnostics) inform statements in the Results, Discussion and Appendix of `paper/paper.qmd`. The manuscript reads `capacity_mediation_summary.csv` and the main dynamic/placebo CSVs; other diagnostics are used for interpretation and are saved under `output/results/`.
