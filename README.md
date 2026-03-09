# Canada's $10/day Childcare Subsidy

This is the repository of the "Stacked Difference-in-Differences Evaluation of Canada's $10-a-Day Childcare Policy: Policy Impact on Maternal Employment", available on SSRN

For the practitioner's handbook, please see the paper folder, and open the PDF file. Alternatively, click the following link: 

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
	paper_complete.qmd                        # manuscript (Quarto)
	paper_complete.pdf
	practitioners_handbook.qmd				  # practitioner's handbook (Quarto)
	practitioners_handbook.pdf				  # in pdf
	references.bib
	figures/
		fig_event_study.pdf			# event study figure
		mermaid-figure.png			# the overall structure of the childcare ecosystem

output/
	figures/                          # PNG/PDF figures used in paper
	results/                          # CSVs used by paper and tables
```

## Key outputs

- `output/results/stacked_did_dynamic_effects.csv` — event‑study estimates (relative time)
- `output/results/multiple_outcomes_results.csv` — ATT for alternate outcomes (employment, hours, earnings)
- `output/results/expanded_subgroup_results.csv` — subgroup ATT estimates
- `output/results/capacity_mediation_summary.csv` — capacity mediation interaction tests
- `output/figures/` — figures saved for inclusion in the manuscript

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



