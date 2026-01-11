# Submission changelog (concise)

Date: 2026-01-10

Summary of edits made to prepare the manuscript for submission without cohort-level Bayesian outputs.

- Archived Bayesian workflow
  - `my scripts/08-bayesian_models.R` renamed/archived as `my scripts/08-bayesian_models.R.disabled` (preserved copy).
  - Added `my scripts/08-bayesian_models.README.md` with instructions to re-enable and rationale for archiving.

- Lower-cost pivot analyses (frequentist)
  - New script: `my scripts/09-pivot_lower_cost_analyses.R` (attempts `glmer` then falls back to `fixest::feglm` or `glm` as needed).
  - Outputs written to `output/results/`:
    - `fit_cohort_fe.rds` (fixest fallback fit)
    - `fit_cohort_fe_tidy.csv` (tidy summary)
    - `mde_table.csv` (approximate MDE calculations)
    - `pivot_diag.json` (diagnostics)

- Manuscript edits
  - Edited `paper/paper.qmd` to (a) add a short methods note that hierarchical Bayesian cohort models were planned but deferred due to local computational constraints, and (b) replace an earlier sentence with a neutral deferral note pointing to `paper/appendix_bayesian_deferral.md`.
  - Re-rendered PDF: `paper/paper.pdf` (render successful).

Notes / rationale
- Running the full cohort-level Bayesian MCMC locally proved resource intensive and fragile on macOS (rstan/StanHeaders ABI issues). The team elected to defer the cohort Bayesian models and rely on frequentist robustness checks for the submission. The archived script preserves full reproducibility for future runs on an HPC or after local toolchain fixes.

How to re-run or re-enable
- To re-enable Bayesian runs locally: move `my scripts/08-bayesian_models.R.disabled` back to `my scripts/08-bayesian_models.R` and set `run_bayesian <- TRUE` inside that script. See `my scripts/08-bayesian_models.README.md` for details.
- Recommended: run the cohort `brms` model on remote compute (>=8 cores, >=32GB RAM) using `cmdstanr` backend. Ask me and I will produce a runnable job script (`run_bayes_on_hpc.sh`) with exact `Rscript` invocation.

Files changed/created (key)
- Edited: `paper/paper.qmd`
- Added: `my scripts/09-pivot_lower_cost_analyses.R`, `my scripts/08-bayesian_models.R.disabled`, `my scripts/08-bayesian_models.README.md`, `submission_changelog.md`
- New outputs in `output/results/` as listed above.

Committed by: automation helper (please review commit message before pushing to remote)
