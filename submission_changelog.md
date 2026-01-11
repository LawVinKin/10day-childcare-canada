# Submission changelog (concise)

Date: 2026-01-10

Summary of edits made to prepare the manuscript for submission.

- Lower-cost pivot analyses (frequentist)
  - New script: `my scripts/09-pivot_lower_cost_analyses.R` (attempts `glmer` then falls back to `fixest::feglm` or `glm` as needed).
  - Outputs written to `output/results/`:
    - `fit_cohort_fe.rds` (fixest fallback fit)
    - `fit_cohort_fe_tidy.csv` (tidy summary)
    - `mde_table.csv` (approximate MDE calculations)
    - `pivot_diag.json` (diagnostics)

- Manuscript edits
  - Edited `paper/paper.qmd` to remove references to deferred analyses.
  - Re-rendered PDF: `paper/paper.pdf` (render successful).

Notes / rationale
- The project relies on frequentist robustness checks for the submitted manuscript.

How to re-run or re-enable
- Scripts are designed to be run in order.

Files changed/created (key)
- Edited: `paper/paper.qmd`
- Added: `my scripts/09-pivot_lower_cost_analyses.R`, `submission_changelog.md`
- New outputs in `output/results/` as listed above.

Committed by: automation helper (please review commit message before pushing to remote)

