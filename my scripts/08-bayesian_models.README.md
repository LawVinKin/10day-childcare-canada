This script has been archived and disabled.

- Original: `my scripts/08-bayesian_models.R`
- Archived copy: `my scripts/08-bayesian_models.R.disabled`

Reason: Cohort-level Bayesian hierarchical models require substantial CPU/RAM
and a working Stan toolchain. To prevent accidental long-running jobs on the
developer laptop, the active script was disabled and an archived copy was
preserved.

How to re-enable:
1. Review the archived file `my scripts/08-bayesian_models.R.disabled`.
2. Move it back to `my scripts/08-bayesian_models.R`:
   ```bash
   mv "my scripts/08-bayesian_models.R.disabled" "my scripts/08-bayesian_models.R"
   ```
3. Inside the script set `run_bayesian <- TRUE` and adjust `brms_options` to
   desired `chains`, `iter`, and `cores`.

Notes:
- Consider running large Bayesian jobs on a remote server or HPC with >8 cores
  and >=32GB RAM.
