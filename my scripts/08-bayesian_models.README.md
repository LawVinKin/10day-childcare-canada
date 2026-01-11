This archived script is being retained for reproducibility only and is
permanently disabled for this project.

Original: `my scripts/08-bayesian_models.R`
Archived copy: `my scripts/08-bayesian_models.R.disabled`

Policy decision: the project will not run cohort-level Bayesian models. The
archived script is preserved so other researchers can reproduce the original
specification if desired, but DO NOT re-enable or execute it as part of this
analysis or the submission package.

If you are an external user who wishes to re-run these models independently,
follow these general steps at your own risk and on appropriate hardware:

1. Review `my scripts/08-bayesian_models.R.disabled` to inspect the model
   specification and priors.
2. Move the file back and re-enable only in a separate fork/clone:
   ```bash
   mv "my scripts/08-bayesian_models.R.disabled" "my scripts/08-bayesian_models.R"
   ```
3. Use `cmdstanr` with CmdStan installed on a machine with sufficient
   resources (recommended: >=8 CPU cores and >=32 GB RAM). This project will
   not provide support for running those jobs.

This README documents the permanent archival decision and the location of the
archived copy for transparency and reproducibility.
