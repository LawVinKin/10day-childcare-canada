```markdown
Planned Bayesian Hierarchical Analysis — Permanent archival note

The project considered cohort-level Bayesian hierarchical models (varying
slopes by treatment cohort) during early stages of analysis. After exploration
and pilot attempts, the team has decided not to run cohort-level Bayesian
models as part of this project or its submission package. This decision is
final for this repository: the archived model code is retained for
transparency only and will not be executed by the project team.

Why archived and not run

- The archived code documents explicit model specifications and priors, but
  re-running the full hierarchical MCMC requires substantial compute and a
  separate implementation/maintenance burden that the project will not
  undertake.
- The project pivoted to a set of frequentist robustness checks (mixed
  models, cluster bootstrap, subgroup MDEs) that provide clear and
  reproducible robustness evidence for the submitted manuscript.

Where the code is

- The archived script is located at `my scripts/08-bayesian_models.R.disabled`.
  It is preserved only for documentation and reproducibility for external
  researchers; DO NOT re-enable this script in the active project workflow.

Contact and reproducibility

If an external researcher wishes to reproduce or extend the Bayesian
specification, they may inspect the archived script and run it independently
on suitable hardware. The project team will not run these models or provide
Bayesian outputs for the submission.

``` 
Planned Bayesian Hierarchical Analysis — Note

Planned Bayesian hierarchical models (cohort-level varying slopes and pooled hierarchical models) were part of the pre-registered analysis plan for this paper. These models were intended to borrow strength across provinces and stabilize month-by-month posterior estimates.

Reason for deferral

During implementation on the author's local macOS machine the Stan/rstan compilation toolchain produced linker and compilation errors (C++ ABI / StanHeaders incompatibilities). Multiple attempts to run the models were made (including installing and using the `cmdstanr` backend), but the full MCMC for the cohort hierarchical model could not be completed in a robust, reproducible way in the local environment prior to submission.

What we report instead

- The manuscript reports the main stacked DiD results, event-study analyses, heterogeneous estimates, capacity mediation analysis, and a set of frequentist robustness checks (mixed-effects models and cluster bootstrap) that we ran successfully and that underpin the paper's substantive conclusions.

Next steps / reproducibility

- Full Bayesian model code (brms + cmdstanr calls) and the resulting RDS outputs will be posted to the project's replication repository when a successful run is completed (either locally after toolchain fixes or on a remote Linux server). The scripts used to attempt the runs are already included in `my scripts/08-bayesian_models.R` and an audit log is available in `output/logs/` (if present).

Contact

If reviewers or editors would like the full Bayesian outputs for additional scrutiny, please contact the author and we will prioritize re-running the models on a stable compute environment and posting results to the online appendix.