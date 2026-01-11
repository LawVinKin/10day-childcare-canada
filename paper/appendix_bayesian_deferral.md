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