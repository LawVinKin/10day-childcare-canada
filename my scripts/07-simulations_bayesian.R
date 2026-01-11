# This script runs simulations to assess power of the DiD design under various data-generating processes.
# In simple terms, we simulate aggregated province-month level data with random intercepts and time effects,
# then estimate a simple DiD ATT estimator on the simulated data, and repeat this many times to assess power.
# this would help us understand how well our design can detect effects of various sizes under different assumptions.

library(data.table) # this is for efficient data manipulation; data.table is faster than dplyr for large datasets
library(dplyr)
library(lubridate) #also for date manipulations; lubridate makes it easy to work with dates
library(ggplot2) # can't do it without the GOAT! 

df <- readRDS("data/02-processed/analysis_dataset.rds")

## The variables we expect are the following:
# prov: province identifier
# date: date of observation (we will extract year/month from this)
# in_lfp: binary indicator of labor force participation (0/1)
# is_treated_province: binary indicator of whether the province is treated (1) or control (0)
# treatment_dummy: binary indicator of whether the observation is post-treatment in a treated province (1) or not (0)
if(!"N_cell" %in% names(df)){
  # create ym from `date` if year/month not present
  df <- df %>% mutate(date = as.Date(date), year = as.integer(format(date, "%Y")), month = as.integer(format(date, "%m")), ym = as.Date(paste0(year, "-", month, "-01")))
  treated_var <- ifelse("treatment_dummy" %in% names(df), "treatment_dummy", ifelse("is_treated_province" %in% names(df), "is_treated_province", NA))
  agg <- df %>%
    group_by(prov, year, month, ym) %>%
    summarise(N_cell = n(), successes = sum(in_lfp), treated = max(.data[[treated_var]], na.rm = TRUE), .groups = "drop")
} else {
  agg <- df %>% select(prov, year, month, N_cell, successes, treated)
}

agg <- as.data.table(agg) # this just converts to data.table for efficiency

# Here, we develop a simple simulation DGP and estimator on aggregated data.
# We assume a logistic model with province random intercepts and month fixed effects.
# The treatment effect is a fixed delta on the logit scale.
# We simulate data under various values of delta (true effect size) and sigma_p (province random intercept SD).
# We then estimate a simple DiD ATT estimator on the aggregated data and repeat many times to assess power.
agg[, p_hat := successes / N_cell]
baseline_rate <- mean(agg$p_hat, na.rm = TRUE)

# Further, we can simulate temporal correlation in month effects via an AR(1) process if desired.
# The AR(1) correlation parameter is rho_time, which is 0 by default (independent month effects).
# in simple time,  AR(1) is the process where each month's effect depends on the previous month's effect plus some noise.

simulate_agg <- function(agg_template, delta = 0.01, sigma_p = 0.02, rho_time = 0, scaleN = 1){
  # agg_template: data.table with prov, year, month, N_cell, treated
  sim_dt <- copy(agg_template)
  # province random intercepts
  provs <- unique(sim_dt$prov)
  alpha_p <- rnorm(length(provs), 0, sigma_p)
  names(alpha_p) <- provs
  # month fixed effect (use observed p_hat variability)
  months <- unique(sim_dt$ym)
  # simulate month effects as AR(1) if rho_time > 0
  month_effects <- rnorm(length(months), 0, 0.005)
  if(rho_time > 0){
    for(i in 2:length(months)) month_effects[i] <- rho_time * month_effects[i-1] + rnorm(1, 0, 0.003)
  }
  names(month_effects) <- months

  sim_dt[, alpha := alpha_p[as.character(prov)]]
  sim_dt[, beta := month_effects[as.character(ym)]]
  sim_dt[, N_sim := round(N_cell * scaleN)]
  # linear predictor on logit scale
  # baseline logit that recovers baseline_rate roughly
  intercept_logit <- qlogis(baseline_rate)
  sim_dt[, logit_p := intercept_logit + alpha + beta + delta * treated]
  sim_dt[, p := plogis(logit_p)]
  sim_dt[, successes := rbinom(.N, size = N_sim, prob = p)]
  sim_dt[, observed_rate := successes / N_sim]
  return(sim_dt)
}

# The following loop runs simulations over a grid of delta and sigma_p values,
# and computes a simple DiD ATT estimator on the simulated aggregated data.
# The estimator is the difference in pre-post changes between treated and control provinces.
estimate_att_agg <- function(sim_dt){
  # naive aggregated estimator: difference in post - pre for treated minus control
  # Here we produce a stacked-DiD style estimate by computing province-level pre/post averages
  dt <- copy(sim_dt)
  dt[, post := treated == 1]
  # province-level pre and post
  prov_stats <- dt[, .(pre_y = sum(successes[post==0]) / sum(N_sim[post==0]), post_y = sum(successes[post==1]) / sum(N_sim[post==1])), by = prov]
  # treated provinces
  treated_provs <- prov_stats[prov %in% dt[treated==1, unique(prov)]]
  control_provs <- prov_stats[!prov %in% dt[treated==1, unique(prov)]]
  att <- mean(treated_provs$post_y - treated_provs$pre_y, na.rm = TRUE) - mean(control_provs$post_y - control_provs$pre_y, na.rm = TRUE)
  return(att)
}

# Quick demo run (small grid): delta in {0, 0.01, 0.02}, sigma_p in {0.01,0.03}
params_grid <- expand.grid(delta = c(0, 0.01, 0.02), sigma_p = c(0.01, 0.03), rho = c(0, 0.3), stringsAsFactors = FALSE)
nsim <- 200
results <- list()

for(i in seq_len(nrow(params_grid))){
  pg <- params_grid[i, ]
  atts <- numeric(nsim)
  for(sim in 1:nsim){
    simdt <- simulate_agg(agg, delta = pg$delta, sigma_p = pg$sigma_p, rho_time = pg$rho, scaleN = 1)
    atts[sim] <- estimate_att_agg(simdt)
  }
  power <- mean(abs(atts) > 2 * sd(atts)) # crude: fraction where estimate > 2*sd ~ roughly detect
  results[[i]] <- data.frame(delta = pg$delta, sigma_p = pg$sigma_p, rho = pg$rho, mean_att = mean(atts), sd_att = sd(atts), power_approx = power)
}
res_df <- bind_rows(results)

# Save results
out_dir <- "output/results"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
write.csv(res_df, file = file.path(out_dir, "simulations_power_grid_summary.csv"), row.names = FALSE)

# Plot a simple visualization
p <- ggplot(res_df, aes(x = delta, y = mean_att, color = as.factor(sigma_p))) +
  geom_point() + geom_line(aes(group = sigma_p)) +
  labs(title = "Simulated ATT by true delta and province SD", x = "True delta (pp)", y = "Mean estimated ATT") +
  theme_minimal()

ggsave(filename = file.path(out_dir, "simulated_att_summary.png"), plot = p, width = 7, height = 4)
