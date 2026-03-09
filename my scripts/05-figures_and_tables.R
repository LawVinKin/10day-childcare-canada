# Generate figures and tables for paper
# This script creates the event-study figure and summary statistics table

library(tidyverse)

source("my scripts/06-utilities.R")


### Main function

run_figures_and_tables <- function(force = FALSE) {

  # Load analysis results
  outcomes <- get_or_run_csv("output/results/multiple_outcomes_results.csv", function() stop("missing outcomes - run 03-main_analysis.R first"), force = force)
  subgroups <- get_or_run_csv("output/results/expanded_subgroup_results.csv", function() stop("missing subgroups"), force = force)
  dynamic <- get_or_run_csv("output/results/stacked_did_dynamic_effects.csv", function() stop("missing dynamic effects"), force = force)
  placebo <- get_or_run_csv("output/results/placebo_tests_summary.csv", function() tibble(), force = force)

  # Load analysis data for summary statistics
  df <- readr::read_rds("data/03-analysis_data/processed/analysis_data_stacked.rds")

  # Create output directories
  dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
  dir.create("paper/figures_highres", recursive = TRUE, showWarnings = FALSE)

  # Event-study figure
  dynamic_clean <- dynamic %>%
    filter(rel_time_num >= -44 & rel_time_num <= 15) %>%
    filter(!is.na(se))

  p_event_quarto <- ggplot(dynamic_clean, aes(x = rel_time_num, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#1B4F72", linewidth = 0.8) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.18, fill = "#4682B4") +
    geom_point(color = "#1B4F72", size = 2.2, shape = 21, fill = "white", stroke = 1) +
    geom_line(color = "#1B4F72", linewidth = 0.8) +
    scale_x_continuous(breaks = seq(-44, 15, by = 4), name = "Months Relative to Policy Implementation") +
    scale_y_continuous(name = "Estimated Effect on Maternal Labour Force Participation (pp)") +
    labs(
      title = "Dynamic Effects of Childcare Policy on Maternal Labour Force Participation",
      subtitle = "Stacked DiD Event Study (2019-2025)\nShaded area: 95% CI\nVertical line: treatment month",
      caption = "Source: Author calculations using Statistics Canada LFS PUMF"
    ) +
    theme_minimal(base_size = 12, base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", size = 11, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 1),
      axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 8)),
      axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 8)),
      axis.text = element_text(size = 11),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.4),
      plot.background = element_rect(fill = "#F8F9FA", color = NA)
    )

  ggsave("paper/figures_highres/fig_event_study_quarto.pdf", p_event_quarto, width = 9, height = 6, dpi = 300)

  # Summary statistics table
  summary_stats <- tibble(
    variable = c("Labour Force Participation", "Employment", "Full-Time Employment", "Part-Time Employment"),
    mean = c(
      mean(df$in_lfp, na.rm = TRUE),
      mean(df$in_employed, na.rm = TRUE),
      mean(df$in_ft_employed, na.rm = TRUE),
      mean(df$in_pt_employed, na.rm = TRUE)
    ),
    sd = c(
      sd(df$in_lfp, na.rm = TRUE),
      sd(df$in_employed, na.rm = TRUE),
      sd(df$in_ft_employed, na.rm = TRUE),
      sd(df$in_pt_employed, na.rm = TRUE)
    ),
    N = c(
      sum(!is.na(df$in_lfp)),
      sum(!is.na(df$in_employed)),
      sum(!is.na(df$in_ft_employed)),
      sum(!is.na(df$in_pt_employed))
    )
  )

  write_csv(summary_stats, "output/results/table1_summary_statistics.csv")

  # Validation statistics for paper verification
  validation_stats <- tibble(
    stat_name = c(
      "total_observations",
      "earnings_observations", 
      "earnings_coverage_pct",
      "non_student_n",
      "student_n",
      "employed_n",
      "ft_employed_n",
      "pt_employed_n"
    ),
    value = c(
      sum(!is.na(df$in_lfp)),
      sum(!is.na(df$earnings_hourly)),
      round(sum(!is.na(df$earnings_hourly)) / sum(!is.na(df$in_lfp)) * 100, 1),
      sum(df$student_status == "Non-student", na.rm = TRUE),
      sum(df$student_status %in% c("Full-time student", "Part-time student"), na.rm = TRUE),
      sum(!is.na(df$in_employed)),
      sum(!is.na(df$in_ft_employed)),
      sum(!is.na(df$in_pt_employed))
    )
  )

  write_csv(validation_stats, "output/results/validation_stats.csv")

  invisible(list(figures = "paper/figures_highres/fig_event_study_quarto.pdf"))
}


# The following line runs figure/table generation when this script is sourced. Set force = TRUE to regenerate all outputs.
invisible(run_figures_and_tables(force = FALSE))
