library(tidyverse)
library(ggplot2)
library(showtext)

showtext_auto()
font_add("Times New Roman", regular = "/System/Library/Fonts/Supplemental/Times New Roman.ttf")

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

all_stacked_results_summary <- read_csv("output/results/stacked_did_results_summary.csv")
dynamic_effects_data <- read_csv("output/results/stacked_did_dynamic_effects.csv") |>
  filter(rel_time_num >= -11 & rel_time_num <= 11) |>
  filter(!is.na(se))

fig1 <- ggplot(dynamic_effects_data, aes(x = rel_time_num, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "steelblue") +
  geom_point(color = "steelblue", size = 2) +
  geom_line(color = "steelblue") +
  scale_x_continuous(breaks = seq(-10, 10, 2)) +
  labs(
    title = "Dynamic Treatment Effects on Maternal Labour Force Participation",
    subtitle = "Notes: Authors' calculations using Labour Force Survey microdata (April 2019–July 2025).",
    x = "Months Relative to Treatment",
    y = "ATT Estimate"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0, size = 8, face = "italic"))

ggsave("output/figures/figure1_event_study_plot.png", fig1, width = 8, height = 6, dpi = 300)
ggsave("output/figures/figure1_event_study_plot.pdf", fig1, width = 8, height = 6)

plot_data_subgroups <- all_stacked_results_summary %>%
  filter(subgroup != "Full Sample")

fig2 <- ggplot(plot_data_subgroups, aes(x = estimate, y = reorder(subgroup, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
  geom_point(size = 3, color = "steelblue") +
  labs(
    title = "Heterogeneous Treatment Effects by Demographic Subgroup",
    subtitle = "Notes: Authors' calculations using Labour Force Survey microdata (April 2019–July 2025).",
    x = "ATT Estimate",
    y = ""
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0, size = 8, face = "italic"))

ggsave("output/figures/figure2_heterogeneity_plot.png", fig2, width = 8, height = 6, dpi = 300)
ggsave("output/figures/figure2_heterogeneity_plot.pdf", fig2, width = 8, height = 6)
