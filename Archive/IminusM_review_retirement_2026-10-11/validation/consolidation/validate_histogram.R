################################################################################
# 09_Figures_Appendix.R
# Latest update: 2026-10-10
# Locked draft order: Figures A4 (CCEI CDFs), A5 (RA CDFs), A6 (I-M histogram),
# A7 (Table 3 Shapley decomposition), A8 (placebo CCEI-difference coefficients),
# followed by supplementary figures.
# Purpose: appendix figures, including the current I-M histogram (A6) and
#          the four-specification Shapley decomposition (A7) and placebo panels (A8).
# Inputs: Code/data panels, retained validated CCEI/M panel, and the
#         Figure A7 CSV from 08 and the six placebo-coefficient distributions from 07.
# Outputs: results/figures/figure_A7/*.pdf (copied to Overleaf), hist_IminusM_ig.png
#          figure4_placebo_col{5,6}.pdf and the other appendix figures.
#          The A7/A8 review PNGs are retained for chat.
################################################################################

rm(list = ls())

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) == 1) {
  code_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
} else if (requireNamespace("rstudioapi", quietly = TRUE) &&
           rstudioapi::isAvailable()) {
  code_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  code_dir <- getwd()
}

.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
suppressPackageStartupMessages({
  library(haven)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(grid)
})

data_dir <- file.path(code_dir, "data")
result_dir <- file.path(code_dir, "results")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
figure_dir <- file.path(result_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

save_appendix_figure <- function(plot, filename, width = 7, height = 5) {
  output_file <- file.path(figure_dir, filename)
  ggsave(output_file, plot, width = width, height = height, dpi = 300)
  stopifnot(file.copy(output_file,
    file.path(code_dir, "../Overleaf/figures_2025", filename), overwrite = TRUE))
}

panel_individual <- read_dta(file.path(data_dir, "panel_individual.dta"))

sig_mark <- function(p) {
  dplyr::case_when(
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.10 ~ "+",
    TRUE ~ ""
  )
}

cdf_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.18, 0.84),
    legend.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

make_wave_cdf <- function(data, value, x_label, x_limits = c(0, 1)) {
  ggplot(
    data,
    aes(
      x = .data[[value]],
      colour = wave,
      linetype = wave
    )
  ) +
    stat_ecdf(geom = "step", linewidth = 0.8, pad = FALSE) +
    scale_colour_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
    scale_linetype_manual(values = c("Baseline" = "solid", "Endline" = "dashed")) +
    scale_x_continuous(
      limits = x_limits,
      breaks = seq(0, 1, by = 0.2),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      expand = c(0, 0)
    ) +
    labs(
      x = x_label,
      y = "Cumulative Frequency",
      colour = NULL,
      linetype = NULL
    ) +
    cdf_theme
}

################################################################################
# Figure A4: CDF Of Individual And Group CCEIs
################################################################################

individual_ccei_data <- panel_individual |>
  filter(!is.na(ccei_i), !is.na(post)) |>
  distinct(id, post, ccei_i) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

group_ccei_data <- panel_individual |>
  filter(!is.na(ccei_g), !is.na(post)) |>
  distinct(group_id, post, ccei_g) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

individual_ccei_cdf <- make_wave_cdf(
  individual_ccei_data, "ccei_i", "Individual CCEI"
)
group_ccei_cdf <- make_wave_cdf(
  group_ccei_data, "ccei_g", "Group CCEI", c(0.1, 1)
)

save_appendix_figure(
  individual_ccei_cdf, "figure_individual_ccei_cdf_baseline_endline.png"
)
save_appendix_figure(
  group_ccei_cdf, "figure_group_ccei_cdf_baseline_endline.png"
)

################################################################################
# Figure A5: CDF Of Individual And Group Risk Aversion
################################################################################

individual_ra_data <- panel_individual |>
  filter(!is.na(RA_i), !is.na(post)) |>
  distinct(id, post, RA_i) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

group_ra_data <- panel_individual |>
  filter(!is.na(RA_g), !is.na(post)) |>
  distinct(group_id, post, RA_g) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

individual_ra_cdf <- make_wave_cdf(
  individual_ra_data, "RA_i", "Individual Risk Aversion"
)
group_ra_cdf <- make_wave_cdf(
  group_ra_data, "RA_g", "Group Risk Aversion"
)

save_appendix_figure(
  individual_ra_cdf, "figure_individual_ra_cdf_baseline_endline.png"
)
save_appendix_figure(
  group_ra_cdf, "figure_group_ra_cdf_baseline_endline.png"
)

################################################################################
# Figure A6: same I-M data and pooled member-category percentages as the main figure.
################################################################################
panel <- read_dta(file.path(code_dir, "data/panel_individual.dta"))
stopifnot(nrow(panel) == 2608,
          max(abs(panel$Istar_ccei - (panel$I_ccei - panel$M_ccei)), na.rm = TRUE) < 1e-12,
          all(panel$n_ccei_donors == 651))
defined <- panel[!is.na(panel$Istar_ccei), ]
stopifnot(nrow(defined) == 2560,
          max(abs(aggregate(Istar_ccei ~ group_id + post, defined, sum)$Istar_ccei)) < 1e-7)
defined$member <- factor(defined$HighCCEI_both_high, levels = c(1, 0),
                        labels = c("Higher CCEI", "Lower CCEI"))
# Normalize percentages within member category, pooling both waves.
defined$weight <- 100 / as.numeric(table(defined$member)[defined$member])
histogram <- ggplot(defined, aes(Istar_ccei, weight = weight,
                               fill = member, colour = member)) +
  geom_histogram(binwidth = .05, boundary = 0, closed = "left",
                 position = "identity", alpha = .35, linewidth = .4) +
  scale_fill_manual(values = c("#4C78A8", "#E69191")) +
  scale_colour_manual(values = c("#2C6FA0", "#E69191")) +
  scale_x_continuous(breaks = seq(-1, 1, .25)) +
  coord_cartesian(xlim = c(-1, 1), expand = FALSE) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, .05))) +
  labs(x = expression("Placebo-adjusted revealed-preference distance (" * I[ig] - M[ig] * ")"),
       y = "Percent", fill = NULL, colour = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom", plot.margin = margin(6, 18, 6, 8),
        plot.background = element_rect(fill = "white", colour = NA))
save_appendix_figure(histogram, "hist_IminusM_ig.png", width = 8, height = 5.1)

################################################################################
