################################################################################
# 6_Figures_Appendix.R: Locked generated appendix panels in draft order.
# Last updated: 2026-10-11. Run 3/5 first; run this script from Code.
# A4/A5: CCEI/RA CDFs; A6: I-M histogram; A7: Table 3 R2 decomposition;
# A8: Table 3 Columns (5)/(6) placebo tests; A10: joint-outcome R2 decomposition;
# A11: multinomial AMEs; A12: alternative-index LPMs.
# A1--A3 are photographs/LaTeX drawings; A9 belongs to 99_6's simulations.
# Manuscript panels retain their existing results/figures and Overleaf paths.
# Unused plots and review previews are in 99_10_Figures_Appendix_Backup.R.
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
# Figure A7: four Table 3 specifications, with separate manuscript subfigures.
# Run independently from Code after the matching section of 5.
################################################################################
if (!exists("code_dir")) code_dir <- getwd()
source(file.path(code_dir, "programs/plot_table3_shapley.R"))
a7_files <- plot_table3_shapley(code_dir)
manuscript_dir <- file.path(code_dir, "../Overleaf/figures_2025/figure_A7")
dir.create(manuscript_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(all(file.copy(a7_files, manuscript_dir, overwrite = TRUE)))


################################################################################
# Figure A8: Placebo tests for Table 3 Columns (5)/(6), immediately after A7.
# Run from Code after 3_Tables_Main.do; main Figure 4 is not a prerequisite.
if (!exists("code_dir")) code_dir <- getwd()
.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(ggplot2)
library(dplyr)
source(file.path(code_dir, "programs/estimate_table3_placebo.R"))
estimate_table3_placebo(code_dir, c(5L, 6L), "figureA8_placebo")
source(file.path(code_dir, "programs/plot_table3_placebo.R"))
export_table3_placebo_panels(code_dir, c(5L, 6L), input_prefix = "figureA8_placebo")

################################################################################
# Figure A10: Joint-Outcome Overall-R2 Decomposition
# Run this section from Code after the collective-outcomes section in 5.
################################################################################
if (!exists("code_dir")) code_dir <- getwd()
source(file.path(code_dir, "programs/plot_joint_outcome_shapley.R"))
shapley_files <- plot_joint_outcome_shapley(code_dir)
draft_dir <- file.path(code_dir, "../Overleaf/figures_2025/collective_rationality")
dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(shapley_files, file.path(draft_dir, basename(shapley_files)), overwrite = TRUE))

################################################################################
# Figure A11: Multinomial-Logit Individual Rationality And Joint Outcomes
################################################################################
# Run this section from Code after the collective-outcomes section in 5.
if (!exists("code_dir")) code_dir <- getwd()
.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(ggplot2)
source(file.path(code_dir, "programs/plot_collective_ame.R"))
sample <- haven::read_dta(file.path(code_dir, "data/collective_analysis_sample.dta"))
member_sd <- c(maximum = sd(sample$ccei_max), minimum = sd(sample$ccei_min))
collective_result_dir <- file.path(code_dir, "results/collective_quality")
collective_figure_dir <- file.path(code_dir, "results/figures/collective_rationality")
dir.create(collective_figure_dir, recursive = TRUE, showWarnings = FALSE)
ame_files <- plot_cei_ame(file.path(collective_result_dir, "joint_outcome_ame.csv"),
                         collective_figure_dir, classification = "a", portrait = TRUE,
                         member_sd = member_sd)
draft_figure_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(draft_figure_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(ame_files, file.path(draft_figure_dir, basename(ame_files)), overwrite = TRUE))

################################################################################
# Figure A12: Alternative Rationality And Joint Collective Outcomes
# Run this section from Code after the alternative-index section in 5.
################################################################################
if (!exists("code_dir")) code_dir <- getwd()
.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(ggplot2)
source(file.path(code_dir, "programs/plot_collective_ame.R"))
out <- file.path(code_dir, "results/collective_quality/extensions")
figure_dir <- file.path(code_dir, "results/figures/collective_rationality")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
# Alternative-index appendix panels use Figure 8's LPM specifications and style.
a <- read.csv(file.path(out, "alternative_linear_ame.csv"))
stopifnot(nrow(a) == 48, setequal(a$measure, c("HM", "MaxMPI")))
sample <- haven::read_dta(file.path(code_dir, "data/collective_analysis_sample.dta"))
indices <- list(HM = cbind(1 - sample$hm_i / 18, 1 - sample$hm_j / 18),
                MaxMPI = cbind(1 - sample$maxmpi_i, 1 - sample$maxmpi_j))
labels <- c(HM = "HM index", MaxMPI = "RevMaxMPI")
alternative_files <- character()
units <- data.frame()
for (measure in names(indices)) {
  scores <- indices[[measure]]
  member_sd <- c(maximum = sd(pmax(scores[, 1], scores[, 2])),
                 minimum = sd(pmin(scores[, 1], scores[, 2])))
  units <- rbind(units, data.frame(measure = measure, member = names(member_sd),
                                  sd = unname(member_sd), n = nrow(sample)))
  input <- tempfile(fileext = ".csv")
  write.csv(a[a$measure == measure, ], input, row.names = FALSE)
  alternative_files <- c(alternative_files,
    plot_cei_ame(input, figure_dir, classification = "a", portrait = TRUE,
                 prefix = paste0("alternative_", measure, "_lpm"),
                 member_sd = member_sd, consistency_label = labels[[measure]]))
  unlink(input)
}
write.csv(units, file.path(out, "alternative_sd_units.csv"), row.names = FALSE)
draft_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(alternative_files, file.path(draft_dir, basename(alternative_files)), overwrite = TRUE))

message("6_Figures_Appendix.R completed. Outputs: ", file.path(code_dir, "results/figures"))
