# Adopted appendix figures: adjusted-distance Shapley decomposition and Figure A6.
# Run from Code. Inputs: outputs/tables/shapley_results.csv and
# outputs/data/ccei_ra_candidate_analysis.dta in IminusM_review.
# Outputs: both figures in Overleaf/figures_2025; the Figure A6 histogram is also
# saved in IminusM_review/outputs/adopted.
suppressPackageStartupMessages({
  library(haven)
  library(ggplot2)
})
d <- read.csv("IminusM_review/outputs/tables/shapley_results.csv")
d <- subset(d, outcome_model == "adjusted")
stopifnot(abs(sum(d$shapley_percent) - 100) < 1e-4)
d$block <- factor(d$block, levels = rev(d$block))
p <- ggplot(d, aes(shapley_percent, block)) +
  geom_col(fill = "steelblue", width = .65) +
  geom_text(aes(label = sprintf("%.1f%%", shapley_percent)), hjust = -.15, size = 4) +
  scale_x_continuous(limits = c(0, 75), expand = expansion(mult = c(0, .02))) +
  labs(x = "Share of explained variation (%)", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
ggsave("../Overleaf/figures_2025/shapley_bargaining_index_M.png", p, width=7, height=4.5, dpi=300)

# Figure A6: same I-M data and pooled member-category percentages as the main figure.
panel <- read_dta("IminusM_review/outputs/data/ccei_ra_candidate_analysis.dta")
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
figure_file <- "IminusM_review/outputs/adopted/hist_IminusM_ig.png"
ggsave(figure_file, histogram, width = 8, height = 5.1, dpi = 300)
file.copy(figure_file, "../Overleaf/figures_2025/hist_IminusM_ig.png", overwrite = TRUE)
