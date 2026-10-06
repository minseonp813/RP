library(ggplot2)

# Run from Code after 99_34_figure6_non_male_male.do; optional full_gender comparison.
args <- commandArgs(trailingOnly = TRUE)
full_gender <- length(args) > 0 && args[1] == "full_gender"
out <- "results/new_indices/non_male_male"
d <- read.csv(file.path(out, "figure6_ame.csv"))
if (full_gender) {
  out <- paste0(out, "_full_gender")
  updated <- read.csv(file.path(out, "figure6_ame.csv"))
  updated <- updated[updated$sample == "full", ]
  updated$sample <- "full_gender"
  d <- rbind(d[d$sample == "full", ], updated)
}
stopifnot(nrow(d) == 16, !anyDuplicated(d[c("sample", "outcome", "member")]))
d[c("estimate", "low", "high")] <- 100 * d[c("estimate", "low", "high")]
d$sample <- factor(d$sample, levels = if (full_gender) c("full", "full_gender") else c("full", "non_male_male"),
                   labels = if (full_gender) c("Mixed-sex indicator only", "Full gender-pair indicators") else
                     c("Full sample", "Excluding male–male pairs"))
d$member <- factor(d$member, levels = c("maximum", "minimum"),
                   labels = c("Higher individual CCEI (maximum)", "Lower individual CCEI (minimum)"))
d$y <- 5 - d$outcome + ifelse(d$sample == levels(d$sample)[1], 0.11, -0.11)
d$colour <- ifelse(d$sample == levels(d$sample)[1], "grey55",
                   ifelse(d$member == levels(d$member)[1], "blue", "red"))
p <- ggplot(d, aes(estimate, y, colour = colour, shape = sample)) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.35) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = 0.10, linewidth = 0.5) +
  geom_point(size = 2.8, stroke = 0.8) +
  facet_wrap(~member, nrow = 1) +
  scale_colour_identity() +
  scale_shape_manual(values = c(1, 16)) +
  scale_x_continuous(limits = c(-25, 35), breaks = seq(-20, 30, 10)) +
  scale_y_continuous(breaks = 1:4, limits = c(0.5, 4.5),
                     labels = c("CCEI = 1\nCEIV = 1", "CCEI < 1\nCEIV = 1",
                                "CCEI = 1\nCEIV < 1", "CCEI < 1\nCEIV < 1")) +
  labs(title = if (full_gender) "Figure 6: full gender-pair controls" else "Figure 6: excluding male–male pairs",
       subtitle = if (full_gender) "Same full sample in both models: 652 pairs, 64 classes" else
         "Full sample: 652 pairs, 64 classes  |  Restricted sample: 327 pairs, 40 classes",
       x = "Average marginal effect (percentage points)", y = NULL, shape = NULL,
       caption = paste0("Effects for a 0.1 increase in individual CCEI; bars show 95% confidence intervals.\n",
                        if (full_gender) "Mixed-sex and male–male indicators, with female–female as reference; other controls unchanged." else
                          "Same controls and class fixed effects; standard errors clustered by class.")) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        strip.text = element_text(size = 13), legend.position = "bottom",
        plot.title = element_text(face = "bold"), plot.caption = element_text(hjust = 0),
        plot.margin = margin(12, 15, 10, 12)) +
  guides(shape = guide_legend(override.aes = list(colour = c("grey55", "black"))))
ggsave(file.path(out, "figure6_comparison.png"), p, width = 12, height = 6.2, dpi = 180, bg = "white")
