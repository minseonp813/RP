# Run after 99_32_figure6_threshold_sensitivity.do both.
library(ggplot2)
library(haven)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
code_dir <- dirname(dirname(normalizePath(script_file)))
out <- file.path(code_dir, "results/new_indices/threshold_sensitivity")
d <- read.csv(file.path(out, "both_ame.csv"))
stopifnot(nrow(d) == 32, !anyDuplicated(d[c("epsilon", "outcome", "member")]),
          all(d$n == 1304), all(d$pairs == 652), all(d$clusters == 64),
          all(is.finite(as.matrix(d[c("estimate", "se", "low", "high")]))),
          all(abs(aggregate(estimate ~ epsilon + member, d, sum)$estimate) < 1e-7))
d[c("estimate", "low", "high")] <- 100 * d[c("estimate", "low", "high")]
epsilons <- c(0, .001, .01, .05)
epsilon_labels <- c("Exact (epsilon = 0)", "epsilon = 0.001", "epsilon = 0.01", "epsilon = 0.05")
d$cutoff <- factor(d$epsilon, levels = epsilons, labels = epsilon_labels)
d$member <- factor(d$member, levels = c("maximum", "minimum"),
                   labels = c("Maximum individual CCEI", "Minimum individual CCEI"))
d$y <- 5 - d$outcome + c(.24, .08, -.08, -.24)[match(d$epsilon, epsilons)]
lower <- min(-20, floor(min(d$low) / 10) * 10)
upper <- max(30, ceiling(max(d$high) / 10) * 10)
plot <- ggplot(d, aes(estimate, y, colour = cutoff, shape = cutoff)) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = .4) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = .065, linewidth = .6) +
  geom_point(size = 3) +
  facet_wrap(~ member, nrow = 1) +
  scale_colour_manual(values = c("#333333", "#2864B7", "#D97706", "#15803D")) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  scale_x_continuous(limits = c(lower, upper), breaks = seq(lower, upper, 10)) +
  scale_y_continuous(limits = c(.5, 4.5), breaks = 4:1,
                     labels = c("CCEI low\nCEIV low", "CCEI high\nCEIV low",
                                "CCEI low\nCEIV high", "CCEI high\nCEIV high")) +
  labs(title = "Sensitivity to near-one group classifications",
       subtitle = "High: group index >= 1 - epsilon; low: below the cutoff. Both CCEI and CEIV cutoffs vary.",
       x = "Average marginal effect (percentage points)", y = NULL, colour = NULL, shape = NULL,
       caption = "Same joint multinomial-logit specification and 1,304 group-waves at every cutoff. Effects per 0.1 increase in individual CCEI.\n95% confidence intervals clustered by 64 classes. Exact baseline uses the existing 1e-9 numerical tolerance.") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        strip.text = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "bottom", legend.text = element_text(size = 11),
        plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
        plot.caption = element_text(hjust = 0, size = 10, margin = margin(t = 12)),
        plot.margin = margin(16, 16, 16, 16),
        plot.background = element_rect(fill = "white", colour = NA))
ggsave(file.path(out, "both_threshold_ame.png"), plot, width = 13, height = 7, dpi = 300)

sample <- read_dta(file.path(code_dir, "results/new_indices/analysis_sample.dta"))
baseline <- 1 + (sample$ccei_g >= 1 - 1e-9) + 2 * (sample$ceiv_g >= 1 - 1e-9)
counts <- do.call(rbind, lapply(epsilons, function(epsilon) {
  tolerance <- max(epsilon, 1e-9)
  category <- 1 + (sample$ccei_g >= 1 - tolerance) + 2 * (sample$ceiv_g >= 1 - tolerance)
  n <- as.integer(table(factor(category, levels = 1:4)))
  exported <- d[d$epsilon == epsilon & d$member == "Maximum individual CCEI", ]
  exported <- exported[order(exported$outcome), ]
  stopifnot(all(abs(exported$share * 1304 - n) < .001))
  data.frame(epsilon = epsilon, outcome = 1:4, n = n, share = n / 1304,
             reclassified_group_waves = sum(category != baseline))
}))
write.csv(counts, file.path(out, "both_category_counts.csv"), row.names = FALSE)
print(counts)
