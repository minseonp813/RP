# Four categorical quadrants for the Section 6.3 review packet.
suppressPackageStartupMessages({
  library(haven)
  library(ggplot2)
})
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "results/new_indices/collective_rationality_summary"
d <- read_dta(file.path(out, "analysis_sample.dta"))
stopifnot(nrow(d) == 1304, length(unique(d$group_id)) == 652,
          !anyDuplicated(d[c("group_id", "post")]),
          all(is.finite(as.matrix(d[c("ccei_g", "ceiv_g", "CEIV_lower", "CEIV_upper")]))))
category <- 1L + (d$ccei_g >= 1 - 1e-9) + 2L * (d$ceiv_g >= 1 - 1e-9)
stopifnot(all(category == d$joint_category),
          all((d$ceiv_g >= 1 - 1e-9) == (d$CEIV_lower >= 1 - 1e-9)),
          all((d$ceiv_g >= 1 - 1e-9) == (d$CEIV_upper >= 1 - 1e-9)))
counts <- data.frame(outcome = 1:4, ccei_one = c(0, 1, 0, 1), ceiv_one = c(0, 0, 1, 1),
                     n = tabulate(category, nbins = 4))
counts$share <- counts$n / nrow(d)
ames <- read.csv(file.path(out, "figure6_ame.csv"))
stopifnot(sum(counts$n) == 1304,
          max(abs(ames$share - counts$share[ames$outcome])) < 1e-12)
write.csv(counts, file.path(out, "joint_outcome_counts.csv"), row.names = FALSE)
counts$label <- sprintf("%s pair-waves\n%.1f%%", format(counts$n, trim = TRUE), 100 * counts$share)

p <- ggplot(counts, aes(ccei_one, ceiv_one)) +
  geom_tile(aes(fill = factor(outcome)), width = 1, height = 1) +
  geom_vline(xintercept = .5, colour = "grey25", linewidth = .7) +
  geom_hline(yintercept = .5, colour = "grey25", linewidth = .7) +
  geom_text(aes(label = label), size = 6.5, lineheight = 1.4, colour = "grey15") +
  scale_fill_manual(values = c("#F3DBDB", "#E6EDF5", "#F3ECD4", "#DCEADD")) +
  scale_x_continuous(breaks = 0:1, labels = c("CCEI < 1", "CCEI = 1"),
                     limits = c(-.5, 1.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = 0:1, labels = c("CEIV < 1", "CEIV = 1"),
                     limits = c(-.5, 1.5), expand = c(0, 0)) +
  coord_fixed() +
  labs(x = "Group CCEI", y = "Group CEIV") +
  theme_classic(base_size = 17) +
  theme(legend.position = "none", axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_text(colour = "grey15"),
        axis.text.x = element_text(margin = margin(t = 12)),
        axis.text.y = element_text(margin = margin(r = 12)),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        panel.border = element_rect(colour = "grey25", fill = NA, linewidth = .7),
        plot.margin = margin(15, 15, 15, 15))
ggsave(file.path(out, "joint_outcome_quadrants.pdf"), p, width = 7, height = 6.3)
print(counts)
