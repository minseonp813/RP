library(ggplot2)

# Run from Code after 99_1_Tables_Main.do. Optional arguments: input CSV, output dir.
args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1) args[1] else "results/cei_joint_ame_pooled.csv"
output_dir <- if (length(args) >= 2) args[2] else "results/figures"
d <- read.csv(input)
stopifnot(nrow(d) == 8, !anyDuplicated(d[c("outcome", "member")]),
          all(is.finite(as.matrix(d[c("estimate", "low", "high")]))),
          all(abs(tapply(d$estimate, d$member, sum)) < 1e-8))
d[c("estimate", "low", "high")] <- 100 * d[c("estimate", "low", "high")]
d$outcome <- factor(d$outcome, levels = 4:1)
outcome_labels <- c("1" = "CCEI < 1\nCEI < 1", "2" = "CCEI = 1\nCEI < 1",
                    "3" = "CCEI < 1\nCEI = 1", "4" = "CCEI = 1\nCEI = 1")

# Match 99_2_Figures_Main.R: base size 14, white background, blue/red members,
# no redundant legend; titles, panel captions, and notes belong in LaTeX.
paper_theme <- theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 8)),
        plot.margin = margin(8, 12, 6, 6))
member_colours <- c(maximum = "blue", minimum = "red")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
for (member in names(member_colours)) {
  p <- ggplot(d[d$member == member, ], aes(estimate, outcome)) +
    geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.35) +
    geom_errorbar(aes(xmin = low, xmax = high), orientation = "y",
                  width = 0.12, linewidth = 0.55, colour = "grey35") +
    geom_point(size = 3, colour = member_colours[member]) +
    scale_x_continuous(limits = c(-20, 30), breaks = seq(-20, 30, 10)) +
    scale_y_discrete(labels = outcome_labels, expand = expansion(add = 0.55)) +
    labs(x = "Average marginal effect (percentage points)", y = NULL) +
    paper_theme
  ggsave(file.path(output_dir, paste0("cei_ame_", member, ".png")), p,
         width = 6, height = 5, dpi = 300)
}
