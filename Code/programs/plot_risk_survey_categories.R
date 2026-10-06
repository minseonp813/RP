# Run from Code after 99_27_risk_survey_joint_categories.do.
library(ggplot2)
library(readr)
out <- "results/new_indices/risk_survey_categories"
shares <- read_csv(file.path(out, "response_shares.csv"), show_col_types = FALSE)
means <- read_csv(file.path(out, "score_means.csv"), show_col_types = FALSE)
answers <- list(cooperation = paste("Score", 1:5),
                similar = c("Very differently", "Somewhat differently", "Somewhat similar", "Mostly similar"),
                whose = c("Mostly partner's", "Both", "Mostly mine", "Neither"))
colours <- list(cooperation = c("#bdbdbd", "#c6dbef", "#9ecae1", "#6baed6", "#2171b5"),
                similar = c("#bdbdbd", "#c6dbef", "#6baed6", "#2171b5"),
                whose = c("#f4b5b5", "#9ecae1", "#ffd591", "#dedede"))
base_theme <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.y = element_text(colour = "black"), plot.title = element_text(face = "bold"),
        legend.position = "bottom", legend.text = element_text(size = 10),
        plot.margin = margin(10, 10, 8, 8))

for (samp in c("all", "less")) {
prefix <- if (samp == "all") "" else "less_"
for (q in names(answers)) {
  d <- subset(shares, question == q & sample == samp)
  d$category <- factor(d$joint_category, levels = 4:1)
  d$answer <- factor(d$response, levels = seq_along(answers[[q]]))
  counts <- unique(d[c("joint_category", "total")])
  counts <- counts[order(counts$joint_category), ]
  ylabels <- setNames(paste0("Category ", counts$joint_category, " (N=", counts$total, ")"), counts$joint_category)
  p <- ggplot(d, aes(share, category, fill = answer)) +
    geom_col(width = .7, position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = ifelse(share >= .07, sprintf("%.1f%%", 100*share), ""),
                  colour = ifelse(q != "whose" & response == length(answers[[q]]), "white", "black")),
              position = position_stack(vjust = .5, reverse = TRUE), size = 3.4) +
    scale_colour_identity() +
    scale_fill_manual(values = colours[[q]], labels = answers[[q]], drop = FALSE) +
    scale_y_discrete(labels = ylabels) +
    scale_x_continuous(breaks = seq(0, 1, .25), labels = function(x) paste0(100*x, "%"),
                       expand = expansion(mult = c(0, .01))) +
    coord_cartesian(xlim = c(0, 1)) +
    guides(fill = guide_legend(nrow = if (q == "similar") 2 else 1, byrow = TRUE)) +
    labs(title = "Response distribution", x = "Share within CCEI-CEIV category", y = NULL, fill = NULL) + base_theme
  ggsave(file.path(out, paste0(prefix, q, "_distribution.png")), p, width = 6, height = 4, dpi = 220)
  if (q != "whose") {
    d <- subset(means, question == q & sample == samp)
    d$category <- factor(d$joint_category, levels = 4:1)
    p <- ggplot(d, aes(mean, category)) +
      geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = .2) +
      geom_point(size = 3, colour = "#2171b5") +
      geom_text(aes(label = sprintf("%.2f", mean)), vjust = -1, size = 3.5) +
      scale_y_discrete(labels = function(x) paste("Category", x)) +
      scale_x_continuous(limits = c(1, length(answers[[q]])), breaks = seq_along(answers[[q]])) +
      labs(title = "Average coded score", x = "Mean score and 95% CI", y = NULL) + base_theme
    ggsave(file.path(out, paste0(prefix, q, "_mean.png")), p, width = 4, height = 4, dpi = 220)
  }
}
}
