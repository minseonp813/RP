# Run from Code; inputs are class-clustered means from 99_26_risk_survey_review.do.
library(ggplot2)
library(readr)

out <- "results/new_indices/risk_survey"
means <- read_csv(file.path(out, "response_means.csv"), show_col_types = FALSE)
labels <- list(
  cooperation = as.character(1:5),
  similar = c("Very\nDifferently", "Somewhat\nDifferently", "Somewhat\nSimilar", "Mostly\nSimilar"),
  whose = c("Mostly\nPartner's", "Both", "Mostly\nMine", "Neither")
)
titles <- c(distance = "Revealed-preference distance", ccei_g = "Group CCEI", ceiv_g = "Group CEIV")
keys <- unique(means[c("question", "sample", "outcome")])
for (i in seq_len(nrow(keys))) {
  key <- keys[i, ]
  d <- subset(means, question == key$question & sample == key$sample & outcome == key$outcome)
  d <- d[order(d$response), ]
  d$answer <- factor(d$response, levels = seq_along(labels[[key$question]]))
  d$fill <- ifelse(key$question == "whose" & d$response == 4, "neither", "response")
  xlabels <- paste0(labels[[key$question]], "\n(N=", d$n, ")")
  p <- ggplot(d, aes(answer, mean)) +
    geom_col(aes(fill = fill), width = .78) +
    geom_errorbar(aes(ymin = low, ymax = high), width = .22, linewidth = .5) +
    scale_fill_manual(values = c(response = "lightblue", neither = "grey90"), guide = "none") +
    scale_x_discrete(labels = xlabels) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2), expand = expansion(mult = c(0, .01))) +
    labs(title = titles[[key$outcome]], x = if (key$question == "cooperation") "Cooperation score (higher = more cooperation)" else NULL,
         y = "Mean index") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(), axis.text.x = element_text(size = 11),
          plot.title = element_text(size = 16, face = "bold"), axis.title.x = element_text(size = 12),
          plot.margin = margin(12, 8, 8, 8))
  ggsave(file.path(out, paste(key$question, key$sample, key$outcome, "png", sep = ".")),
         p, width = 5, height = 4.1, dpi = 200)
}
