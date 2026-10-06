library(ggplot2)

# Optional arguments: input CSV, output dir, classification (original/a/b), portrait.
args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1) args[1] else "results/cei_joint_ame_pooled.csv"
output_dir <- if (length(args) >= 2) args[2] else "results/figures"
classification <- if (length(args) >= 3) args[3] else "original"
portrait <- length(args) >= 4 && args[4] == "portrait"
stopifnot(classification %in% c("original", "a", "b"))
n_categories <- if (classification == "b") 3L else 4L
prefix <- if (classification == "original") "cei" else paste0("figure6_", classification)
d <- read.csv(input)
split_sample <- "ra_high" %in% names(d)
communication_split <- all(c("moderator", "group") %in% names(d))
split_fields <- if (communication_split) c("moderator", "group") else if (split_sample) "ra_high" else character()
panels <- if (length(split_fields)) unique(d[split_fields]) else data.frame(panel = 1)
keys <- c(split_fields, "outcome", "member")
sum_groups <- if (length(split_fields)) do.call(interaction, c(d[c(split_fields, "member")], list(drop = TRUE))) else d$member
stopifnot(nrow(d) == 2 * n_categories * nrow(panels), !anyDuplicated(d[keys]),
          all(is.finite(as.matrix(d[c("estimate", "low", "high")]))),
          all(abs(tapply(d$estimate, sum_groups, sum)) < 1e-8))
d[c("estimate", "low", "high")] <- 100 * d[c("estimate", "low", "high")]
d$outcome <- factor(d$outcome, levels = n_categories:1)
outcome_labels <- c("1" = "CCEI < 1\nCEI < 1", "2" = "CCEI = 1\nCEI < 1",
                    "3" = "CCEI < 1\nCEI = 1", "4" = "CCEI = 1\nCEI = 1")

if (classification == "a") {
  outcome_labels <- c("1" = "CCEI < 1\nCEIV < 1", "2" = "CCEI = 1\nCEIV < 1",
                      "3" = "CCEI < 1\nCEIV = 1", "4" = "CCEI = 1\nCEIV = 1")
}
if (classification == "b") {
  outcome_labels <- c("1" = "CEIV < 1\nCEIC < 1", "2" = "CEIV = 1\nCEIC < 1",
                      "3" = "CEIV = 1\nCEIC = 1")
}
# Use a common axis across split samples, expanding only when their intervals require it.
limits <- c(-20, 30)
if (length(split_fields)) limits <- c(min(-20, floor(min(d$low) / 10) * 10),
                             max(30, ceiling(max(d$high) / 10) * 10))
stopifnot(all(d$low >= limits[1]), all(d$high <= limits[2]))

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
for (panel in seq_len(nrow(panels))) {
  keep <- rep(TRUE, nrow(d))
  for (field in split_fields) keep <- keep & d[[field]] == panels[[field]][panel]
  sample <- d[keep, ]
  sample_prefix <- if (communication_split) paste(prefix, panels$moderator[panel], panels$group[panel], sep = "_") else
    if (split_sample) paste0(prefix, "_", c("similar", "different")[panels$ra_high[panel] + 1]) else prefix
  for (member in names(member_colours)) {
    p <- ggplot(sample[sample$member == member, ], aes(estimate, outcome)) +
      geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.35) +
      geom_errorbar(aes(xmin = low, xmax = high), orientation = "y",
                    width = 0.12, linewidth = 0.55, colour = "grey35") +
      geom_point(size = 3, colour = member_colours[member]) +
      scale_x_continuous(limits = limits, breaks = seq(limits[1], limits[2], 10)) +
      scale_y_discrete(labels = outcome_labels, expand = expansion(add = 0.55)) +
      labs(x = "Average marginal effect (percentage points)", y = NULL) +
      paper_theme
    ggsave(file.path(output_dir, paste0(sample_prefix, "_ame_", member, ".png")), p,
           width = if (portrait) 5.5 else 6, height = if (portrait) 3 else 5, dpi = 300)
  }
}
