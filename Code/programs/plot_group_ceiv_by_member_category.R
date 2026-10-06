# Member-CCEI category plots. Optional arguments: output directory, ccei/ceiv, portrait.
suppressPackageStartupMessages({
  library(haven)
  library(ggplot2)
  library(dplyr)
  library(grid)
})
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
code_dir <- dirname(dirname(normalizePath(script_file)))
args <- commandArgs(trailingOnly = TRUE)
review <- length(args) >= 1
portrait <- length(args) >= 3 && args[3] == "portrait"
out <- if (review) args[1] else file.path(code_dir, "results/new_indices/member_categories")
outcome <- if (length(args) >= 2) args[2] else "ceiv"
stopifnot(outcome %in% c("ccei", "ceiv"))
variable <- paste0(outcome, "_g")
title <- toupper(outcome)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
d <- read_dta(if (review) file.path(out, "analysis_sample.dta") else
              file.path(code_dir, "results/new_indices/analysis_sample.dta"))
stopifnot(nrow(d) == 1304, !anyDuplicated(d[c("group_id", "post")]),
          all(is.finite(as.matrix(d[c("ccei_min", "ccei_max", "ccei_g", "ceiv_g")]))))
pooled_median <- median(c(d$ccei_min, d$ccei_max))
stopifnot(!any(c(d$ccei_min, d$ccei_max) == pooled_median))
d$pair_category <- factor((d$ccei_min > pooled_median) + (d$ccei_max > pooled_median),
                          levels = 0:2, labels = c("Low-Low", "Low-High", "High-High"))
stats <- d |>
  group_by(pair_category) |>
  summarise(mean = mean(.data[[variable]]), sd = sd(.data[[variable]]), n = n(),
            se = sd / sqrt(n), ci = qt(.975, n - 1) * se, .groups = "drop")
write.csv(stats, file.path(out, paste0("group_", outcome, "_category_means.csv")), row.names = FALSE)
sig_mark <- function(p) if (p < .01) "**" else if (p < .05) "*" else if (p < .1) "+" else ""
comparisons <- list(c("Low-Low", "Low-High"), c("Low-High", "High-High"), c("Low-Low", "High-High"))
diffs <- bind_rows(lapply(comparisons, function(categories) {
  lower <- d[[variable]][d$pair_category == categories[1]]
  upper <- d[[variable]][d$pair_category == categories[2]]
  test <- t.test(lower, upper)
  difference <- mean(upper) - mean(lower)
  data.frame(lower = categories[1], upper = categories[2], difference = difference,
             p = test$p.value, label = sprintf("Diff. = %.3f%s", difference, sig_mark(test$p.value)))
}))
write.csv(diffs, file.path(out, paste0("group_", outcome, "_category_differences.csv")), row.names = FALSE)

y_min <- max(.75, floor((min(stats$mean - stats$ci) - .02) * 100) / 100)
y_top <- max(stats$mean + stats$ci)
bracket_low <- y_top + .025
bracket_high <- y_top + .070
bar <- ggplot(stats, aes(pair_category, mean, fill = pair_category)) +
  geom_col(width = .62, colour = "black", linewidth = .3) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = .16, linewidth = .45) +
  scale_fill_manual(values = c("Low-Low" = "#E39695", "Low-High" = "#D8C98C", "High-High" = "#74A9CF")) +
  scale_x_discrete(labels = c("Low-Low" = "Low\nLow", "Low-High" = "Low\nHigh", "High-High" = "High\nHigh")) +
  scale_y_continuous(breaks = seq(.85, 1, .05), labels = scales::label_number(accuracy = .01), expand = c(0, 0)) +
  coord_cartesian(ylim = c(y_min, bracket_high + .035)) +
  labs(x = NULL, y = paste("Mean Collective", title)) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none", panel.grid.major = element_line(colour = "grey90", linewidth = .4),
        plot.margin = margin(12, 12, 12, 12), plot.background = element_rect(fill = "white", colour = NA))
for (i in 1:3) {
  x1 <- c(1, 2, 1)[i]
  x2 <- c(2, 3, 3)[i]
  y <- if (i == 3) bracket_high else bracket_low
  bar <- bar +
    annotate("segment", x = x1, xend = x2, y = y, yend = y) +
    annotate("segment", x = x1, xend = x1, y = y - .020, yend = y) +
    annotate("segment", x = x2, xend = x2, y = y - .020, yend = y) +
    annotate("label", x = mean(c(x1, x2)), y = y + .012,
             label = diffs$label[i], size = if (portrait) 4.2 else if (review) 3.8 else 4.4, fill = "white")
}
cdf <- ggplot(d, aes(.data[[variable]], colour = pair_category, linetype = pair_category)) +
  stat_ecdf(geom = "step", linewidth = .9, pad = FALSE) +
  scale_colour_manual(values = c("Low-Low" = "red", "Low-High" = "grey45", "High-High" = "blue")) +
  scale_linetype_manual(values = c("Low-Low" = "dashed", "Low-High" = "dotdash", "High-High" = "solid")) +
  scale_x_continuous(breaks = seq(0, 1, .2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2), expand = c(0, 0)) +
  coord_cartesian(xlim = c(if (review) 0 else .1, 1)) +
  labs(x = paste("Collective", title), y = "Cumulative probability", colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "inside", legend.position.inside = c(.02, .98),
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = "white", colour = "black"),
        panel.grid.minor = element_blank(), plot.margin = margin(12, 18, 12, 12),
        plot.background = element_rect(fill = "white", colour = NA))

if (review) {
  for (kind in c("bar", "cdf")) {
    plot <- if (kind == "bar") bar else cdf
    plot <- plot + theme(text = element_text(size = if (portrait) 16 else 14))
    ggsave(file.path(out, paste0("group_", outcome, "_", kind, ".pdf")), plot,
           width = if (portrait) 5 else 6, height = if (portrait) 4.5 else 3.5)
  }
}

png(file.path(out, paste0("group_", outcome, "_by_member_ccei_category.png")), width = 3600, height = 2100, res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 2, heights = unit(c(.10, .75, .06, .09), "npc"))))
grid.text(paste("Collective", title, "by members' individual CCEI category"), gp = gpar(fontsize = 20, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(bar, newpage = FALSE, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(cdf, newpage = FALSE, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
grid.text(paste("(a) Mean Collective", title), gp = gpar(fontsize = 15),
          vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
grid.text(paste("(b) CDF of Collective", title), gp = gpar(fontsize = 15),
          vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
grid.text(sprintf("High: individual CCEI above pooled median %.4f. N = 1,304 group-waves; Low–Low / Low–High / High–High: %s.\n95%% t confidence intervals; difference labels use Welch two-sample tests. + p < .10, * p < .05, ** p < .01.",
                  pooled_median, paste(stats$n, collapse = " / ")),
          gp = gpar(fontsize = 11, col = "grey30"),
          vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2))
popViewport()
dev.off()
print(stats)
print(diffs)
cat("Pooled individual CCEI median:", pooled_median, "\n")
