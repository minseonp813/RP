# Shared Table 3 R2 panels (6) and optional review composite (99_10).
plot_table3_shapley <- function(code_dir, preview_file = NULL, export_panels = TRUE) {
  input_file <- file.path(code_dir, "results/figure_A7_review/table3_shapley.csv")
  panel_dir <- file.path(code_dir, "results/figures/figure_A7")
  decomposition <- read.csv(input_file)
  columns <- c(2, 3, 5, 6)
  stopifnot(nrow(decomposition) == 20, all(decomposition$N == 2560),
            all(decomposition$clusters == 64),
            all(decomposition$shapley_value >= -1e-10))
  # Top to bottom, matching the legend.
  blocks <- c("Rationality", "Individual/Friendship", "Corner/Midpoint shares",
              "M benchmark", "Fixed effects")
  fills <- c("#7FB2D8", adjustcolor("#D94B4B", alpha.f = .55),
             adjustcolor("#D94B4B", alpha.f = .8), "#E4E4E4", "#C8C8C8")
  titles <- c("Column (2): Higher CCEI / class FE",
              "Column (3): Higher CCEI / individual FE",
              "Column (5): CCEI difference / class FE",
              "Column (6): CCEI difference / individual FE")
  legend_labels <- c("Individual CCEI", "Individual / friendship",
                     "Corner / midpoint shares", "Placebo benchmark M", "Fixed effects")
  ymax <- ceiling(max(decomposition$total_r2) / .1) * .1
  draw_block <- function(x0, y0, x1, y1, block) {
    rect(x0, y0, x1, y1, col = fills[block], border = "#333333", lwd = 1,
         density = if (block == 3) 18 else NULL, angle = 45)
  }
  draw_panel <- function(panel, show_title = FALSE) {
    d <- subset(decomposition, column == columns[panel])
    d$block[d$block %in% c("Class FE", "Individual FE")] <- "Fixed effects"
    d <- d[match(rev(blocks), d$block), ]
    stopifnot(!anyNA(d$block), abs(sum(d$shapley_value) - d$total_r2[1]) < 1e-10,
              abs(sum(d$shapley_percent) - 100) < 1e-8)
    d$bottom <- c(0, head(cumsum(d$shapley_value), -1))
    d$top <- cumsum(d$shapley_value)
    d$middle <- (d$bottom + d$top) / 2
    values <- ifelse(d$shapley_value < .0005, "<0.001", sprintf("%.3f", d$shapley_value))
    percentages <- ifelse(d$shapley_percent < .05, "<0.1", sprintf("%.1f", d$shapley_percent))
    d$label <- sprintf("%s (%s%%)", values, percentages)
    par(mar = c(.6, 4.1, if (show_title) 2.5 else .6, .5), family = "sans")
    plot.new()
    plot.window(xlim = c(0, 1.65), ylim = c(0, ymax), xaxs = "i", yaxs = "i")
    ticks <- seq(0, ymax, .1)
    abline(h = ticks, col = "#ECECEC", lwd = 1)
    axis(2, at = ticks, labels = sprintf("%.1f", ticks), las = 1,
         col = NA, col.axis = "#555555", cex.axis = .9)
    mtext(expression(R^2 ~ "Contribution"), side = 2, line = 2.8, cex = .95)
    if (show_title) title(main = titles[panel], cex.main = 1, font.main = 1, line = .7)
    for (row in seq_len(nrow(d))) {
      draw_block(.32, d$bottom[row], .90, d$top[row], match(d$block[row], blocks))
    }
    inside <- d$shapley_value >= .05
    text(.61, d$middle[inside], d$label[inside], cex = .82)
    small <- which(!inside)
    label_y <- d$middle[small]
    label_y[1] <- max(label_y[1], .025)
    if (length(small) > 1) {
      for (k in 2:length(small)) label_y[k] <- max(label_y[k], label_y[k - 1] + .042)
      label_y <- label_y - max(0, max(label_y) - ymax + .025)
    }
    segments(.90, d$middle[small], 1.02, label_y, col = "#777777", lwd = .7)
    text(1.05, label_y, d$label[small], adj = 0, cex = .78)
  }
  draw_legend <- function() {
    par(mar = c(0, 0, 0, 0), family = "sans")
    plot.new()
    # Base legend fills columns first; this yields the requested row-wise order.
    order <- c(1, 4, 2, 5, 3)
    legend("center", legend = legend_labels[order], fill = fills[order],
           border = "#333333", density = c(NA, NA, 18, NA, NA)[order],
           angle = 45, bty = "n", ncol = 3, cex = .9)
  }
  files <- character()
  if (export_panels) {
    dir.create(panel_dir, recursive = TRUE, showWarnings = FALSE)
    for (panel in seq_along(columns)) {
      pdf(file.path(panel_dir, sprintf("table3_col%d.pdf", columns[panel])),
          width = 5, height = 3.6, pointsize = 14, useDingbats = FALSE)
      draw_panel(panel)
      dev.off()
    }
    pdf(file.path(panel_dir, "legend.pdf"), width = 10, height = .8,
        pointsize = 14, useDingbats = FALSE)
    draw_legend()
    dev.off()
    files <- file.path(panel_dir, c(sprintf("table3_col%d.pdf", columns), "legend.pdf"))
  }
  if (!is.null(preview_file)) {
    dir.create(dirname(preview_file), recursive = TRUE, showWarnings = FALSE)
    png(preview_file, width = 3300, height = 2550, res = 300, pointsize = 14)
    par(oma = c(0, 0, 2, 0))
    layout(matrix(c(1, 2, 3, 4, 5, 5), nrow = 3, byrow = TRUE),
           heights = c(1, 1, .18))
    for (panel in seq_along(columns)) draw_panel(panel, show_title = TRUE)
    draw_legend()
    mtext("Revealed-Preference Distance: Shorrocks–Shapley Decomposition",
          outer = TRUE, side = 3, line = .6, cex = 1.2)
    dev.off()
    files <- c(files, preview_file)
  }
  invisible(files)
}
