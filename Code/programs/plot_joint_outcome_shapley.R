# Shared joint-outcome R2 figure (6) and optional PNG preview (99_10).
plot_joint_outcome_shapley <- function(code_dir, preview_file = NULL, export_pdf = TRUE) {
  input_dir <- file.path(code_dir, "results/collective_quality/extensions")
  output_dir <- file.path(code_dir, "results/figures/collective_rationality")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  d <- read.csv(file.path(input_dir, "joint_outcome_shapley_r2.csv"))
  fits <- read.csv(file.path(input_dir, "joint_outcome_shapley_models.csv"))
  previous <- read.csv(file.path(code_dir, "results/collective_quality/joint_outcome_linear_ame.csv"))
  stopifnot(nrow(d) == 40, nrow(fits) == 8, all(d$n == 1304),
            all(d$pairs == 652), all(d$clusters == 64), all(d$shapley_value >= -1e-10))
  for (member in c("maximum", "minimum")) {
    check <- merge(fits, previous[previous$member == member, ], by = c("specification", "outcome"))
    stopifnot(nrow(check) == 8,
              all(abs(check[[paste0(member, "_beta")]] - check$estimate) < 1e-12),
              all(abs(check[[paste0(member, "_se")]] - check$se) < 5e-9))
  }

  # Borrow A7's exact overall-R2 decomposition, colours, hatching, and leader labels.
  blocks <- c("Individual CCEI", "Student characteristics", "Friendship measures",
              "Corner/Midpoint shares", "Fixed effects")
  fills <- c("#7FB2D8", adjustcolor("#D94B4B", alpha.f = .55), "#E4E4E4",
             adjustcolor("#D94B4B", alpha.f = .8), "#C8C8C8")
  outcome_labels <- c("CCEI < 1\nCEIV < 1", "CCEI = 1\nCEIV < 1",
                      "CCEI < 1\nCEIV = 1", "CCEI = 1\nCEIV = 1")
  specifications <- c("full_controls", "pair_fe")
  titles <- c("(a) Class FE + controls", "(b) Pair FE + controls")
  ymax <- ceiling(max(d$total_r2) / .1) * .1
  centres <- c(1.00, 3.55, 6.10, 8.65)

  draw_panel <- function(specification, title) {
    par(mar = c(3.6, 4.3, 2.3, .5), family = "sans")
    plot.new()
    plot.window(xlim = c(0, 10.4), ylim = c(0, ymax), xaxs = "i", yaxs = "i")
    ticks <- seq(0, ymax, .1)
    abline(h = ticks, col = "#ECECEC", lwd = 1)
    axis(2, at = ticks, labels = sprintf("%.1f", ticks), las = 1,
         col = NA, col.axis = "#555555", cex.axis = .95)
    axis(1, at = centres, labels = outcome_labels, tick = FALSE, line = .1,
         col.axis = "black", cex.axis = .95)
    mtext(expression(R^2 ~ "Contribution"), side = 2, line = 2.8, cex = .95)
    title(main = title, cex.main = 1.1, font.main = 1, line = .8)
    for (outcome in 1:4) {
      bar <- d[d$specification == specification & d$outcome == outcome, ]
      bar <- bar[match(rev(blocks), bar$block), ]
      stopifnot(!anyNA(bar$block), abs(sum(bar$shapley_value) - bar$total_r2[1]) < 1e-10,
                abs(sum(bar$shapley_percent) - 100) < 1e-8)
      bottom <- c(0, head(cumsum(bar$shapley_value), -1))
      top <- cumsum(bar$shapley_value)
      middle <- (bottom + top) / 2
      values <- ifelse(bar$shapley_value < .0005, "<0.001", sprintf("%.3f", bar$shapley_value))
      percentages <- ifelse(bar$shapley_percent < .05, "<0.1", sprintf("%.1f", bar$shapley_percent))
      labels <- sprintf("%s (%s%%)", values, percentages)
      centre <- centres[outcome]
      left <- centre - .55
      right <- centre + .55
      for (row in seq_len(nrow(bar))) {
        block <- match(bar$block[row], blocks)
        rect(left, bottom[row], right, top[row], col = fills[block], border = "#333333", lwd = 1,
             density = if (block == 4) 18 else NULL, angle = 45)
      }
      inside <- bar$shapley_value >= .035
      text(centre, middle[inside], labels[inside], cex = .78)
      small <- which(!inside)
      if (length(small)) {
        label_y <- middle[small]
        label_y[1] <- max(label_y[1], .025)
        if (length(small) > 1) {
          for (k in 2:length(small)) label_y[k] <- max(label_y[k], label_y[k - 1] + .029)
        }
        label_y <- label_y - max(0, max(label_y) - ymax + .028)
        segments(right, middle[small], right + .16, label_y, col = "#777777", lwd = .7)
        text(right + .20, label_y, labels[small], adj = 0, cex = .75)
      }
      text(centre, bar$total_r2[1] + .021, sprintf("R² = %.3f", bar$total_r2[1]), cex = .8)
    }
  }

  draw_legend <- function() {
    par(mar = c(0, 0, 0, 0), family = "sans")
    plot.new()
    order <- c(1, 4, 2, 5, 3)
    legend("center", legend = c("Individual CCEI", "Student characteristics", "Friendship measures",
                                "Corner / midpoint shares", "Fixed effects")[order],
           fill = fills[order], border = "#333333", density = c(NA, NA, NA, 18, NA)[order],
           angle = 45, bty = "n", ncol = 3, cex = .9)
  }

  draw_figure <- function(show_title = FALSE) {
    par(oma = c(0, 0, if (show_title) 1.8 else 0, 0))
    layout(matrix(c(1, 2, 3), ncol = 1), heights = c(1, 1, .17))
    for (panel in seq_along(specifications)) draw_panel(specifications[panel], titles[panel])
    draw_legend()
    if (show_title) mtext("Explained Variation in Joint Collective Outcomes", outer = TRUE,
                          side = 3, line = .3, cex = 1.2)
  }
  files <- character()
  if (!is.null(preview_file)) {
    dir.create(dirname(preview_file), recursive = TRUE, showWarnings = FALSE)
    png(preview_file, width = 3900, height = 2850, res = 300, pointsize = 14)
    draw_figure(show_title = TRUE)
    dev.off()
    files <- preview_file
  }
  if (export_pdf) {
    # LaTeX supplies the overall caption.
    manuscript_file <- file.path(output_dir, "joint_outcome_shapley.pdf")
    pdf(manuscript_file, width = 13, height = 9.5, pointsize = 20, useDingbats = FALSE)
    draw_figure()
    dev.off()
    files <- c(files, manuscript_file)
  }
  invisible(files)
}
