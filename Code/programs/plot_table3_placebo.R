# Shared display for Figure 4 (Columns 2/3) and Figure A8 (Columns 5/6).
# Both figures read the same 500-repetition, six-specification output from 07.
export_table3_placebo_panels <- function(code_dir, columns, review_filename) {
  figure_dir <- file.path(code_dir, "results/figures")
  draws <- read.csv(file.path(figure_dir, "figure4_placebo_coefficients.csv"))
  summary <- read.csv(file.path(figure_dir, "figure4_placebo_summary.csv"))
  # Reject the retained 2,512-row results when 09 is run before the updated 07.
  stopifnot(nrow(summary) == 6L, all(summary$N == 2560L),
            all(summary$clusters == 64L), all(summary$repetitions == 500L),
            all(draws$N == 2560L), all(draws$clusters == 64L),
            !anyDuplicated(draws[c("specification", "repetition")]))
  titles <- c(
    "2" = "Table 3 (2): Higher CCEI / controls + class FE",
    "3" = "Table 3 (3): Higher CCEI / controls + individual FE",
    "5" = "Table 3 (5): CCEI difference / controls + class FE",
    "6" = "Table 3 (6): CCEI difference / controls + individual FE")
  plots <- lapply(columns, function(column) {
    plot_data <- draws[draws$specification == column, ]
    actual <- summary[summary$specification == column, ]
    focal <- if (column <= 3L) "HighCCEI_both_high" else "ccei_gap_ij"
    stopifnot(nrow(plot_data) == 500L, nrow(actual) == 1L,
              actual$focal == focal, all(plot_data$focal == focal),
              is.finite(actual$actual_coefficient), all(is.finite(plot_data$coefficient)))
    actual_value <- actual$actual_coefficient
    ggplot2::ggplot(plot_data, ggplot2::aes(coefficient, after_stat(count / sum(count) * 100))) +
      ggplot2::geom_histogram(binwidth = if (column <= 3L) .005 else .015, boundary = 0,
        fill = "#BFDDEF", colour = "#4C83A6", linewidth = .35) +
      ggplot2::geom_vline(xintercept = actual_value, colour = "#B22222", linewidth = .9) +
      ggplot2::annotate("text", x = actual_value, y = Inf,
        label = sprintf("Actual = %.3f", actual_value), colour = "#B22222",
        hjust = -.08, vjust = 1.6, size = 3.7) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(.08, .15))) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .15))) +
      ggplot2::labs(title = titles[as.character(column)],
        x = if (column <= 3L) "Coefficient on Higher CCEI" else "Coefficient on CCEI Difference",
        y = "Percent") +
      ggplot2::theme_classic(base_size = 13) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12),
        plot.margin = ggplot2::margin(12, 18, 12, 12))
  })
  for (panel in seq_along(columns)) {
    panel_file <- file.path(figure_dir, sprintf("figure4_placebo_col%d.pdf", columns[panel]))
    ggplot2::ggsave(panel_file, plots[[panel]] + ggplot2::labs(title = NULL),
      width = 6, height = 4.5, device = grDevices::cairo_pdf)
    stopifnot(file.copy(panel_file,
      file.path(code_dir, "../Overleaf/figures_2025", basename(panel_file)), overwrite = TRUE))
  }
  grDevices::png(file.path(figure_dir, review_filename), width = 3600, height = 1350, res = 300)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
  for (panel in seq_along(columns)) {
    print(plots[[panel]], newpage = FALSE,
      vp = grid::viewport(layout.pos.row = 1, layout.pos.col = panel))
  }
  grDevices::dev.off()
  invisible(plots)
}
