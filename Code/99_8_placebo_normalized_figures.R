rm(list = ls())

code_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
local_library <- file.path(code_dir, ".R-library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(haven)
  library(readr)
})

configs <- list(
  list(measure = "ccei", folder = "placebo_normalized", gap = "ccei_gap_ij",
       label = "CCEI difference", bin_width = 0.05),
  list(measure = "hm", folder = "placebo_normalized_hm", gap = "hm_gap_ij",
       label = "HM rationality gap", bin_width = 1),
  list(measure = "maxmpi", folder = "placebo_normalized_maxmpi", gap = "maxmpi_gap_ij",
       label = "RevMaxMPI difference", bin_width = 0.05)
)

summarise_outcome <- function(x, label) {
  q25 <- unname(quantile(x, 0.25, na.rm = TRUE))
  q75 <- unname(quantile(x, 0.75, na.rm = TRUE))
  tibble(variable = label, N = sum(!is.na(x)), mean = mean(x, na.rm = TRUE),
         sd = sd(x, na.rm = TRUE), q25 = q25, q75 = q75, IQR = q75 - q25,
         min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
}

for (cfg in configs) {
  input_file <- file.path(code_dir, "results", cfg$folder, "regressions",
                          "placebo_normalized_analysis.dta")
  if (!file.exists(input_file)) stop("Missing input file: ", input_file)
  output_dir <- file.path(code_dir, "results", "figures", cfg$folder)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  panel <- read_dta(input_file)
  analysis_sample <- panel |>
    filter(balanced_pn == 1, !is.na(.data[[cfg$gap]]),
           !is.na(I_actual), !is.na(Istar_all_imp))
  if (!nrow(analysis_sample)) stop("Empty analysis sample for ", cfg$measure)

  x <- analysis_sample[[cfg$gap]]
  outer <- ceiling(max(abs(x), na.rm = TRUE) / cfg$bin_width) * cfg$bin_width
  midpoints <- seq(-outer, outer, by = cfg$bin_width)
  breaks <- c(midpoints - cfg$bin_width / 2, tail(midpoints, 1) + cfg$bin_width / 2)
  reference <- tibble(bin = seq_along(midpoints), bin_midpoint = midpoints)
  bins <- analysis_sample |>
    mutate(bin = as.integer(cut(.data[[cfg$gap]], breaks = breaks,
                                include.lowest = TRUE, labels = FALSE))) |>
    group_by(bin) |>
    summarise(mean_gap = mean(.data[[cfg$gap]]),
              revealed_preference_distance = mean(I_actual), n = n(), .groups = "drop") |>
    right_join(reference, by = "bin") |>
    arrange(bin)

  plot <- ggplot(bins, aes(bin_midpoint, revealed_preference_distance)) +
    geom_point(size = 3, colour = "#2C6DA4") +
    geom_text(aes(label = n), vjust = -0.9, size = 3, colour = "#444444", na.rm = TRUE) +
    scale_x_continuous(breaks = bins$bin_midpoint,
                       labels = format(bins$bin_midpoint, trim = TRUE),
                       guide = guide_axis(angle = 90)) +
    scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +
    labs(x = cfg$label, y = expression(I[ig])) +
    theme_classic(base_size = 16) +
    theme(axis.title = element_text(size = 17), axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 7), plot.margin = margin(12, 18, 12, 12))

  stem <- paste0("binscatter_", cfg$measure, "_difference_Iig")
  ggsave(file.path(output_dir, paste0(stem, ".png")), plot,
         width = 13, height = 6.2, dpi = 300, bg = "white")
  write_csv(bins, file.path(output_dir, paste0(stem, "_data.csv")))

  summary <- bind_rows(
    summarise_outcome(analysis_sample$I_actual, paste0(cfg$measure, "-based I_ig")),
    summarise_outcome(analysis_sample$Istar_all_imp, paste0(cfg$measure, "-based I_ig - M_ig"))
  )
  write_csv(summary, file.path(output_dir, "outcome_summary.csv"))
  cat("Completed: ", cfg$measure, "\n", sep = "")
}
