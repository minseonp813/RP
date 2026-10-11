# Run from Code after 12_1_collective_quality_extensions.do.
# Build Figure A11's alternative-index panels.
.libPaths(c(".R-library", .libPaths()))
library(ggplot2)
source("programs/plot_collective_ame.R")
out <- "results/collective_quality/extensions"
figure_dir <- "results/figures/collective_rationality"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
# Alternative-index appendix panels use Figure 8's LPM specifications and style.
a <- read.csv(file.path(out, "alternative_linear_ame.csv"))
stopifnot(nrow(a) == 48, setequal(a$measure, c("HM", "MaxMPI")))
sample <- haven::read_dta("data/collective_analysis_sample.dta")
indices <- list(HM = cbind(1 - sample$hm_i / 18, 1 - sample$hm_j / 18),
                MaxMPI = cbind(1 - sample$maxmpi_i, 1 - sample$maxmpi_j))
labels <- c(HM = "HM index", MaxMPI = "RevMaxMPI")
alternative_files <- character()
units <- data.frame()
for (measure in names(indices)) {
  scores <- indices[[measure]]
  member_sd <- c(maximum = sd(pmax(scores[, 1], scores[, 2])),
                 minimum = sd(pmin(scores[, 1], scores[, 2])))
  units <- rbind(units, data.frame(measure = measure, member = names(member_sd),
                                  sd = unname(member_sd), n = nrow(sample)))
  input <- tempfile(fileext = ".csv")
  write.csv(a[a$measure == measure, ], input, row.names = FALSE)
  alternative_files <- c(alternative_files,
    plot_cei_ame(input, figure_dir, classification = "a", portrait = TRUE,
                 prefix = paste0("alternative_", measure, "_lpm"),
                 member_sd = member_sd, consistency_label = labels[[measure]]))
  unlink(input)
}
write.csv(units, file.path(out, "alternative_sd_units.csv"), row.names = FALSE)
draft_dir <- "../Overleaf/figures_2025/collective_rationality"
stopifnot(file.copy(alternative_files, file.path(draft_dir, basename(alternative_files)), overwrite = TRUE))
