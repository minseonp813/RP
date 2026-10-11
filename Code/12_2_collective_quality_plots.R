# Run from Code after 12_1_collective_quality_extensions.do.
# Build the alternative-index appendix figure and retained covariate review plots.
.libPaths(c(".R-library", .libPaths()))
library(ggplot2)
source("programs/plot_collective_ame.R")
out <- "results/collective_quality/extensions"
figure_dir <- "results/figures/collective_rationality"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
d <- read.csv(file.path(out, "covariate_ame.csv"))
d <- d[!grepl("missing", d$term), ]
stopifnot(all(is.finite(as.matrix(d[c("estimate", "low", "high", "scale")]))),
          all(abs(tapply(d$estimate, d$term, sum)) < 1e-8))
labels <- c(mathscore_max = "Math score: maximum", mathscore_dist = "Math score: gap",
            height_max = "Height: maximum", height_dist = "Height: gap",
            male_diff = "Mixed gender", outgoing_max = "Extraversion: maximum",
            outgoing_dist = "Extraversion: gap", opened_max = "Openness: maximum",
            opened_dist = "Openness: gap", agreeable_max = "Agreeableness: maximum",
            agreeable_dist = "Agreeableness: gap", conscientious_max = "Conscientiousness: maximum",
            conscientious_dist = "Conscientiousness: gap", stable_max = "Emotional stability: maximum",
            stable_dist = "Emotional stability: gap", inclass_n_friends_max = "Out-degree: maximum",
            inclass_n_friends_dist = "Out-degree: gap", inclass_popularity_max = "In-degree: maximum",
            inclass_popularity_dist = "In-degree: gap", friend = "Friendship link",
            corner_share_max = "Corner share: maximum", corner_share_dist = "Corner share: gap",
            mid_share_max = "Midpoint share: maximum", mid_share_dist = "Midpoint share: gap")
d$term <- factor(d$term, levels = rev(names(labels)), labels = rev(labels))
outcomes <- c("CCEI < 1, CEIV < 1", "CCEI = 1, CEIV < 1",
              "CCEI < 1, CEIV = 1", "CCEI = 1, CEIV = 1")
d$outcome <- factor(d$outcome, levels = 1:4, labels = outcomes)
for (x in c("estimate", "low", "high")) d[[x]] <- 100*d[[x]]*d$scale
paper_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = NA))
p <- ggplot(d, aes(estimate, term)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = .3) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = .15) +
  geom_point(colour = "#1F4E79", size = 1.8) + facet_wrap(~outcome, ncol = 2) +
  labs(x = "Scaled average derivative (percentage points)", y = NULL,
       caption = "Continuous covariates: one SD; binary covariates: one unit. 95% class-clustered intervals.\nDerivatives hold other regressors fixed; these are not exact finite changes. Missing indicators omitted from display.") +
  paper_theme
ggsave(file.path(figure_dir, "covariate_ame.png"), p, width = 13, height = 10, dpi = 180)

focus <- c("Math score: maximum", "Math score: gap", "Out-degree: maximum", "Out-degree: gap",
           "In-degree: maximum", "In-degree: gap", "Friendship link")
focused <- d[as.character(d$term) %in% focus, ]
focused$term <- factor(as.character(focused$term), levels = rev(focus))
focused_plot <- ggplot(focused, aes(estimate, term)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = .3) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = .15) +
  geom_point(colour = "#1F4E79", size = 2.4) + facet_wrap(~outcome, ncol = 2) +
  labs(x = "Scaled average derivative (percentage points)", y = NULL,
       title = "Figure 8 review: math and friendship covariates",
       caption = "Continuous covariates: one SD; friendship indicator: one unit. 95% class-clustered intervals.\nAverage derivatives from the existing model; these are not exact finite changes.") +
  paper_theme + theme(axis.text.y = element_text(size = 12), strip.text = element_text(size = 12))
ggsave(file.path(figure_dir, "figure8_math_friendship_ame.png"), focused_plot, width = 13, height = 7.5, dpi = 240)

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
