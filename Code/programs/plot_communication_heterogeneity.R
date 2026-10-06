# Run from Code after 99_28_communication_heterogeneity.do.
library(ggplot2)
library(readr)
out <- "results/new_indices/communication_heterogeneity"
labels <- list(wave = c("Baseline", "Endline"),
               friend = c("No nomination", "One-sided nomination", "Mutual nomination"),
               math_pair = c("Below wave median", "At/above wave median"),
               math_less = c("Below wave median", "At/above wave median"),
               friend_any = c("No nomination", "Any nomination"))
data <- lapply(names(labels), function(m) {
  d <- read_csv(file.path(out, paste0(m, "_coefficients.csv")), show_col_types = FALSE)
  subset(d, sample == "subgroup" & outcome != "ccei_ceiv_gap")
})
names(data) <- names(labels)
limits <- range(unlist(lapply(data, function(d) c(d$low, d$high))))
limits <- c(floor(limits[1]*10)/10, ceiling(limits[2]*10)/10)
for (m in names(labels)) {
  d <- data[[m]]
  d$stratum <- factor(d$group, levels = rev(seq_along(labels[[m]])-1))
  d$outcome <- factor(d$outcome, levels = c("ccei_g", "ceiv_g"), labels = c("Group CCEI", "Group CEIV"))
  d$member <- factor(d$member, levels = c("max", "min"))
  dodge <- position_dodge(width = .55)
  p <- ggplot(d, aes(estimate, stratum, colour = member)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = .4) +
    geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = .15,
                  linewidth = .5, position = dodge) +
    geom_point(position = dodge, size = 2.5) +
    facet_wrap(~outcome, nrow = 1) +
    scale_y_discrete(labels = setNames(labels[[m]], seq_along(labels[[m]])-1)) +
    scale_x_continuous(limits = limits, breaks = seq(ceiling(limits[1]*5)/5, limits[2], .2)) +
    scale_colour_manual(values = c(max = "blue", min = "red"),
                        labels = c(max = "Maximum individual CCEI", min = "Minimum individual CCEI")) +
    labs(x = "OLS coefficient on individual CCEI", y = NULL, colour = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          legend.position = "bottom", axis.text.y = element_text(colour = "black"),
          strip.text = element_text(face = "bold", size = 13), plot.margin = margin(6, 8, 4, 8))
  ggsave(file.path(out, paste0(m, "_coefficients.png")), p,
         width = 9.5, height = if (m == "friend") 2.15 else 2.5, dpi = 220)
}
