# Run from Code after 99_31_preference_distance_ccei_slopes.do.
library(ggplot2)
out <- "results/new_indices/preference_distance_slopes"
d <- read.csv(file.path(out, "ccei_coefficients.csv"))
stopifnot(nrow(d) == 16, all(is.finite(as.matrix(d[c("estimate", "low", "high")]))))
labels <- c("CCEI < 1\nCEIV < 1", "CCEI = 1\nCEIV < 1", "CCEI < 1\nCEIV = 1", "CCEI = 1\nCEIV = 1")
d$member <- factor(d$member, levels = c("max", "min"), labels = c("Maximum CCEI", "Minimum CCEI"))
d$y <- 5-d$joint_category+ifelse(d$member == "Maximum CCEI", .12, -.12)
limits <- c(min(0, floor(min(d$low)/.05)*.05), max(0, ceiling(max(d$high)/.05)*.05))
for (specification in c("unadjusted", "controlled")) {
  sample <- d[d$specification == specification, ]
  p <- ggplot(sample, aes(estimate, y, colour = member)) +
    geom_vline(xintercept = 0, colour = "grey55", linewidth = .4) +
    geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = .1, linewidth = .6) +
    geom_point(size = 3) +
    scale_colour_manual(values = c("blue", "red")) +
    scale_y_continuous(breaks = 4:1, labels = labels, limits = c(.6, 4.4)) +
    scale_x_continuous(limits = limits, breaks = seq(limits[1], limits[2], .1)) +
    labs(x = "Coefficient per 0.1 increase in CCEI", y = NULL, colour = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          axis.text.y = element_text(colour = "black"), legend.position = "bottom",
          legend.text = element_text(size = 10),
          plot.background = element_rect(fill = "white", colour = NA))
  ggsave(file.path(out, paste0(specification, "_coefficients.png")), p, width = 5, height = 3.5, dpi = 300)
}
