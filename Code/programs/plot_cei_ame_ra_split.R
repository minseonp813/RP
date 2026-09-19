library(ggplot2)

# Run from Code: Rscript programs/plot_cei_ame_ra_split.R
output_dir <- file.path("..", "output", "visualizations")
d <- read.csv(file.path(output_dir, "cei-ame-ra-split.csv"))
stopifnot(nrow(d) == 16, !anyNA(d))
median_ra <- unique(d$median_ra)
stopifnot(length(median_ra) == 1)
sizes <- d[!duplicated(d$ra_high), c("ra_high", "n", "pairs", "clusters")]
sizes <- sizes[order(sizes$ra_high), ]
split_labels <- sprintf(c("RA difference at/below median (N = %s)",
                          "RA difference above median (N = %s)"), sizes$n)
d$split <- factor(d$ra_high, levels = 0:1, labels = split_labels)
d$member <- factor(d$member, levels = c("maximum", "minimum"),
                   labels = c("A. Higher-CCEI member", "B. Lower-CCEI member"))
d$y <- 5 - d$outcome + ifelse(d$ra_high == 0, .17, -.17)
for (v in c("estimate", "low", "high")) d[[v]] <- 100 * d[[v]]
d$value <- sprintf("%+.1f", d$estimate)
labels <- rev(c("Fails both\nCEI < 1, CCEI < 1", "CCEI only\nCEI < 1, CCEI = 1",
                "CEI only\nCEI = 1, CCEI < 1", "Passes both\nCEI = 1, CCEI = 1"))
limits <- range(c(d$low, d$high))
limits <- c(floor(limits[1] / 5) * 5, ceiling(limits[2] / 5) * 5)

p <- ggplot(d, aes(estimate, y, colour = split, shape = split)) +
  geom_hline(yintercept = 1:4, colour = "grey94", linewidth = .4) +
  geom_vline(xintercept = 0, colour = "grey45", linewidth = .5) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y",
                width = .10, linewidth = .8) +
  geom_point(size = 3.2) +
  geom_text(aes(label = value), nudge_y = .13, size = 3.7,
            colour = "grey15", show.legend = FALSE) +
  facet_wrap(~ member, nrow = 1) +
  scale_colour_manual(values = c("#287BA3", "#B86826")) +
  scale_shape_manual(values = c(16, 17)) +
  scale_x_continuous(limits = limits, breaks = scales::breaks_pretty(n = 6)) +
  scale_y_continuous(breaks = 1:4, labels = labels, limits = c(.5, 4.65)) +
  labs(title = "Individual rationality and collective outcomes, by RA difference",
       subtitle = "Average marginal effects scaled to a 0.1 increase in individual CCEI; 95% confidence intervals",
       x = "Average marginal effect (percentage points)", y = NULL, colour = NULL, shape = NULL,
       caption = sprintf(paste(
         "Split uses |RA_i - RA_j| in each pair-wave; pooled baseline/endline median = %.4f.",
         "Separate four-category multinomial logits; full controls and class fixed effects; RA controls excluded.",
         "Standard errors clustered by class. Effects are scaled average derivatives, not exact probability changes.", sep = "\n"), median_ra)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey91"),
        axis.text = element_text(colour = "grey15"), axis.text.y = element_text(size = 11),
        strip.text = element_text(size = 14, face = "bold", hjust = 0),
        plot.title = element_text(size = 17, face = "bold"),
        plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
        plot.caption = element_text(hjust = 0, size = 10, lineheight = 1.2, margin = margin(t = 15)),
        panel.spacing = grid::unit(1.1, "cm"), plot.margin = margin(16, 20, 14, 15))
ggsave(file.path(output_dir, "cei-ame-ra-split.png"), p,
       width = 13, height = 7.3, dpi = 180, bg = "white")
print(sizes)
