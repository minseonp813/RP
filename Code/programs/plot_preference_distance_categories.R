# Run from Code after 99_30_preference_distance_categories.do.
library(ggplot2)
out <- "results/new_indices/preference_distance_categories"
d <- read.csv(file.path(out, "distance_means.csv"))
stopifnot(nrow(d) == 8, all(is.finite(as.matrix(d[c("mean", "low", "high")]))))
d$category <- factor(d$joint_category, levels = 1:4)
d$member <- factor(d$role, levels = 1:2, labels = c("More rational member", "Less rational member"))
n <- d$pairwaves[d$role == 1]
labels <- paste0(c("CCEI < 1\nCEIV < 1", "CCEI = 1\nCEIV < 1",
                   "CCEI < 1\nCEIV = 1", "CCEI = 1\nCEIV = 1"), "\nN = ", n)
dodge <- position_dodge(width = .4)
p <- ggplot(d, aes(category, mean, colour = member)) +
  geom_hline(yintercept = .5, linetype = "dashed", linewidth = .4, colour = "grey55") +
  geom_errorbar(aes(ymin = low, ymax = high), position = dodge, width = .1, linewidth = .6) +
  geom_point(position = dodge, size = 3) +
  geom_text(aes(y = high + .045, label = sprintf("%.3f", mean)), position = dodge, size = 3.5) +
  scale_colour_manual(values = c("blue", "red")) +
  scale_x_discrete(labels = labels) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .25)) +
  labs(x = NULL, y = "Mean revealed-preference\ndistance to group", colour = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.x = element_text(colour = "black"), legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = NA))
ggsave(file.path(out, "distance_by_role.png"), p, width = 9, height = 3.6, dpi = 300)
