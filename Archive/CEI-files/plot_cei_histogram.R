rm(list = ls())

suppressPackageStartupMessages(library(ggplot2))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
code_dir <- if (length(file_arg) == 1) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else {
  normalizePath(getwd())
}

result_dir <- file.path(code_dir, "results")
data <- read.csv(file.path(result_dir, "cei_pairwave.csv"))
data <- data[data$spec == "tempered", ]

plot_data <- rbind(
  data.frame(index = "Group CCEI", value = data$ccei_g),
  data.frame(index = "CEI", value = data$e_coll)
)
plot_data$index <- factor(plot_data$index, levels = c("Group CCEI", "CEI"))
plot_data$weight <- ave(
  plot_data$value,
  plot_data$index,
  FUN = function(x) rep(100 / length(x), length(x))
)

exact_one <- 100 * c(
  "Group CCEI" = mean(data$ccei_g >= 1 - 1e-9),
  "CEI" = mean(data$e_coll >= 1 - 1e-9)
)

histogram <- ggplot(
  plot_data,
  aes(x = value, weight = weight, fill = index)
) +
  geom_histogram(
    binwidth = 0.05,
    boundary = 0,
    closed = "right",
    colour = "white",
    linewidth = 0.4
  ) +
  facet_wrap(~ index, nrow = 1) +
  scale_fill_manual(values = c("Group CCEI" = "#74A9CF", "CEI" = "#E39695")) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Pooled distributions of group CCEI and CEI",
    subtitle = "Baseline and endline pooled; N = 1,304 pair-wave observations",
    x = "Efficiency index",
    y = "Percent of pair-wave observations",
    fill = NULL,
    caption = sprintf(
      "Bin width = 0.05. Exact-one share: Group CCEI %.1f%%; CEI %.1f%%",
      exact_one["Group CCEI"], exact_one["CEI"]
    )
  ) +
  guides(fill = "none") +
  theme_classic(base_size = 13) +
  theme(
    axis.text = element_text(colour = "black"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.caption = element_text(hjust = 0),
    panel.spacing.x = grid::unit(1.2, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(result_dir, "hist_group_ccei_cei_pooled.png"),
  histogram,
  width = 9,
  height = 4.8,
  dpi = 400
)
ggsave(
  file.path(result_dir, "hist_group_ccei_cei_pooled.pdf"),
  histogram,
  width = 9,
  height = 4.8
)
