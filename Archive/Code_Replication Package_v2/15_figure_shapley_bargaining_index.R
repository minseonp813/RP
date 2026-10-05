################################################################################
# Figure: Shapley decomposition of the bargaining-index regression
#
# Run the Table 3/Shapley block in 13_main_results.do first. It creates
# results/shapley_bargaining_index.csv.
################################################################################

library(ggplot2)
library(ggpattern)

input_file <- "results/shapley_bargaining_index.csv"
output_file <- "results/shapley_bargaining_index.png"

if (!file.exists(input_file)) {
  stop(
    paste0(
      input_file,
      " does not exist. Run the Table 3/Shapley block in ",
      "13_main_results.do first."
    )
  )
}

shapley <- read.csv(input_file, stringsAsFactors = FALSE)

required_columns <- c(
  "block", "shapley_value", "shapley_percent", "total_r2"
)
missing_columns <- setdiff(required_columns, names(shapley))
if (length(missing_columns) > 0) {
  stop(
    "The Shapley CSV is missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# This order is used both from top to bottom in the stack and in the legend.
block_order <- c(
  "CCEI",
  "Individual/Friendship",
  "Risk Aversion",
  "Corner/Midpoint Shares",
  "Individual FE"
)

shapley$block <- factor(shapley$block, levels = block_order)
shapley$label <- sprintf(
  "%.3f (%.1f%%)",
  shapley$shapley_value,
  shapley$shapley_percent
)

fill_values <- c(
  "CCEI" = "white",
  "Individual/Friendship" = "#B7D5E1",
  "Risk Aversion" = "#A9E891",
  "Corner/Midpoint Shares" = "#F2C6CF",
  "Individual FE" = "#C8C8C8"
)

pattern_values <- c(
  "CCEI" = "none",
  "Individual/Friendship" = "stripe",
  "Risk Aversion" = "stripe",
  "Corner/Midpoint Shares" = "stripe",
  "Individual FE" = "none"
)

pattern_angles <- c(
  "CCEI" = 0,
  "Individual/Friendship" = -45,
  "Risk Aversion" = 45,
  "Corner/Midpoint Shares" = 0,
  "Individual FE" = 0
)

plot_shapley <- ggplot(
  shapley,
  aes(
    x = "",
    y = shapley_value,
    fill = block,
    pattern = block,
    pattern_angle = block
  )
) +
  geom_col_pattern(
    width = 0.50,
    colour = "black",
    linewidth = 0.45,
    pattern_spacing = 0.045,
    pattern_density = 0.35,
    pattern_fill = "white",
    pattern_colour = "white"
  ) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 4.0
  ) +
  scale_fill_manual(
    name = "Block",
    values = fill_values,
    breaks = block_order,
    drop = FALSE
  ) +
  scale_pattern_manual(
    name = "Block",
    values = pattern_values,
    breaks = block_order,
    drop = FALSE
  ) +
  scale_pattern_angle_manual(values = pattern_angles, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = expression(R^2 ~ "Contribution")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(
  filename = output_file,
  plot = plot_shapley,
  width = 6,
  height = 5,
  dpi = 300,
  bg = "white"
)

message(
  "Saved ", output_file,
  " (total R-squared = ",
  sprintf("%.4f", unique(shapley$total_r2)),
  ")."
)
