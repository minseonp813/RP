library(ggplot2)
d <- read.csv("IminusM_review/outputs/tables/shapley_results.csv")
d <- subset(d, outcome_model == "adjusted")
stopifnot(abs(sum(d$shapley_percent) - 100) < 1e-4)
d$block <- factor(d$block, levels = rev(d$block))
p <- ggplot(d, aes(shapley_percent, block)) +
  geom_col(fill = "steelblue", width = .65) +
  geom_text(aes(label = sprintf("%.1f%%", shapley_percent)), hjust = -.15, size = 4) +
  scale_x_continuous(limits = c(0, 75), expand = expansion(mult = c(0, .02))) +
  labs(x = "Share of explained variation (%)", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
ggsave("../Overleaf/figures_2025/shapley_bargaining_index_M.png", p, width=7, height=4.5, dpi=300)
