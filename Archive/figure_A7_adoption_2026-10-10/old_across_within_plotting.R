################################################################################
# Figure A7: across/within explained variation and within Shapley shares.
# Run the Figure A7 section of 08 first. Individual effects enter before the
# four covariate blocks; panel (b) conditions on those individual effects.
# Outputs stay in results/figures for review before updating the manuscript.
################################################################################

decomposition <- read.csv(file.path(result_dir, "tables/shapley_bargaining_index_M.csv"))
stopifnot(nrow(decomposition) == 6, all(decomposition$N == 2512))
overall <- subset(decomposition, panel == "overall")
within <- subset(decomposition, panel == "within")
stopifnot(abs(sum(overall$shapley_percent) - 100) < 1e-8,
          abs(sum(within$shapley_percent) - 100) < 1e-8,
          abs(sum(overall$shapley_value) - unique(decomposition$total_r2)) < 1e-10,
          abs(sum(within$shapley_value) - unique(decomposition$within_r2)) < 1e-10)
overall$block <- factor(overall$block,
  levels = c("Within individuals", "Across individuals"))
within$block <- factor(within$block,
  levels = c("Corner/Midpoint shares", "Individual/Friendship", "M benchmark", "Higher CCEI"))
overall$label <- ifelse(overall$block == "Across individuals",
  sprintf("%.1f%%\n(R² = %.3f)", overall$shapley_percent, overall$shapley_value),
  sprintf("%.1f%%\n(ΔR² = %.3f)", overall$shapley_percent, overall$shapley_value))
within$label <- sprintf("%.1f%%", within$shapley_percent)
shapley_theme <- theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.title = element_text(size = 14), plot.subtitle = element_text(size = 11),
        plot.margin = margin(10, 12, 8, 8))
across_within <- ggplot(overall, aes(shapley_percent, block)) +
  geom_col(fill = "#80ADD0", colour = "black", linewidth = .3, width = .55) +
  geom_text(aes(label = label), hjust = -.12, size = 3.8) +
  scale_x_continuous(limits = c(0, 105), breaks = seq(0, 100, 25),
                    expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(labels = c("Across individuals" = "Across\nindividuals",
                             "Within individuals" = "Within\nindividuals")) +
  labs(title = "(a) Across and within individuals",
       subtitle = sprintf("Overall R² = %.3f; N = 2,512", unique(overall$total_r2)),
       x = "Share of total explained variation (%)", y = NULL) + shapley_theme
within_shapley <- ggplot(within, aes(shapley_percent, block)) +
  geom_col(fill = "#80ADD0", colour = "black", linewidth = .3, width = .6) +
  geom_text(aes(label = label), hjust = -.15, size = 3.8) +
  scale_x_continuous(limits = c(0, 105), breaks = seq(0, 100, 25),
                    expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(labels = c("Higher CCEI" = "Higher CCEI",
    "M benchmark" = "Placebo\nbenchmark M",
    "Individual/Friendship" = "Individual and\nfriendship controls",
    "Corner/Midpoint shares" = "Corner and\nmidpoint shares")) +
  labs(title = "(b) Within-individual Shapley shares",
       subtitle = sprintf("Conditional within R² = %.3f", unique(within$within_r2)),
       x = "Share of within explained variation (%)", y = NULL) + shapley_theme
figure_dir <- file.path(result_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
png(file.path(figure_dir, "shapley_bargaining_index_M.png"),
    width = 3600, height = 1500, res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(across_within, newpage = FALSE, vp = viewport(layout.pos.col = 1))
print(within_shapley, newpage = FALSE, vp = viewport(layout.pos.col = 2))
dev.off()
message("Figure A7: across/within shares and four conditional Shapley blocks exported.")

