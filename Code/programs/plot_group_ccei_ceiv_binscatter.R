# Run from Code; use the same pair-wave sample as Figure 6.
library(ggplot2)
library(haven)
library(grid)

script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
code_dir <- dirname(dirname(normalizePath(script_file)))
out <- file.path(code_dir, "results/new_indices/scatter")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
d <- read_dta(file.path(code_dir, "results/new_indices/analysis_sample.dta"))
variables <- c("ccei_min", "ccei_max", "ccei_g", "ceiv_g")
stopifnot(nrow(d) == 1304, !anyDuplicated(d[c("group_id", "post")]),
          all(is.finite(as.matrix(d[variables]))),
          all(as.matrix(d[variables]) >= 0), all(as.matrix(d[variables]) <= 1 + 1e-9),
          all(d$ccei_min <= d$ccei_max + 1e-9))

plots <- list()
bin_data <- list()
for (member in c("ccei_min", "ccei_max")) {
  breaks <- unique(quantile(d[[member]], probs = seq(0, 1, .05), names = FALSE))
  bin <- cut(d[[member]], breaks = breaks, include.lowest = TRUE, labels = FALSE)
  stopifnot(!anyNA(bin))
  means <- aggregate(d[c(member, "ccei_g", "ceiv_g")], list(bin = bin), mean)
  names(means)[2] <- "mean_individual_ccei"
  means$n <- as.integer(table(bin))
  means$member <- member
  stopifnot(sum(means$n) == nrow(d))
  bin_data[[member]] <- means
}
bins <- do.call(rbind, bin_data)
write.csv(bins, file.path(out, "group_ccei_ceiv_binscatter_data.csv"), row.names = FALSE)
x_lower <- floor(min(bins$mean_individual_ccei) / .1) * .1
y_lower <- floor(min(bins$ccei_g, bins$ceiv_g) / .05) * .05
for (outcome in c("ccei_g", "ceiv_g")) {
  for (member in c("ccei_min", "ccei_max")) {
    x_label <- if (member == "ccei_min") "Minimum individual CCEI" else "Maximum individual CCEI"
    y_label <- if (outcome == "ccei_g") "Group CCEI" else "Group CEIV"
    colour <- if (member == "ccei_min") "#D1495B" else "#2864B7"
    plots[[length(plots) + 1]] <- ggplot(bin_data[[member]],
      aes(x = mean_individual_ccei, y = .data[[outcome]])) +
      geom_point(colour = colour, size = 3, shape = 16) +
      scale_x_continuous(limits = c(x_lower, 1), breaks = seq(x_lower, 1, .1),
                         expand = expansion(add = .015)) +
      scale_y_continuous(limits = c(y_lower, 1), breaks = seq(y_lower, 1, .05),
                         expand = expansion(add = .005)) +
      labs(x = x_label, y = y_label) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_line(colour = "grey90", linewidth = .3),
            axis.text = element_text(colour = "grey25"),
            axis.title = element_text(colour = "black"),
            axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 10)),
            plot.margin = margin(15, 18, 10, 12),
            plot.background = element_rect(fill = "white", colour = NA))
  }
}

png(file.path(out, "group_ccei_ceiv_binscatter.png"), width = 3600, height = 3000, res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 2, heights = unit(c(.065, .45, .45, .035), "npc"))))
grid.text("Individual rationality and group outcomes: binscatter", y = .5,
          gp = gpar(fontsize = 21, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
for (i in seq_along(plots)) {
  print(plots[[i]], newpage = FALSE,
        vp = viewport(layout.pos.row = 2 + (i - 1) %/% 2,
                      layout.pos.col = 1 + (i - 1) %% 2))
}
grid.text(sprintf("1,304 group-waves; unadjusted bin means. 20 quantile bins requested; ties yield %d min bins and %d max bins.",
                  nrow(bin_data$ccei_min), nrow(bin_data$ccei_max)),
          gp = gpar(fontsize = 11, col = "grey35"),
          vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2))
popViewport()
dev.off()
print(bins)
cat(file.path(out, "group_ccei_ceiv_binscatter.png"), "\n")
