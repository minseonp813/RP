# Shared style for the main and appendix collective-outcome figures.
plot_cei_ame <- function(input, output_dir, classification = "original", portrait = FALSE,
                         prefix = NULL, member_sd = NULL, consistency_label = "CCEI") {
  stopifnot(classification %in% c("original", "a", "b"))
  n_categories <- if (classification == "b") 3L else 4L
  if (is.null(prefix)) prefix <- if (classification == "original") "cei" else paste0("figure6_", classification)
  d <- read.csv(input)
  if (!"specification" %in% names(d)) d$specification <- "full_controls"
  specifications <- c("class_fe", "full_controls", "pair_fe")
  stopifnot(all(d$specification %in% specifications))
  d$specification <- factor(d$specification, levels = specifications)
  n_specifications <- length(unique(d$specification))
  split_sample <- "ra_high" %in% names(d)
  communication_split <- all(c("moderator", "group") %in% names(d))
  split_fields <- if (communication_split) c("moderator", "group") else if (split_sample) "ra_high" else character()
  panels <- if (length(split_fields)) unique(d[split_fields]) else data.frame(panel = 1)
  keys <- c(split_fields, "specification", "outcome", "member")
  sum_groups <- do.call(interaction, c(d[c(split_fields, "specification", "member")], list(drop = TRUE)))
  stopifnot(nrow(d) == 2 * n_categories * nrow(panels) * n_specifications, !anyDuplicated(d[keys]),
            all(is.finite(as.matrix(d[c("estimate", "low", "high")]))),
            all(abs(tapply(d$estimate, sum_groups, sum)) < 1e-8))
  x_label <- "Average marginal effect (percentage points)"
  if (!is.null(member_sd)) {
    stopifnot(all(c("maximum", "minimum") %in% names(member_sd)),
              all(is.finite(member_sd)), all(member_sd > 0))
    # Stored effects are per 0.1 units; use each member's pooled-sample SD.
    d[c("estimate", "low", "high")] <- d[c("estimate", "low", "high")] *
      unname(member_sd[d$member]) / 0.1
    x_label <- "Effect of a one-SD increase (percentage points)"
  }
  d[c("estimate", "low", "high")] <- 100 * d[c("estimate", "low", "high")]
  d$outcome <- factor(d$outcome, levels = n_categories:1)
  outcome_labels <- c("1" = "CCEI < 1\nCEI < 1", "2" = "CCEI = 1\nCEI < 1",
                      "3" = "CCEI < 1\nCEI = 1", "4" = "CCEI = 1\nCEI = 1")

  if (classification == "a") {
    outcome_labels <- c("1" = "CCEI < 1\nCEIV < 1", "2" = "CCEI = 1\nCEIV < 1",
                        "3" = "CCEI < 1\nCEIV = 1", "4" = "CCEI = 1\nCEIV = 1")
  }
  if (classification == "b") {
    outcome_labels <- c("1" = "CEIV < 1\nCEIC < 1", "2" = "CEIV = 1\nCEIC < 1",
                        "3" = "CEIV = 1\nCEIC = 1")
  }
  outcome_labels <- sub("CCEI", consistency_label, outcome_labels, fixed = TRUE)
  # Use a common axis across members, specifications, and split samples.
  limits <- c(-20, 30)
  if (length(split_fields) || n_specifications > 1) limits <- c(min(-20, floor(min(d$low) / 10) * 10),
                               max(30, ceiling(max(d$high) / 10) * 10))
  stopifnot(all(d$low >= limits[1]), all(d$high <= limits[2]))

  # Match 07_Figures_Main.R: base size 14, white background, blue/red members,
  # Titles, panel captions, and notes belong in LaTeX.
  paper_theme <- theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.background = element_rect(fill = "white", colour = NA),
          axis.text.y = element_text(colour = "black"),
          axis.title.x = element_text(margin = margin(t = 8)),
          plot.margin = margin(8, 12, 6, 6))
  member_colours <- c(maximum = "blue", minimum = "red")
  # Reverse the dodge so the legend order also runs from top to bottom.
  dodge <- position_dodge(width = if (n_specifications == 3) .6 else if (n_specifications == 2) .35 else 0,
                          orientation = "y", reverse = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_files <- character()
  for (panel in seq_len(nrow(panels))) {
    keep <- rep(TRUE, nrow(d))
    for (field in split_fields) keep <- keep & d[[field]] == panels[[field]][panel]
    sample <- d[keep, ]
    sample_prefix <- if (communication_split) paste(prefix, panels$moderator[panel], panels$group[panel], sep = "_") else
      if (split_sample) paste0(prefix, "_", c("similar", "different")[panels$ra_high[panel] + 1]) else prefix
    for (member in names(member_colours)) {
      p <- ggplot(sample[sample$member == member, ], aes(estimate, outcome, group = specification)) +
        geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.35) +
        geom_errorbar(aes(xmin = low, xmax = high), orientation = "y",
                      width = 0.12, linewidth = 0.55, colour = "grey35", position = dodge) +
        geom_point(aes(shape = specification), size = 3, colour = member_colours[member],
                   fill = "white", stroke = .65, position = dodge) +
        scale_shape_manual(name = NULL, values = c(class_fe = 17, full_controls = 21, pair_fe = 15),
                           labels = c(class_fe = "Class FE", full_controls = "Class FE + controls",
                                      pair_fe = "Pair FE + controls"),
                           guide = if (n_specifications > 1) guide_legend(nrow = 1) else "none") +
        scale_x_continuous(limits = limits, breaks = seq(limits[1], limits[2], 10)) +
        scale_y_discrete(labels = outcome_labels, expand = expansion(add = 0.55)) +
        labs(x = x_label, y = NULL) +
        paper_theme + theme(legend.position = "bottom", legend.location = "plot",
                            legend.text = element_text(size = 12))
      output_file <- file.path(output_dir, paste0(sample_prefix, "_ame_", member, ".png"))
      ggsave(output_file, p,
             width = if (portrait) 5.5 else 6,
             height = if (portrait) 3 + .5 * (n_specifications > 1) else 5, dpi = 300)
      output_files <- c(output_files, output_file)
    }
  }
  invisible(output_files)
}
