# Run from Code after analysis.do. Plots support only the Section 6 XXX passage.
library(ggplot2)
out <- "results/new_indices/section6_xxx"
d <- read.csv(file.path(out, "covariate_ame.csv"))
d <- d[!grepl("missing", d$term), ]
stopifnot(all(is.finite(as.matrix(d[c("estimate", "low", "high", "scale")]))),
          all(abs(tapply(d$estimate, d$term, sum)) < 1e-8))
labels <- c(mathscore_max = "Math score: maximum", mathscore_dist = "Math score: gap",
            height_max = "Height: maximum", height_dist = "Height: gap",
            male_diff = "Mixed gender", outgoing_max = "Extraversion: maximum",
            outgoing_dist = "Extraversion: gap", opened_max = "Openness: maximum",
            opened_dist = "Openness: gap", agreeable_max = "Agreeableness: maximum",
            agreeable_dist = "Agreeableness: gap", conscientious_max = "Conscientiousness: maximum",
            conscientious_dist = "Conscientiousness: gap", stable_max = "Emotional stability: maximum",
            stable_dist = "Emotional stability: gap", inclass_n_friends_max = "Out-degree: maximum",
            inclass_n_friends_dist = "Out-degree: gap", inclass_popularity_max = "In-degree: maximum",
            inclass_popularity_dist = "In-degree: gap", friend = "Friendship link",
            corner_share_max = "Corner share: maximum", corner_share_dist = "Corner share: gap",
            mid_share_max = "Midpoint share: maximum", mid_share_dist = "Midpoint share: gap")
d$term <- factor(d$term, levels = rev(names(labels)), labels = rev(labels))
outcomes <- c("CCEI < 1, CEIV < 1", "CCEI = 1, CEIV < 1",
              "CCEI < 1, CEIV = 1", "CCEI = 1, CEIV = 1")
d$outcome <- factor(d$outcome, levels = 1:4, labels = outcomes)
for (x in c("estimate", "low", "high")) d[[x]] <- 100*d[[x]]*d$scale
paper_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = NA))
p <- ggplot(d, aes(estimate, term)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = .3) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = .15) +
  geom_point(colour = "#1F4E79", size = 1.8) + facet_wrap(~outcome, ncol = 2) +
  labs(x = "Scaled average derivative (percentage points)", y = NULL,
       caption = "Continuous covariates: one SD; binary covariates: one unit. 95% class-clustered intervals.\nDerivatives hold other regressors fixed; these are not exact finite changes. Missing indicators omitted from display.") +
  paper_theme
ggsave(file.path(out, "covariate_ame.png"), p, width = 13, height = 10, dpi = 180)

focus <- c("Math score: maximum", "Math score: gap", "Out-degree: maximum", "Out-degree: gap",
           "In-degree: maximum", "In-degree: gap", "Friendship link")
focused <- d[as.character(d$term) %in% focus, ]
focused$term <- factor(as.character(focused$term), levels = rev(focus))
focused_plot <- ggplot(focused, aes(estimate, term)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = .3) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = .15) +
  geom_point(colour = "#1F4E79", size = 2.4) + facet_wrap(~outcome, ncol = 2) +
  labs(x = "Scaled average derivative (percentage points)", y = NULL,
       title = "Figure 8 review: math and friendship covariates",
       caption = "Continuous covariates: one SD; friendship indicator: one unit. 95% class-clustered intervals.\nAverage derivatives from the existing model; these are not exact finite changes.") +
  paper_theme + theme(axis.text.y = element_text(size = 12), strip.text = element_text(size = 12))
ggsave(file.path(out, "figure8_math_friendship_ame.png"), focused_plot, width = 13, height = 7.5, dpi = 240)

a <- read.csv(file.path(out, "alternative_ame.csv"))
stopifnot(nrow(a) == 16, all(is.finite(as.matrix(a[c("estimate", "low", "high")]))),
          all(abs(tapply(a$estimate, interaction(a$measure, a$member), sum)) < 1e-8))
for (x in c("estimate", "low", "high")) a[[x]] <- 100*a[[x]]
a$outcome <- factor(a$outcome, levels = 4:1,
                    labels = c("Consistency = 1\nCEIV = 1", "Consistency < 1\nCEIV = 1",
                               "Consistency = 1\nCEIV < 1", "Consistency < 1\nCEIV < 1"))
a$measure <- factor(a$measure, levels = c("HM", "MaxMPI"), labels = c("HM index", "RevMaxMPI"))
a$member <- factor(a$member, levels = c("maximum", "minimum"),
                   labels = c("Higher individual rationality", "Lower individual rationality"))
limits <- c(min(-20, floor(min(a$low)/10)*10), max(30, ceiling(max(a$high)/10)*10))
p <- ggplot(a, aes(estimate, outcome)) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = .35) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = .12,
                linewidth = .55, colour = "grey35") +
  geom_point(aes(colour = member), size = 2.8, show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red")) +
  scale_x_continuous(limits = limits, breaks = seq(limits[1], limits[2], 10)) +
  facet_grid(measure~member) + labs(x = "Average marginal effect (percentage points)", y = NULL) +
  paper_theme + theme(strip.text = element_text(size = 11))
ggsave(file.path(out, "alternative_joint_ame.png"), p, width = 11, height = 7, dpi = 240)
draft <- "../Overleaf/figures_2025/collective_rationality/alternative_joint_ame.png"
stopifnot(file.copy(file.path(out, "alternative_joint_ame.png"), draft, overwrite = TRUE))
