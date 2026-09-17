rm(list = ls())

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
code_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  normalizePath(getwd())
}
replication_dir <- file.path(code_dir, "..", "Code_Replication Package_Upload")
data_dir <- file.path(replication_dir, "data")
result_dir <- file.path(code_dir, "results", "oddeven_placebo_R")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(parallel)
library(cluster)

source(file.path(replication_dir, "programs", "warshall.R"))
source(file.path(replication_dir, "programs", "ex_cross.R"))

set.seed(20260824)

panel <- read_dta(file.path(data_dir, "panel_individual_evenodd_R.dta"))
base_raw <- read_dta(file.path(data_dir, "base_raw.dta"))
end_raw <- read_dta(file.path(data_dir, "end_raw.dta"))

panel <- panel %>%
  mutate(across(c(group_id, id), as.character),
         across(c(post, person), as.integer))

observed <- panel %>%
  drop_na(Ihat_ig, Ihat_ig_odd, Ihat_ig_even) %>%
  filter(Ihat_ig < 0.5) %>%
  mutate(default_lower = Ihat_ig,
         default_pair_gap = abs(1 - 2 * Ihat_ig),
         split_gap = abs(Ihat_ig_odd - Ihat_ig_even))
stopifnot(nrow(observed) == 1201L)

ordered_gap <- sort(observed$default_pair_gap)
default_gap_cutoff <- ordered_gap[ceiling(0.25 * length(ordered_gap))]
observed <- observed %>%
  mutate(bottom25 = default_pair_gap <= default_gap_cutoff)
stopifnot(sum(observed$bottom25) == 301L)

base_raw$post <- 0L
end_raw$post <- 1L
raw <- rbind(base_raw, end_raw)
raw$group_id <- as.character(raw$group_id)
raw$id <- as.character(raw$id)
raw$round_number <- as.integer(raw$round_number)

keep <- complete.cases(raw[, c("coord_x", "coord_y", "intercept_x", "intercept_y")]) &
  raw$intercept_x != 0 & raw$intercept_y != 0
raw <- raw[keep, ]

individual <- raw[raw$round_number %in% 1:18, ]
individual <- merge(
  individual,
  panel[, c("group_id", "post", "id", "person")],
  by = c("group_id", "post", "id"),
  all = FALSE
)
individual$role <- individual$person

group <- raw[raw$round_number %in% 19:36 & raw$mover == 1, ]
group$round_number <- group$round_number - 18L
group$role <- 3L
group <- merge(
  group,
  unique(panel[, c("group_id", "post")]),
  by = c("group_id", "post"),
  all = FALSE
)

cols <- c(
  "group_id", "post", "role", "round_number",
  "coord_x", "coord_y", "intercept_x", "intercept_y"
)
round_data <- rbind(individual[, cols], group[, cols])
round_data <- round_data[order(
  round_data$group_id, round_data$post,
  round_data$role, round_data$round_number
), ]

case_id <- paste(round_data$group_id, round_data$post, sep = "_")
cases <- split(round_data, case_id)
stopifnot(all(vapply(cases, nrow, integer(1)) == 54L))

random_halves <- lapply(cases, function(d) {
  half_A <- sort(sample(1:18, 9, replace = FALSE))
  list(A = half_A, B = setdiff(1:18, half_A))
})

calculate_ihat <- function(d, group_rounds) {
  i <- d[d$role == 1, ]
  j <- d[d$role == 2, ]
  g <- d[d$role == 3 & d$round_number %in% group_rounds, ]

  ex_i <- compute_ex_from_subsets(i, g)
  ex_j <- compute_ex_from_subsets(j, g)
  ex_ij <- compute_ex_from_subsets(rbind(i, j), g)

  c(
    member1 = ihat_from_ex(ex_i, ex_j, ex_ij),
    member2 = ihat_from_ex(ex_j, ex_i, ex_ij)
  )
}

calculate_placebo <- function(task) {
  d <- task$data
  A <- calculate_ihat(d, task$halves$A)
  B <- calculate_ihat(d, task$halves$B)

  data.frame(
    group_id = d$group_id[1],
    post = d$post[1],
    Ihat_A1 = A[1],
    Ihat_A2 = A[2],
    Ihat_B1 = B[1],
    Ihat_B2 = B[2]
  )
}

tasks <- Map(function(data, halves) list(data = data, halves = halves), cases, random_halves)
workers <- min(4L, max(1L, detectCores() - 1L))
cl <- makeCluster(workers)
clusterExport(
  cl,
  c(
    "calculate_placebo", "calculate_ihat", "compute_ex_from_subsets",
    "ex_cross", "cross_garp", "warshall", "ihat_from_ex"
  ),
  envir = environment()
)
placebo_list <- parLapply(cl, tasks, calculate_placebo)
stopCluster(cl)

placebo_wide <- do.call(rbind, placebo_list)
row.names(placebo_wide) <- NULL
placebo_long <- rbind(
  data.frame(
    group_id = placebo_wide$group_id,
    post = placebo_wide$post,
    person = 1L,
    Ihat_A = placebo_wide$Ihat_A1,
    Ihat_B = placebo_wide$Ihat_B1
  ),
  data.frame(
    group_id = placebo_wide$group_id,
    post = placebo_wide$post,
    person = 2L,
    Ihat_A = placebo_wide$Ihat_A2,
    Ihat_B = placebo_wide$Ihat_B2
  )
)

placebo <- merge(
  panel[, c("group_id", "post", "person", "id", "mover", "Ihat_ig")],
  placebo_long,
  by = c("group_id", "post", "person"),
  all = FALSE
)
placebo <- placebo[
  complete.cases(placebo[, c("Ihat_ig", "Ihat_A", "Ihat_B")]) &
    placebo$Ihat_ig < 0.5,
]
placebo$default_lower <- placebo$Ihat_ig
placebo$default_pair_gap <- abs(1 - 2 * placebo$Ihat_ig)
placebo$split_gap <- abs(placebo$Ihat_A - placebo$Ihat_B)
placebo$bottom25 <- placebo$default_pair_gap <= default_gap_cutoff

draw_histogram <- function(d, title, filename) {
  plot_data <- bind_rows(
    transmute(d, split_gap, sample = sprintf("All selected pair-waves (n = %d)", nrow(d))),
    d %>% filter(bottom25) %>%
      transmute(split_gap,
                sample = sprintf("Bottom 25%% default gap (n = %d)", n()))
  )

  p <- ggplot(plot_data, aes(split_gap,
                             after_stat(count / ave(count, group, FUN = sum)),
                             colour = sample, fill = sample)) +
    geom_histogram(binwidth = .05, boundary = 0, position = "identity",
                   alpha = .18, linewidth = .9) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_colour_manual(values = c("#2F80ED", "#F28E2B")) +
    scale_fill_manual(values = c("#2F80ED", NA)) +
    labs(
      title = title,
      subtitle = sprintf("Bottom quartile: within-pair default Ihat gap <= %.3f",
                         default_gap_cutoff),
      x = "Absolute split index difference",
      y = "Within-sample share",
      colour = NULL, fill = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())

  ggsave(filename, p, width = 12, height = 7.2, dpi = 180)
}

run_kmeans <- function(d, title, filename, prefix) {
  z <- scale(d[, c("default_lower", "split_gap")])
  set.seed(20260824)
  fit <- kmeans(z, centers = 4, nstart = 50, iter.max = 100)

  centers <- aggregate(
    d[, c("default_lower", "split_gap")],
    list(cluster_raw = fit$cluster),
    mean
  )
  low_x <- centers$cluster_raw[order(centers$default_lower)[1:2]]
  high_x <- setdiff(centers$cluster_raw, low_x)
  map <- integer(4)
  map[low_x[order(centers$split_gap[match(low_x, centers$cluster_raw)])]] <- c(1L, 2L)
  map[high_x[order(centers$split_gap[match(high_x, centers$cluster_raw)])]] <- c(3L, 4L)
  d$cluster <- map[fit$cluster]

  labels <- c(
    "C1 closer / stable", "C2 closer / separated",
    "C3 less close / stable", "C4 less close / separated"
  )
  colors <- c("#2F6BFF", "#F28E2B", "#42B96B", "#E66AA3")
  counts <- tabulate(d$cluster, nbins = 4)
  sil <- mean(silhouette(d$cluster, dist(z))[, "sil_width"])

  d <- d %>%
    mutate(cluster_label = factor(
      cluster, levels = 1:4,
      labels = sprintf("%s (n = %d)", labels, counts)
    ))

  p <- ggplot(d, aes(default_lower, split_gap, colour = cluster_label)) +
    geom_point(alpha = 1, size = 2) +
    scale_colour_manual(values = colors) +
    scale_x_continuous(limits = c(0, .5)) +
    scale_y_continuous(limits = c(0, 1)) +
    coord_fixed(ratio = 1) +
    labs(
      title = title,
      subtitle = sprintf(
        "Standardized default Ihat and split difference; N = %d; silhouette = %.3f",
        nrow(d), sil
      ),
      x = "Default Ihat of lower-distance member",
      y = "Absolute split index difference",
      colour = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 11)
    ) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))

  ggsave(filename, p, width = 9.5, height = 12, dpi = 180)

  write_dta(d, paste0(prefix, ".dta"))
  write.csv(d, paste0(prefix, ".csv"), row.names = FALSE)
  write.csv(
    data.frame(
      cluster = 1:4,
      label = labels,
      n = counts,
      default_center = tapply(d$default_lower, d$cluster, mean),
      gap_center = tapply(d$split_gap, d$cluster, mean)
    ),
    paste0(prefix, "_centroids.csv"),
    row.names = FALSE
  )
  invisible(list(data = d, silhouette = sil))
}

draw_histogram(
  observed,
  "Observed odd-even distance gap by default-index similarity",
  file.path(result_dir, "observed_histogram.png")
)
observed_fit <- run_kmeans(
  observed,
  "Observed odd-even split: K=4 clustering",
  file.path(result_dir, "observed_kmeans_k4.png"),
  file.path(result_dir, "observed_kmeans_k4")
)

draw_histogram(
  placebo,
  "Placebo random 9/9 distance gap by default-index similarity",
  file.path(result_dir, "placebo_histogram.png")
)
placebo_fit <- run_kmeans(
  placebo,
  "Placebo random 9/9 split: K=4 clustering",
  file.path(result_dir, "placebo_kmeans_k4.png"),
  file.path(result_dir, "placebo_kmeans_k4")
)

summary <- data.frame(
  sample = c("Observed odd-even", "Placebo random 9/9"),
  n = c(nrow(observed), nrow(placebo)),
  gap_mean = c(mean(observed$split_gap), mean(placebo$split_gap)),
  gap_median = c(median(observed$split_gap), median(placebo$split_gap)),
  bottom25_n = c(sum(observed$bottom25), sum(placebo$bottom25)),
  bottom25_gap_mean = c(
    mean(observed$split_gap[observed$bottom25]),
    mean(placebo$split_gap[placebo$bottom25])
  ),
  silhouette = c(observed_fit$silhouette, placebo_fit$silhouette)
)
write.csv(summary, file.path(result_dir, "observed_placebo_summary.csv"), row.names = FALSE)
write_dta(placebo_wide, file.path(result_dir, "placebo_Ihat_pair_wide.dta"))

cat("Saved results in:", result_dir, "\n")
print(summary)
