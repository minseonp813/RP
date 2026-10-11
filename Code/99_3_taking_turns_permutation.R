rm(list = ls())

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
code_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  normalizePath(getwd())
}
replication_dir <- code_dir
data_dir <- file.path(replication_dir, "data")
result_dir <- file.path(code_dir, "results", "taking_turns_permutation")
figure_dir <- file.path(code_dir, "results", "figures", "taking_turns")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
checkpoint_dir <- file.path(result_dir, "checkpoints")
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(parallel)

source(file.path(replication_dir, "programs", "warshall.R"))
source(file.path(replication_dir, "programs", "ex_cross.R"))

n_permutations <- as.integer(Sys.getenv("TAKING_TURNS_REPS", "500"))
base_seed <- 10001L
max_run_minutes <- as.numeric(Sys.getenv("TAKING_TURNS_MAX_MINUTES", "60"))
if (is.na(max_run_minutes) || max_run_minutes <= 0) {
  stop("TAKING_TURNS_MAX_MINUTES must be a positive number.")
}
run_started <- Sys.time()

panel <- read_dta(file.path(data_dir, "panel_individual_evenodd_R.dta")) %>%
  mutate(across(c(group_id, id), as.character),
         across(c(post, person), as.integer))

observed <- panel %>%
  drop_na(Ihat_ig, Ihat_ig_odd, Ihat_ig_even) %>%
  filter(Ihat_ig < .5) %>%
  transmute(
    group_id, post,
    default_lower = Ihat_ig,
    default_pair_gap = abs(1 - 2 * Ihat_ig),
    observed_gap = abs(Ihat_ig_odd - Ihat_ig_even)
  )

bottom_cutoff <- sort(observed$default_pair_gap)[ceiling(.25 * nrow(observed))]
observed <- observed %>%
  mutate(bottom25 = default_pair_gap <= bottom_cutoff)

base_raw <- read_dta(file.path(data_dir, "base_raw.dta"))
end_raw <- read_dta(file.path(data_dir, "end_raw.dta"))

raw <- bind_rows(
  mutate(base_raw, post = 0L),
  mutate(end_raw, post = 1L)
) %>%
  mutate(across(c(group_id, id), as.character),
         round_number = as.integer(round_number)) %>%
  filter(if_all(c(coord_x, coord_y, intercept_x, intercept_y), ~ !is.na(.x)),
         intercept_x != 0, intercept_y != 0)

individual <- raw %>%
  filter(round_number %in% 1:18) %>%
  inner_join(select(panel, group_id, post, id, person),
             by = c("group_id", "post", "id")) %>%
  mutate(role = person)

group <- raw %>%
  filter(round_number %in% 19:36, mover == 1) %>%
  mutate(round_number = round_number - 18L, role = 3L) %>%
  semi_join(distinct(panel, group_id, post), by = c("group_id", "post"))

cols <- c(
  "group_id", "post", "role", "round_number",
  "coord_x", "coord_y", "intercept_x", "intercept_y"
)
round_data <- bind_rows(select(individual, all_of(cols)),
                        select(group, all_of(cols))) %>%
  semi_join(select(observed, group_id, post), by = c("group_id", "post")) %>%
  arrange(group_id, post, role, round_number)

cases <- split(round_data, paste(round_data$group_id, round_data$post, sep = "_"))
stopifnot(all(vapply(cases, nrow, integer(1)) == 54L))

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

calculate_split <- function(task) {
  A <- calculate_ihat(task$data, task$A)
  B <- calculate_ihat(task$data, task$B)
  data.frame(
    group_id = task$data$group_id[1],
    post = task$data$post[1],
    gap = abs(A[1] - B[1])
  )
}

requested_workers <- as.integer(Sys.getenv("TAKING_TURNS_WORKERS", "4"))
if (is.na(requested_workers) || requested_workers < 1L) {
  stop("TAKING_TURNS_WORKERS must be a positive integer.")
}
workers <- min(requested_workers, max(1L, detectCores() - 1L))
if (workers == 1L) {
  cat("Running sequentially without a parallel cluster.\n")
  cl <- NULL
} else {
  cat("Using", workers, "parallel workers.\n")
  cl <- makeCluster(workers)
  clusterExport(
    cl,
    c(
      "calculate_split", "calculate_ihat", "compute_ex_from_subsets",
      "ex_cross", "cross_garp", "warshall", "ihat_from_ex"
    ),
    envir = environment()
  )
}

run_tasks <- function(tasks) {
  if (is.null(cl)) {
    lapply(tasks, calculate_split)
  } else {
    parLapply(cl, tasks, calculate_split)
  }
}

checkpoint_is_valid <- function(path, repetition, expected_rows) {
  if (!file.exists(path)) return(FALSE)
  x <- tryCatch(readRDS(path), error = function(e) NULL)
  !is.null(x) &&
    is.data.frame(x) &&
    nrow(x) == expected_rows &&
    all(c("group_id", "post", "gap", "permutation", "seed") %in% names(x)) &&
    all(x$permutation == repetition) &&
    all(x$seed == base_seed + repetition - 1L)
}

write_checkpoint <- function(x, path) {
  temporary_path <- paste0(path, ".tmp")
  saveRDS(x, temporary_path)
  if (is.null(tryCatch(readRDS(temporary_path), error = function(e) NULL))) {
    stop("Temporary checkpoint could not be read: ", temporary_path)
  }
  if (file.exists(path)) file.remove(path)
  if (!file.rename(temporary_path, path)) {
    stop("Could not finalize checkpoint: ", path)
  }
}

write_progress <- function() {
  repetitions <- seq_len(n_permutations)
  paths <- file.path(checkpoint_dir, sprintf("permutation_%03d.rds", repetitions))
  complete <- mapply(
    checkpoint_is_valid, paths, repetitions,
    MoreArgs = list(expected_rows = length(cases))
  )
  progress <- data.frame(
    repetition = repetitions,
    seed = base_seed + repetitions - 1L,
    complete = complete
  )
  temporary_path <- file.path(result_dir, "permutation_progress.csv.tmp")
  write.csv(progress, temporary_path, row.names = FALSE)
  final_path <- file.path(result_dir, "permutation_progress.csv")
  if (file.exists(final_path)) file.remove(final_path)
  file.rename(temporary_path, final_path)
  invisible(progress)
}

for (r in seq_len(n_permutations)) {
  checkpoint <- file.path(checkpoint_dir, sprintf("permutation_%03d.rds", r))
  if (checkpoint_is_valid(checkpoint, r, length(cases))) {
    cat(sprintf("Skipping completed permutation %d of %d\n", r, n_permutations))
    next
  }

  if (file.exists(checkpoint)) {
    invalid_path <- paste0(checkpoint, ".invalid")
    if (file.exists(invalid_path)) file.remove(invalid_path)
    file.rename(checkpoint, invalid_path)
    warning("Invalid checkpoint was preserved as: ", invalid_path)
  }

  repetition_seed <- base_seed + r - 1L
  set.seed(repetition_seed)
  tasks <- lapply(cases, function(d) {
    A <- sort(sample(1:18, 9, replace = FALSE))
    list(data = d, A = A, B = setdiff(1:18, A))
  })

  result <- run_tasks(tasks) %>%
    bind_rows() %>%
    mutate(permutation = r, seed = repetition_seed)
  write_checkpoint(result, checkpoint)
  write_progress()
  cat(
    sprintf("%s | completed permutation %d of %d | seed %d\n",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            r, n_permutations, repetition_seed),
    file = file.path(result_dir, "permutation_run.log"),
    append = TRUE
  )
  cat(sprintf("Finished permutation %d of %d (seed %d)\n",
              r, n_permutations, repetition_seed))

  elapsed_minutes <- as.numeric(difftime(Sys.time(), run_started, units = "mins"))
  if (elapsed_minutes >= max_run_minutes) {
    cat(sprintf(
      "Reached the %.0f-minute run limit after safely saving permutation %d.\n",
      max_run_minutes, r
    ))
    break
  }
}

progress <- write_progress()
if (!all(progress$complete)) {
  completed <- sum(progress$complete)
  stop(sprintf(
    "Scheduled pause: %d of %d permutations are complete. Run the same script later to continue.",
    completed, n_permutations
  ), call. = FALSE)
}

# First nine versus last nine.
first_last_file <- file.path(checkpoint_dir, "first_last.rds")
if (!file.exists(first_last_file)) {
  first_last_tasks <- lapply(cases, function(d) {
    list(data = d, A = 1:9, B = 10:18)
  })
  first_last <- run_tasks(first_last_tasks) %>%
    bind_rows() %>%
    rename(first_last_gap = gap)
  saveRDS(first_last, first_last_file)
}
if (!is.null(cl)) stopCluster(cl)

permutation_files <- file.path(
  checkpoint_dir,
  sprintf("permutation_%03d.rds", seq_len(n_permutations))
)
placebo <- lapply(permutation_files, readRDS) %>% bind_rows()
first_last <- readRDS(first_last_file)

pair_results <- placebo %>%
  group_by(group_id, post) %>%
  summarise(
    placebo_mean = mean(gap, na.rm = TRUE),
    placebo_median = median(gap, na.rm = TRUE),
    placebo_p95 = quantile(gap, .95, na.rm = TRUE, names = FALSE),
    valid_permutations = sum(!is.na(gap)),
    .groups = "drop"
  ) %>%
  left_join(observed, by = c("group_id", "post")) %>%
  left_join(first_last, by = c("group_id", "post")) %>%
  mutate(
    n_permutations = n_permutations,
    turn_like = observed_gap > placebo_p95,
    first_last_turn_like = first_last_gap > placebo_p95
  )

write.csv(pair_results, file.path(result_dir, "taking_turns_pairwave.csv"),
          row.names = FALSE)
write_dta(pair_results, file.path(result_dir, "taking_turns_pairwave.dta"))

grid <- seq(0, 1, by = .002)
ecdf_on_grid <- function(x) ecdf(x[!is.na(x)])(grid)

make_cdf_data <- function(bottom_only = FALSE) {
  keys <- observed %>% filter(!bottom_only | bottom25) %>% select(group_id, post)
  observed_values <- observed %>%
    semi_join(keys, by = c("group_id", "post")) %>% pull(observed_gap)
  first_last_values <- first_last %>%
    semi_join(keys, by = c("group_id", "post")) %>% pull(first_last_gap)

  placebo_ecdf_list <- placebo %>%
    semi_join(keys, by = c("group_id", "post")) %>%
    group_split(permutation) %>%
    lapply(function(d) ecdf_on_grid(d$gap))
  placebo_ecdfs <- do.call(rbind, placebo_ecdf_list)

  data.frame(
    gap = grid,
    observed = ecdf_on_grid(observed_values),
    first_last = ecdf_on_grid(first_last_values),
    placebo = colMeans(placebo_ecdfs),
    placebo_low = apply(placebo_ecdfs, 2, quantile, .025, na.rm = TRUE),
    placebo_high = apply(placebo_ecdfs, 2, quantile, .975, na.rm = TRUE),
    sample = if (bottom_only) "Bottom 25%" else "All pair-waves"
  )
}

cdf_data <- bind_rows(make_cdf_data(FALSE), make_cdf_data(TRUE))
write.csv(cdf_data, file.path(result_dir, "taking_turns_cdf_data.csv"),
          row.names = FALSE)

# Random-split KS tests.
ks_by_draw <- function(test_values, keys, split_label, sample_label) {
  test_values <- test_values[!is.na(test_values)]
  placebo %>%
    semi_join(keys, by = c("group_id", "post")) %>%
    filter(!is.na(gap)) %>%
    group_by(permutation) %>%
    summarise(
      ks_statistic = unname(suppressWarnings(
        ks.test(test_values, gap, exact = FALSE)$statistic
      )),
      p_value = suppressWarnings(
        ks.test(test_values, gap, exact = FALSE)$p.value
      ),
      .groups = "drop"
    ) %>%
    mutate(split = split_label, sample = sample_label)
}

all_keys <- select(observed, group_id, post)
bottom_keys <- observed %>% filter(bottom25) %>% select(group_id, post)

ks_results <- bind_rows(
  ks_by_draw(observed$observed_gap, all_keys,
             "Odd-even", "All pair-waves"),
  ks_by_draw(observed$observed_gap[observed$bottom25], bottom_keys,
             "Odd-even", "Bottom 25%"),
  ks_by_draw(first_last$first_last_gap, all_keys,
             "First 9 vs last 9", "All pair-waves"),
  ks_by_draw(
    first_last %>% semi_join(bottom_keys, by = c("group_id", "post")) %>%
      pull(first_last_gap),
    bottom_keys, "First 9 vs last 9", "Bottom 25%"
  )
)

write.csv(ks_results, file.path(result_dir, "ks_tests_by_draw.csv"),
          row.names = FALSE)

draw_ks_histogram <- function(d, title, filename) {
  p <- ggplot(d, aes(p_value)) +
    geom_histogram(binwidth = .05, boundary = 0,
                   fill = "#80ADD0", colour = "white") +
    geom_vline(xintercept = .05, linetype = "dashed", colour = "#C44E52") +
    facet_wrap(~ sample) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
    labs(
      title = title,
      subtitle = sprintf("KS tests across %d random 9/9 splits", n_permutations),
      x = "KS-test p-value",
      y = "Number of random splits"
    ) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank())
  ggsave(filename, p, width = 11, height = 6.5, dpi = 180)
}

draw_ks_histogram(
  filter(ks_results, split == "Odd-even"),
  "Distribution of KS-test p-values: observed odd-even vs placebo",
  file.path(figure_dir, "taking_turns_ks_pvalues.png")
)
draw_ks_histogram(
  filter(ks_results, split == "First 9 vs last 9"),
  "Distribution of KS-test p-values: first-last vs placebo",
  file.path(figure_dir, "taking_turns_first_last_ks_pvalues.png")
)

cdf_long <- cdf_data %>%
  pivot_longer(c(observed, placebo),
               names_to = "series", values_to = "cdf") %>%
  mutate(series = recode(
    series,
    observed = "Observed odd-even",
    placebo = "Mean placebo"
  ))

p_cdf <- ggplot(cdf_data, aes(gap)) +
  geom_ribbon(aes(ymin = placebo_low, ymax = placebo_high),
              fill = "grey75", alpha = .5) +
  geom_line(data = cdf_long, aes(y = cdf, colour = series,
                                 linetype = series), linewidth = .9) +
  facet_wrap(~ sample) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  scale_colour_manual(values = c(
    "Observed odd-even" = "#2F80ED",
    "Mean placebo" = "#F28E2B"
  )) +
  scale_linetype_manual(values = c(
    "Observed odd-even" = "solid",
    "Mean placebo" = "dashed"
  )) +
  labs(
    title = "Observed split gaps and random 9/9 benchmarks",
    subtitle = sprintf("Mean placebo CDF and pointwise 95%% range across %d permutations",
                       n_permutations),
    x = "Absolute split-index difference",
    y = "Cumulative share",
    colour = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(file.path(figure_dir, "taking_turns_cdf.png"), p_cdf,
       width = 12, height = 6.8, dpi = 180)

first_last_long <- cdf_data %>%
  pivot_longer(c(first_last, placebo),
               names_to = "series", values_to = "cdf") %>%
  mutate(series = recode(
    series,
    first_last = "First 9 vs last 9",
    placebo = "Mean placebo"
  ))

p_first_last <- ggplot(cdf_data, aes(gap)) +
  geom_ribbon(aes(ymin = placebo_low, ymax = placebo_high),
              fill = "grey75", alpha = .5) +
  geom_line(data = first_last_long,
            aes(y = cdf, colour = series, linetype = series), linewidth = .9) +
  facet_wrap(~ sample) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  scale_colour_manual(values = c("First 9 vs last 9" = "#3BAA66",
                                 "Mean placebo" = "#F28E2B")) +
  scale_linetype_manual(values = c("First 9 vs last 9" = "solid",
                                   "Mean placebo" = "dashed")) +
  labs(
    title = "First-half versus second-half split gaps",
    subtitle = sprintf("Mean placebo CDF and pointwise 95%% range across %d permutations",
                       n_permutations),
    x = "Absolute split-index difference", y = "Cumulative share",
    colour = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(file.path(figure_dir, "taking_turns_first_last_cdf.png"), p_first_last,
       width = 12, height = 6.8, dpi = 180)

permutation_stats <- placebo %>%
  left_join(select(observed, group_id, post, bottom25),
            by = c("group_id", "post")) %>%
  group_by(permutation) %>%
  summarise(
    mean_all = mean(gap, na.rm = TRUE),
    median_all = median(gap, na.rm = TRUE),
    mean_bottom25 = mean(gap[bottom25], na.rm = TRUE),
    median_bottom25 = median(gap[bottom25], na.rm = TRUE),
    .groups = "drop"
  )

observed_stats <- data.frame(
  n_permutations = n_permutations,
  statistic = c("Mean gap", "Median gap"),
  observed_all = c(mean(observed$observed_gap), median(observed$observed_gap)),
  placebo_all = c(mean(permutation_stats$mean_all),
                  mean(permutation_stats$median_all)),
  observed_bottom25 = c(
    mean(observed$observed_gap[observed$bottom25]),
    median(observed$observed_gap[observed$bottom25])
  ),
  placebo_bottom25 = c(
    mean(permutation_stats$mean_bottom25),
    mean(permutation_stats$median_bottom25)
  )
)

write.csv(permutation_stats,
          file.path(result_dir, "permutation_summary_by_draw.csv"), row.names = FALSE)
write.csv(observed_stats,
          file.path(result_dir, "taking_turns_summary.csv"), row.names = FALSE)

cat("Saved permutation results in:", result_dir, "\n")
print(observed_stats)
print(
  ks_results %>%
    group_by(split, sample) %>%
    summarise(median_p = median(p_value),
              share_below_05 = mean(p_value < .05), .groups = "drop")
)
cat("Turn-like pair-waves:", sum(pair_results$turn_like, na.rm = TRUE),
    "of", sum(!is.na(pair_results$turn_like)), "\n")
