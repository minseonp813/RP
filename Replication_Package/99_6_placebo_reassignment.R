# Placebo reassignment.

rm(list = ls())
local({

# Settings.
n_repetitions <- as.integer(Sys.getenv("PLACEBO_REPS", "500"))
base_output_subdir <- Sys.getenv("PLACEBO_OUTPUT_SUBDIR", "placebo_reassignment")
save_intermediates <- Sys.getenv("PLACEBO_SAVE_INTERMEDIATES", "1") == "1"
master_seed <- 20260812L
requested_definition <- Sys.getenv("HIGH_DEFINITION", "all")
definitions_to_run <- if (requested_definition == "all") {
  c("both_high", "both_low", "ccei_diff")
} else {
  requested_definition
}

run_definition <- function(high_definition) {

definition_map <- c(both_high = "HighCCEI_both_high",
                    both_low = "HighCCEI_both_low",
                    ccei_diff = "ccei_gap_ij")
suffix_map <- c(both_high = "", both_low = "_bothlow", ccei_diff = "_cceidiff")
if (!high_definition %in% names(definition_map)) stop("Unknown HIGH_DEFINITION.")
treatment_var <- unname(definition_map[high_definition])
output_subdir <- paste0(base_output_subdir, unname(suffix_map[high_definition]))

if (is.na(n_repetitions) || n_repetitions < 1L) {
  stop("PLACEBO_REPS must be a positive integer.")
}

code_dir_setting <- Sys.getenv("RP_CODE_DIR", "")
if (nzchar(code_dir_setting)) {
  code_dir <- normalizePath(code_dir_setting, winslash = "/", mustWork = TRUE)
} else {
  command_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(command_file) > 0L) {
    script_path <- sub("^--file=", "", command_file[1L])
    code_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
  } else if (file.exists(file.path(getwd(), "99_6_placebo_reassignment.R"))) {
    code_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  } else {
    code_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }
}
local_library <- file.path(code_dir, ".R-library")
if (dir.exists(local_library)) {
  .libPaths(c(local_library, .libPaths()))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(haven)
  library(readr)
  library(vroom)
})

data_dir <- file.path(code_dir, "data")
donor_file <- file.path(
  code_dir,
  "results",
  "placebo_normalized",
  "placebo_donor_matrix.csv"
)
output_dir <- file.path(code_dir, "results", output_subdir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
table_dir <- file.path(code_dir, "results", "tables", output_subdir)
figure_dir <- file.path(code_dir, "results", "figures", output_subdir)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(
  file.path(data_dir, "panel_individual.dta"),
  file.path(data_dir, "base_raw.dta"),
  file.path(data_dir, "end_raw.dta"),
  donor_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required input(s):\n", paste(missing_files, collapse = "\n"))
}

# Functions.

as_plain_character <- function(x) {
  if (is.numeric(x)) sprintf("%.0f", x) else as.character(x)
}

within_transform <- function(z, fixed_effect) {
  z <- as.matrix(z)
  storage.mode(z) <- "double"
  group <- factor(fixed_effect, exclude = NULL)
  counts <- as.numeric(tabulate(as.integer(group), nbins = nlevels(group)))
  sums <- rowsum(z, group, reorder = TRUE)
  means <- sums / counts
  z - means[as.integer(group), , drop = FALSE]
}

fit_absorbed <- function(data, outcome, rhs, fixed_effect, cluster = "class") {
  needed <- unique(c(outcome, rhs, fixed_effect, cluster))
  sample_ok <- complete.cases(data[, needed, drop = FALSE])
  d <- data[sample_ok, needed, drop = FALSE]

  repeat {
    fe_count <- table(d[[fixed_effect]])
    keep <- as.character(d[[fixed_effect]]) %in% names(fe_count)[fe_count > 1L]
    if (all(keep)) break
    d <- d[keep, , drop = FALSE]
    if (nrow(d) == 0L) stop("No observations remain after singleton removal.")
  }

  y <- within_transform(d[[outcome]], d[[fixed_effect]])[, 1L]
  x <- within_transform(d[, rhs, drop = FALSE], d[[fixed_effect]])

  nonzero <- colSums(x^2) > 1e-20
  if (!nonzero[1L]) stop("The HighCCEI variable is absorbed in this sample.")
  x <- x[, nonzero, drop = FALSE]
  fitted <- lm.fit(x = x, y = y)
  residual <- fitted$residuals
  rank <- fitted$rank
  n <- nrow(x)

  pivot <- fitted$qr$pivot[seq_len(rank)]
  x_rank <- x[, pivot, drop = FALSE]
  beta_rank <- fitted$coefficients[pivot]
  bread <- chol2inv(qr.R(qr(x_rank)))
  scores <- x_rank * residual
  cluster_id <- factor(d[[cluster]])
  score_by_cluster <- rowsum(scores, cluster_id, reorder = FALSE)
  meat <- crossprod(score_by_cluster)
  g <- nlevels(cluster_id)
  correction <- if (g > 1L && n > rank) {
    (g / (g - 1)) * ((n - 1) / (n - rank))
  } else {
    1
  }
  vcov_cluster <- correction * bread %*% meat %*% bread
  se_rank <- sqrt(pmax(diag(vcov_cluster), 0))

  first_position <- match(1L, pivot)
  list(
    coefficient = unname(beta_rank[first_position]),
    se = unname(se_rank[first_position]),
    N = n,
    r_squared_within = 1 - sum(residual^2) / sum(y^2)
  )
}

estimate_four_specs <- function(data, outcome) {
  p_group <- c(
    "mathscore_i", "mathscore_diff", "height_i", "height_diff",
    "outgoing_i", "outgoing_diff", "opened_i", "opened_diff",
    "agreeable_i", "agreeable_diff", "conscientious_i",
    "conscientious_diff", "stable_i", "stable_diff",
    "female_i_male_j", "male_i_female_j"
  )
  p_group_nogender <- setdiff(
    p_group,
    c("female_i_male_j", "male_i_female_j")
  )
  p_friend <- c(
    "inclass_n_friends_i", "inclass_n_diff",
    "inclass_popularity_i", "inclass_pop_diff"
  )
  p_missing <- c(
    "mathscore_diff_missing", "outgoing_diff_missing",
    "opened_diff_missing", "agreeable_diff_missing",
    "conscientious_diff_missing", "stable_diff_missing"
  )
  p_ra <- c("RA_i", "RA_diff")
  p_share <- c(
    "corner_share_i", "corner_share_diff",
    "mid_share_i", "mid_share_diff"
  )

  rhs <- list(
    treatment_var,
    c(treatment_var, p_group, p_friend, p_missing),
    c(treatment_var, p_group, p_friend, p_missing, p_ra, p_share),
    c(
      treatment_var, p_group_nogender, p_friend,
      p_missing, p_ra, p_share
    )
  )
  fixed_effect <- c("class", "class", "class", "id")

  result <- lapply(seq_len(4L), function(s) {
    fit <- fit_absorbed(
      data = data,
      outcome = outcome,
      rhs = rhs[[s]],
      fixed_effect = fixed_effect[s],
      cluster = "class"
    )
    data.frame(
      specification = s,
      coefficient = fit$coefficient,
      se = fit$se,
      N = fit$N,
      r_squared_within = fit$r_squared_within
    )
  })
  bind_rows(result)
}

make_choice_shares <- function(path, post_value) {
  read_dta(path) |>
    filter(game_type == 1) |>
    mutate(
      post = post_value,
      corner_share = as.numeric(coord_x == 0 | coord_y == 0),
      mid_share = as.numeric(coord_x == coord_y)
    ) |>
    group_by(id, post) |>
    summarise(
      corner_share = mean(corner_share, na.rm = TRUE),
      mid_share = mean(mid_share, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(id = as_plain_character(id))
}

# Prepare controls.

cat("[1/5] Reading panel and recreating Table 3 controls...\n")

panel <- read_dta(file.path(data_dir, "panel_individual.dta")) |>
  mutate(
    across(c(group_id, id, partner_id, class), as_plain_character),
    post = as.integer(post)
  )

choice_shares <- bind_rows(
  make_choice_shares(file.path(data_dir, "base_raw.dta"), 0L),
  make_choice_shares(file.path(data_dir, "end_raw.dta"), 1L)
)

own_shares <- choice_shares |>
  rename(
    corner_share_i = corner_share,
    mid_share_i = mid_share
  )
partner_shares <- choice_shares |>
  rename(
    partner_id = id,
    corner_share_j = corner_share,
    mid_share_j = mid_share
  )

panel <- panel |>
  left_join(own_shares, by = c("id", "post")) |>
  left_join(partner_shares, by = c("partner_id", "post")) |>
  mutate(
    corner_share_diff = corner_share_i - corner_share_j,
    mid_share_diff = mid_share_i - mid_share_j,
    female_i_male_j = as.numeric(male_i == 0 & male_j == 1),
    male_i_female_j = as.numeric(male_i == 1 & male_j == 0)
  ) |>
  group_by(id) |>
  mutate(balanced_actual = sum(!is.na(Ihat_ig)) == 2L) |>
  ungroup()

stopifnot(all(panel$HighCCEI_both_high[panel$ccei_i == panel$ccei_j] == 1, na.rm = TRUE),
          all(panel$HighCCEI_both_low[panel$ccei_i == panel$ccei_j] == 0, na.rm = TRUE))

actual_sample <- panel |> filter(balanced_actual)
actual_results <- estimate_four_specs(actual_sample, "Ihat_ig") |>
  rename(
    actual_coefficient = coefficient,
    actual_se = se,
    actual_N = N,
    actual_r_squared_within = r_squared_within
  )

write_csv(actual_results, file.path(output_dir, "actual_table3_coefficients.csv"))
write_dta(actual_results, file.path(output_dir, "actual_table3_coefficients.dta"))
cat(
  "      Actual coefficients verified: ",
  paste(sprintf("%.3f", actual_results$actual_coefficient), collapse = ", "),
  "\n",
  sep = ""
)

# Read donor matrix.

cat("[2/5] Reading the precomputed target-pair x donor-group matrix...\n")

donor_matrix <- vroom(
  donor_file,
  col_types = cols(
    target_group_id = col_character(),
    post = col_integer(),
    target_class = col_character(),
    member1_id = col_character(),
    member2_id = col_character(),
    donor_group_id = col_character(),
    donor_class = col_character(),
    is_own = col_integer(),
    same_class = col_integer(),
    cost1 = col_double(),
    cost2 = col_double(),
    cost12 = col_double(),
    Ihat1_donor = col_double(),
    Ihat2_donor = col_double(),
    degenerate = col_integer()
  ),
  progress = TRUE,
  altrep = FALSE
) |>
  as.data.frame()

donor_matrix$lookup_key <- paste(
  donor_matrix$target_group_id,
  donor_matrix$post,
  donor_matrix$donor_group_id,
  sep = "|"
)
if (anyDuplicated(donor_matrix$lookup_key)) {
  stop("The donor matrix lookup key is not unique.")
}
lookup_row <- setNames(seq_len(nrow(donor_matrix)), donor_matrix$lookup_key)

pair_roster <- donor_matrix |>
  distinct(
    target_group_id, post, target_class,
    member1_id, member2_id
  ) |>
  arrange(target_class, post, target_group_id)

if (nrow(pair_roster) != 1304L) {
  stop("Expected 1,304 pair-wave targets, found ", nrow(pair_roster), ".")
}

diagonal <- donor_matrix |>
  filter(is_own == 1L) |>
  select(
    target_group_id, post, member1_id, member2_id,
    Ihat1_donor, Ihat2_donor
  )
diagonal <- bind_rows(
  diagonal |>
    transmute(
      group_id = target_group_id,
      post,
      id = member1_id,
      Ihat_diagonal = Ihat1_donor
    ),
  diagonal |>
    transmute(
      group_id = target_group_id,
      post,
      id = member2_id,
      Ihat_diagonal = Ihat2_donor
    )
)

diagonal_check <- panel |>
  select(group_id, post, id, Ihat_ig) |>
  inner_join(diagonal, by = c("group_id", "post", "id"))
diagonal_tolerance <- 1e-4
if (
  any(
    abs(diagonal_check$Ihat_ig - diagonal_check$Ihat_diagonal) > diagonal_tolerance,
    na.rm = TRUE
  ) ||
    any(is.na(diagonal_check$Ihat_ig) != is.na(diagonal_check$Ihat_diagonal))
) {
  stop("The diagonal donor indices do not reproduce panel_individual.dta.")
}

# Run placebo assignments.

cat(
  "[3/5] Running ", n_repetitions,
  " randomized placebo assignments...\n",
  sep = ""
)

set.seed(master_seed)
coefficient_results <- vector("list", n_repetitions)
pair_results <- if (save_intermediates) vector("list", n_repetitions) else NULL
member_results <- if (save_intermediates) vector("list", n_repetitions) else NULL

strata <- split(
  seq_len(nrow(pair_roster)),
  interaction(pair_roster$target_class, pair_roster$post, drop = TRUE)
)
if (any(lengths(strata) < 2L)) {
  stop("At least one class-wave stratum has fewer than two pairs.")
}

start_time <- Sys.time()
for (r in seq_len(n_repetitions)) {
  donor_assignment <- character(nrow(pair_roster))

  for (indices in strata) {
    randomized <- sample(indices, length(indices), replace = FALSE)
    donor_indices <- c(randomized[-1L], randomized[1L])
    donor_assignment[randomized] <- pair_roster$target_group_id[donor_indices]
  }

  if (any(donor_assignment == pair_roster$target_group_id)) {
    stop("A placebo assignment matched a pair to itself in repetition ", r, ".")
  }

  keys <- paste(
    pair_roster$target_group_id,
    pair_roster$post,
    donor_assignment,
    sep = "|"
  )
  rows <- unname(lookup_row[keys])
  if (anyNA(rows)) {
    stop("A donor-matrix lookup failed in repetition ", r, ".")
  }

  pair_rep <- data.frame(
    repetition = r,
    group_id = pair_roster$target_group_id,
    class = pair_roster$target_class,
    post = pair_roster$post,
    member1_id = pair_roster$member1_id,
    member2_id = pair_roster$member2_id,
    matched_group_id = donor_assignment,
    cost1 = donor_matrix$cost1[rows],
    cost2 = donor_matrix$cost2[rows],
    cost12 = donor_matrix$cost12[rows],
    Ihat1_placebo = donor_matrix$Ihat1_donor[rows],
    Ihat2_placebo = donor_matrix$Ihat2_donor[rows],
    degenerate = donor_matrix$degenerate[rows]
  )

  member_rep <- bind_rows(
    pair_rep |>
      transmute(
        repetition, group_id, class, post,
        id = member1_id,
        matched_group_id,
        Ihat_placebo = Ihat1_placebo,
        cost_i = cost1,
        cost_j = cost2,
        cost_Ng = cost12,
        degenerate
      ),
    pair_rep |>
      transmute(
        repetition, group_id, class, post,
        id = member2_id,
        matched_group_id,
        Ihat_placebo = Ihat2_placebo,
        cost_i = cost2,
        cost_j = cost1,
        cost_Ng = cost12,
        degenerate
      )
  ) |>
    left_join(panel, by = c("group_id", "class", "post", "id")) |>
    group_by(id) |>
    mutate(balanced_placebo = sum(!is.na(Ihat_placebo)) == 2L) |>
    ungroup()

  regression_sample <- member_rep |> filter(balanced_placebo)
  rep_coefficients <- estimate_four_specs(regression_sample, "Ihat_placebo") |>
    transmute(
      repetition = r,
      specification,
      placebo_coefficient = coefficient,
      placebo_se = se,
      N,
      r_squared_within
    )
  coefficient_results[[r]] <- rep_coefficients

  if (save_intermediates) {
    pair_results[[r]] <- pair_rep
    member_results[[r]] <- member_rep |>
      select(
        repetition, group_id, class, post, id, partner_id,
        matched_group_id, ccei_i, ccei_j, all_of(treatment_var),
        Ihat_placebo, cost_i, cost_j, cost_Ng, degenerate,
        balanced_placebo
      )
  }

  if (r == 1L || r %% 25L == 0L || r == n_repetitions) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    projected <- elapsed / r * n_repetitions
    cat(
      sprintf(
        "      repetition %d/%d | elapsed %.1f sec | projected %.1f sec\n",
        r, n_repetitions, elapsed, projected
      )
    )
  }
}

placebo_coefficients <- bind_rows(coefficient_results)
placebo_coefficients_wide <- Reduce(
  function(x, y) merge(x, y, by = "repetition", sort = TRUE),
  lapply(seq_len(4L), function(s) {
    z <- placebo_coefficients[placebo_coefficients$specification == s, ]
    z <- z[, c("repetition", "placebo_coefficient", "placebo_se", "N")]
    names(z)[-1L] <- c(paste0("b", s), paste0("se", s), paste0("N", s))
    z
  })
)
write_csv(
  placebo_coefficients_wide,
  file.path(output_dir, paste0("placebo_repetitions_", n_repetitions, ".csv"))
)
write_dta(
  placebo_coefficients_wide,
  file.path(output_dir, paste0("placebo_repetitions_", n_repetitions, ".dta"))
)
saveRDS(
  placebo_coefficients_wide,
  file.path(output_dir, paste0("placebo_repetitions_", n_repetitions, ".rds")),
  compress = "xz"
)
write_csv(
  placebo_coefficients,
  file.path(
    output_dir,
    paste0("placebo_repetitions_", n_repetitions, "_long.csv")
  )
)
write_dta(
  placebo_coefficients,
  file.path(
    output_dir,
    paste0("placebo_repetitions_", n_repetitions, "_long.dta")
  )
)

if (save_intermediates) {
  cat("[4/5] Saving complete pair- and member-level intermediate results...\n")
  all_pairs <- bind_rows(pair_results)
  all_members <- bind_rows(member_results)

  saveRDS(
    all_pairs,
    file.path(output_dir, "placebo_pair_indices_all_repetitions.rds"),
    compress = "xz"
  )
  saveRDS(
    all_members,
    file.path(output_dir, "placebo_member_outcomes_all_repetitions.rds"),
    compress = "xz"
  )
  write_dta(
    all_pairs,
    file.path(output_dir, "placebo_pair_indices_all_repetitions.dta")
  )
  write_dta(
    all_members,
    file.path(output_dir, "placebo_member_outcomes_all_repetitions.dta")
  )
  rm(all_pairs, all_members, pair_results, member_results)
  invisible(gc())
} else {
  cat("[4/5] Intermediate result saving was disabled.\n")
}

# Save results.

cat("[5/5] Creating summary files and figures...\n")

placebo_summary <- placebo_coefficients |>
  group_by(specification) |>
  summarise(
    placebo_median = median(placebo_coefficient),
    placebo_lower = unname(quantile(placebo_coefficient, 0.025)),
    placebo_upper = unname(quantile(placebo_coefficient, 0.975)),
    .groups = "drop"
  ) |>
  left_join(actual_results, by = "specification") |>
  select(
    specification, actual_coefficient, actual_se, actual_N,
    placebo_median, placebo_lower, placebo_upper
  )

write_csv(placebo_summary, file.path(output_dir, "placebo_final_summary.csv"))
write_dta(placebo_summary, file.path(output_dir, "placebo_final_summary.dta"))

tex_lines <- c(
  "& Actual coefficient & Placebo median & 95\\% placebo interval \\\\",
  "\\midrule",
  vapply(seq_len(nrow(placebo_summary)), function(i) {
    sprintf(
      "Specification (%d) & %.3f & %.3f & [%.3f, %.3f] \\\\",
      placebo_summary$specification[i],
      placebo_summary$actual_coefficient[i],
      placebo_summary$placebo_median[i],
      placebo_summary$placebo_lower[i],
      placebo_summary$placebo_upper[i]
    )
  }, character(1L)),
  "\\bottomrule"
)
writeLines(tex_lines, file.path(table_dir, "table_placebo_final_summary.tex"))

for (s in seq_len(4L)) {
  plot_data <- placebo_coefficients |>
    filter(specification == s)
  actual_value <- placebo_summary$actual_coefficient[
    placebo_summary$specification == s
  ]

  if (high_definition == "ccei_diff") {
    x_lower <- -0.85
    x_upper <- -0.40
    x_breaks <- seq(x_lower, x_upper, by = 0.05)
    x_label <- "Coefficient on CCEI Difference"
    annotation_x <- actual_value + 0.003
    annotation_hjust <- 0
  } else {
    x_lower <- -0.30
    x_upper <- -0.10
    x_breaks <- seq(x_lower, x_upper, by = 0.05)
    x_label <- "Coefficient on Higher CCEI"
    annotation_x <- actual_value - 0.003
    annotation_hjust <- 1
  }

  coefficient_plot <- ggplot(
    plot_data,
    aes(
      x = placebo_coefficient,
      y = after_stat(count / sum(count) * 100)
    )
  ) +
    geom_histogram(
      binwidth = 0.01,
      boundary = -0.25,
      fill = "#BFDDEF",
      colour = "#4C83A6",
      linewidth = 0.35
    ) +
    geom_vline(
      xintercept = actual_value,
      colour = "#8B1A1A",
      linetype = "dashed",
      linewidth = 0.9
    ) +
    annotate(
      "text",
      x = annotation_x,
      y = Inf,
      label = "Actual coefficient",
      colour = "#8B1A1A",
      hjust = annotation_hjust,
      vjust = 1.6,
      size = 4.2
    ) +
    labs(
      x = x_label,
      y = "Percent"
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      breaks = seq(0, 40, by = 5),
      expand = expansion(mult = c(0, 0.02))
    ) +
    coord_cartesian(xlim = c(x_lower, x_upper), ylim = c(0, 40)) +
    theme_classic(base_size = 15) +
    theme(
      plot.margin = margin(12, 18, 12, 12),
      axis.title = element_text(size = 16)
    )

  ggsave(
    file.path(
      figure_dir,
      paste0("placebo_coefficient_distribution_spec", s, ".png")
    ),
    coefficient_plot,
    width = 7.0,
    height = 5.0,
    dpi = 300,
    bg = "white"
  )
}

run_config <- data.frame(
  high_definition = high_definition,
  regression_variable = treatment_var,
  repetitions = n_repetitions,
  seed = master_seed,
  saved_intermediates = save_intermediates,
  donor_matrix = normalizePath(donor_file, winslash = "/"),
  run_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
)
write_csv(run_config, file.path(output_dir, "placebo_run_config.csv"))
saveRDS(run_config, file.path(output_dir, "placebo_run_config.rds"))

cat("\nCompleted successfully.\n")
cat("Results: ", normalizePath(output_dir, winslash = "/"), "\n", sep = "")
print(placebo_summary, width = Inf)
}

for (definition in definitions_to_run) {
  run_definition(definition)
}
})
