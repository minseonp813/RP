# Shared reassignment calculations for main Figure 4 and Appendix Figure A8.
# Both use the same retained donor matrix, 500 assignments and Table 3 reference.
# Call after loading dplyr; columns are the controlled specifications shown.
estimate_table3_placebo <- function(code_dir, columns, output_prefix) {
  stopifnot(length(columns) > 0L, all(columns %in% c(2L, 3L, 5L, 6L)),
            !anyDuplicated(columns))
  figure_dir <- file.path(code_dir, "results/figures")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  distance_panel <- haven::read_dta(file.path(code_dir, "data/panel_individual.dta"))
  # Reuse the retained full donor matrix; no index recalculation.
  donor_file <- file.path(code_dir, "results/placebo_normalized/placebo_donor_matrix.csv")
  donors <- readr::read_csv(donor_file, show_col_types = FALSE, progress = FALSE,
    col_select = c(target_group_id, post, member1_id, member2_id, donor_group_id,
                   is_own, Ihat1_donor, Ihat2_donor),
    col_types = readr::cols(.default = readr::col_character(), post = readr::col_integer(),
      is_own = readr::col_integer(), Ihat1_donor = readr::col_double(),
      Ihat2_donor = readr::col_double()))
  roster <- donors |>
    distinct(target_group_id, post, member1_id, member2_id) |>
    arrange(post, target_group_id)
  stopifnot(nrow(roster) == 1304, !anyDuplicated(roster[c("target_group_id", "post")]))
  counts <- donors |>
    filter(is_own == 0) |>
    count(target_group_id, post)
  stopifnot(nrow(counts) == 1304, all(counts$n == 651))
  matrix_means <- donors |>
    filter(is_own == 0) |>
    group_by(target_group_id, post, member1_id, member2_id) |>
    summarise(M1 = mean(ifelse(is.na(Ihat1_donor), .5, Ihat1_donor)),
              M2 = mean(ifelse(is.na(Ihat2_donor), .5, Ihat2_donor)), .groups = "drop")
  benchmark_check <- bind_rows(
    transmute(matrix_means, id = member1_id, post, M = M1),
    transmute(matrix_means, id = member2_id, post, M = M2)) |>
    inner_join(select(distance_panel, id, post, M_ccei), by = c("id", "post"))
  stopifnot(nrow(benchmark_check) == 2608,
            max(abs(benchmark_check$M - benchmark_check$M_ccei)) < 1e-10)
  # Estimate only the requested controlled Table 3 specifications in each repetition.
  # The retained panel includes the exact corner/midpoint shares prepared for Table 3.
  regression_data <- filter(distance_panel, !is.na(I_ccei))
  stopifnot(nrow(regression_data) == 2560, n_distinct(regression_data$class) == 64,
            n_distinct(regression_data$id) == 1304,
            sum(table(regression_data$id) == 1L) == 48L,
            all(is.finite(regression_data$I_ccei)))
  individual_controls <- c(
    "mathscore_i", "mathscore_diff", "height_i", "height_diff",
    "outgoing_i", "outgoing_diff", "opened_i", "opened_diff",
    "agreeable_i", "agreeable_diff", "conscientious_i", "conscientious_diff",
    "stable_i", "stable_diff")
  friendship_controls <- c("inclass_n_friends_i", "inclass_n_diff",
                          "inclass_popularity_i", "inclass_pop_diff")
  missing_controls <- c("mathscore_diff_missing", "outgoing_diff_missing",
    "opened_diff_missing", "agreeable_diff_missing", "conscientious_diff_missing",
    "stable_diff_missing")
  share_controls <- c("corner_share_i", "corner_share_diff", "mid_share_i", "mid_share_diff")
  full_controls <- c(individual_controls, friendship_controls, missing_controls, share_controls)
  gender_controls <- c("female_i_male_j", "male_i_female_j")
  estimate_table3 <- function(data, outcome) {
    bind_rows(lapply(columns, function(column) {
      focal <- if (column <= 3L) "HighCCEI_both_high" else "ccei_gap_ij"
      fe <- if (column %in% c(3L, 6L)) "id" else "class"
      controls <- full_controls
      if (column %in% c(2L, 5L)) controls <- c(controls, gender_controls)
      rhs <- c(focal, "M_ccei", controls)
      formula <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + "), "|", fe))
      fit <- fixest::feols(formula, data = data, vcov = ~class, fixef.rm = "none",
        ssc = fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, t.df = "min"),
        nthreads = 1, notes = FALSE)
      stopifnot(nobs(fit) == 2560, all(is.finite(coef(fit))), all(is.finite(fixest::se(fit))))
      tibble(specification = column, focal = focal, coefficient = unname(coef(fit)[focal]),
        se = unname(fixest::se(fit)[focal]), coefficient_M = unname(coef(fit)["M_ccei"]),
        N = nobs(fit), clusters = 64L)
    }))
  }
  actual_coefficients <- read.csv(file.path(code_dir, "results/tables/table3_focal_estimates.csv"))
  actual_coefficients <- actual_coefficients[actual_coefficients$specification %in% columns, ]
  stopifnot(nrow(actual_coefficients) == length(columns),
            setequal(actual_coefficients$specification, columns),
            all(actual_coefficients$N == 2560), all(actual_coefficients$clusters == 64))

  # Resolve donor/member rows once; reassignment changes only the outcome.
  strata <- split(seq_len(nrow(roster)), roster$post)
  stopifnot(all(lengths(strata) == 652))
  key <- function(group_id, post, donor_group_id) paste(group_id, post, donor_group_id, sep = "|")
  donor_keys <- key(donors$target_group_id, donors$post, donors$donor_group_id)
  stopifnot(!anyDuplicated(donor_keys))
  lookup_row <- setNames(seq_len(nrow(donors)), donor_keys)
  member_keys <- paste(c(roster$member1_id, roster$member2_id), rep(roster$post, 2), sep = "|")
  member_rows <- match(paste(regression_data$id, regression_data$post, sep = "|"), member_keys)
  stopifnot(!anyDuplicated(member_keys), !anyNA(member_rows))

  seed <- 20260812L
  n_repetitions <- 500L
  set.seed(seed)
  coefficient_draws <- vector("list", n_repetitions)
  undefined_counts <- integer(n_repetitions)
  for (r in seq_len(n_repetitions)) {
    donor_assignment <- character(nrow(roster))
    for (indices in strata) {
      randomized <- sample(indices, length(indices), replace = FALSE)
      donor_indices <- c(randomized[-1L], randomized[1L])
      stopifnot(!anyDuplicated(donor_indices), setequal(donor_indices, indices),
                all(roster$post[donor_indices] == roster$post[randomized]))
      donor_assignment[randomized] <- roster$target_group_id[donor_indices]
    }
    stopifnot(all(donor_assignment != roster$target_group_id))
    rows <- unname(lookup_row[key(roster$target_group_id, roster$post, donor_assignment)])
    stopifnot(!anyNA(rows), all(donors$is_own[rows] == 0))
    donor_distance <- c(donors$Ihat1_donor[rows], donors$Ihat2_donor[rows])[member_rows]
    undefined_counts[r] <- sum(is.na(donor_distance))
    regression_data$I_placebo <- ifelse(is.na(donor_distance), .5, donor_distance)
    coefficient_draws[[r]] <- estimate_table3(regression_data, "I_placebo") |>
      mutate(repetition = r, undefined_donor_observations = undefined_counts[r])
    if (r == 1 || r %% 25 == 0) message(output_prefix, ": repetition ", r, "/", n_repetitions)
  }
  placebo_coefficients <- bind_rows(coefficient_draws)
  stopifnot(nrow(placebo_coefficients) == 500L * length(columns),
            all(is.finite(placebo_coefficients$coefficient)),
            all(table(placebo_coefficients$specification) == 500),
            all(placebo_coefficients$N == 2560), all(placebo_coefficients$clusters == 64))
  figure4_summary <- placebo_coefficients |>
    group_by(specification) |>
    summarise(placebo_median = median(coefficient),
      placebo_lower = unname(quantile(coefficient, .025)),
      placebo_upper = unname(quantile(coefficient, .975)),
      repetitions = n(), placebo_sd = sd(coefficient),
      .groups = "drop") |>
    left_join(actual_coefficients, by = "specification")
  readr::write_csv(placebo_coefficients, file.path(figure_dir, paste0(output_prefix, "_coefficients.csv")))
  readr::write_csv(figure4_summary, file.path(figure_dir, paste0(output_prefix, "_summary.csv")))
  readr::write_csv(tibble(repetitions = n_repetitions, seed = seed, sample_N = 2560,
    clusters = 64, matching = "cyclic reassignment within wave across all classes",
    undefined_donor_value = .5), file.path(figure_dir, paste0(output_prefix, "_run_config.csv")))

  message(output_prefix, " completed: ", n_repetitions, " repetitions x ",
          length(columns), " fits; N=2560, 64 classes; seed ", seed)
  invisible(figure4_summary)
}
