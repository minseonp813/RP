rm(list = ls())

code_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
local_library <- file.path(code_dir, ".R-library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(readr)
})
source(file.path(code_dir, "programs", "calculate_rp_indices.R"))

repetitions <- as.integer(Sys.getenv("DISJOINT_REPS", "500"))
master_seed <- 20260812L
output_root <- file.path(code_dir, "results", "tests", "disjoint_all_indices")
chunk_dir <- file.path(output_root, "chunks")
dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)

plain <- function(x) trimws(as.character(x))
panel <- as.data.frame(read_dta(file.path(code_dir, "data", "panel_individual.dta")))
for (v in c("group_id", "id", "partner_id", "class")) panel[[v]] <- plain(panel[[v]])
panel$post <- as.integer(panel$post)
panel$id_fe <- as.integer(factor(panel$id))
panel$female_i_male_j <- as.numeric(panel$male_i == 0 & panel$male_j == 1)
panel$male_i_female_j <- as.numeric(panel$male_i == 1 & panel$male_j == 0)

base <- rp_load_wave(file.path(code_dir, "data", "base_raw.dta")); base$post <- 0L
end <- rp_load_wave(file.path(code_dir, "data", "end_raw.dta")); end$post <- 1L
raw <- rbind(base, end)
raw$group_id <- plain(raw$group_id); raw$id <- plain(raw$id)

pair_panel <- panel[!duplicated(panel[c("group_id", "post")]), c("group_id", "post", "class")]
pair_panel <- pair_panel[order(pair_panel$post, pair_panel$group_id), ]
choice_blocks <- vector("list", nrow(pair_panel))
for (k in seq_len(nrow(pair_panel))) {
  g <- pair_panel$group_id[k]; w <- pair_panel$post[k]
  members <- panel[panel$group_id == g & panel$post == w, ]
  members <- members[order(members$person), ]
  d <- raw[raw$group_id == g & raw$post == w, ]
  one <- d[d$id == members$id[1] & d$round_number <= 18, ]
  two <- d[d$id == members$id[2] & d$round_number <= 18, ]
  group <- d[d$mover == 1 & d$round_number >= 19, ]
  one <- one[order(one$round_number), ]; two <- two[order(two$round_number), ]
  group <- group[order(group$round_number), ]
  stopifnot(nrow(one) == 18, nrow(two) == 18, nrow(group) == 18)
  choice_blocks[[k]] <- list(group_id = g, post = w, class = pair_panel$class[k],
                             id1 = members$id[1], id2 = members$id[2],
                             one = one, two = two, group = group)
}

measure_cost <- function(measure, choices, side = NULL) {
  EX <- rp_expenditure(choices)
  if (measure == "ccei") return(rp_cost_ccei(EX, side))
  if (measure == "hm") return(as.numeric(rp_cost_hm(EX, side)))
  z <- if (is.null(side)) list(value = rp_maxmpi(EX), exhausted = TRUE) else rp_cost_mpi(EX, side)
  if (!isTRUE(z$exhausted)) stop("MaxMPI search did not exhaust.")
  as.numeric(z$value)
}

rationality <- function(measure, choices) {
  cost <- measure_cost(measure, choices)
  if (measure == "ccei") 1 - cost else if (measure == "hm") 1 - cost / nrow(choices) else 1 - cost
}

distance <- function(measure, a, b, group) {
  cross <- function(x) {
    z <- rbind(x, group)
    measure_cost(measure, z, c(rep("I", nrow(x)), rep("G", nrow(group))))
  }
  rp_index(cross(a), cross(b), cross(rbind(a, b)))
}

half_controls <- function(d) {
  total <- d$coord_x + d$coord_y
  expensive <- ifelse(d$intercept_x < d$intercept_y, d$coord_x, d$coord_y)
  c(corner = mean(d$coord_x == 0 | d$coord_y == 0),
    midpoint = mean(d$coord_x == d$coord_y),
    ra = round(mean(ifelse(total == 0, NA, expensive / total), na.rm = TRUE), 5))
}

within_transform <- function(z, fe) {
  z <- as.matrix(z); storage.mode(z) <- "double"; f <- factor(fe)
  z - (rowsum(z, f, reorder = TRUE) / as.numeric(table(f)))[as.integer(f), , drop = FALSE]
}

fit_absorbed <- function(data, outcome, rhs, fe, cluster = "class") {
  d <- data[complete.cases(data[, unique(c(outcome, rhs, fe, cluster))]), ]
  repeat {
    counts <- table(d[[fe]])
    keep_rows <- as.character(d[[fe]]) %in% names(counts)[counts > 1L]
    if (all(keep_rows)) break
    d <- d[keep_rows, ]
  }
  y <- within_transform(d[[outcome]], d[[fe]])[, 1]
  x <- within_transform(d[, rhs, drop = FALSE], d[[fe]])
  keep <- colSums(x^2) > 1e-20; x <- x[, keep, drop = FALSE]
  fit <- lm.fit(x, y); pivot <- fit$qr$pivot[seq_len(fit$rank)]
  xr <- x[, pivot, drop = FALSE]; scores <- xr * fit$residuals
  meat <- crossprod(rowsum(scores, factor(d[[cluster]]), reorder = TRUE))
  bread <- chol2inv(qr.R(qr(xr)))
  vcov <- bread %*% meat %*% bread
  idx <- match(rhs[1], colnames(xr))
  c(coefficient = fit$coefficients[pivot][idx], se = sqrt(vcov[idx, idx]), N = nrow(d))
}

fit_pair_gap <- function(measures, source, outcome) {
  s1 <- measures[[paste0("score1_", source)]]
  s2 <- measures[[paste0("score2_", source)]]
  I1 <- measures[[paste0("I1_", outcome)]]
  d <- data.frame(gap = abs(s1 - s2),
                  distance_gap = ifelse(s1 > s2, 2 * I1 - 1,
                                        ifelse(s2 > s1, 1 - 2 * I1, 0)),
                  class = measures$class)
  d <- d[complete.cases(d), ]
  X <- cbind(`(Intercept)` = 1, gap = d$gap)
  fit <- lm.fit(X, d$distance_gap)
  bread <- chol2inv(qr.R(qr(X)))
  scores <- X * fit$residuals
  meat <- crossprod(rowsum(scores, factor(d$class), reorder = TRUE))
  vcov <- bread %*% meat %*% bread
  data.frame(specification = 0, coefficient = fit$coefficients[2],
             se = sqrt(vcov[2, 2]), N = nrow(d))
}

group_controls <- c("mathscore_i", "mathscore_diff", "height_i", "height_diff",
  "outgoing_i", "outgoing_diff", "opened_i", "opened_diff", "agreeable_i",
  "agreeable_diff", "conscientious_i", "conscientious_diff", "stable_i",
  "stable_diff", "female_i_male_j", "male_i_female_j")
group_ng <- setdiff(group_controls, c("female_i_male_j", "male_i_female_j"))
friend <- c("inclass_n_friends_i", "inclass_n_diff", "inclass_popularity_i", "inclass_pop_diff")
missing_controls <- c("mathscore_diff_missing", "outgoing_diff_missing", "opened_diff_missing",
  "agreeable_diff_missing", "conscientious_diff_missing", "stable_diff_missing")

make_measures <- function(repetition, measure) {
  rows <- vector("list", nrow(choice_blocks))
  for (k in seq_along(choice_blocks)) {
    b <- choice_blocks[[k]]
    set.seed(master_seed + repetition * 100000L + k)
    A1 <- sort(sample.int(18, 9)); A2 <- sort(sample.int(18, 9)); Ag <- sort(sample.int(18, 9))
    B1 <- setdiff(1:18, A1); B2 <- setdiff(1:18, A2); Bg <- setdiff(1:18, Ag)
    h <- list(A1 = b$one[A1, ], A2 = b$two[A2, ], Ag = b$group[Ag, ],
              B1 = b$one[B1, ], B2 = b$two[B2, ], Bg = b$group[Bg, ])
    cA1 <- half_controls(h$A1); cA2 <- half_controls(h$A2)
    cB1 <- half_controls(h$B1); cB2 <- half_controls(h$B2)
    rows[[k]] <- data.frame(group_id = b$group_id, post = b$post, class = b$class,
      score1_A = rationality(measure, h$A1), score2_A = rationality(measure, h$A2),
      score1_B = rationality(measure, h$B1), score2_B = rationality(measure, h$B2),
      I1_A = distance(measure, h$A1, h$A2, h$Ag),
      I1_B = distance(measure, h$B1, h$B2, h$Bg),
      corner1_A = cA1[1], corner2_A = cA2[1], midpoint1_A = cA1[2], midpoint2_A = cA2[2],
      RA1_A = cA1[3], RA2_A = cA2[3], corner1_B = cB1[1], corner2_B = cB2[1],
      midpoint1_B = cB1[2], midpoint2_B = cB2[2], RA1_B = cB1[3], RA2_B = cB2[3])
  }
  bind_rows(rows)
}

estimate_direction <- function(measures, source, outcome, definition) {
  d <- panel |> left_join(measures, by = c("group_id", "post", "class"))
  s1 <- d[[paste0("score1_", source)]]; s2 <- d[[paste0("score2_", source)]]
  d$treatment <- if (definition == "both_high") ifelse(d$person == 1, s1 >= s2, s2 >= s1) else
    if (definition == "both_low") ifelse(d$person == 1, s1 > s2, s2 > s1) else
      ifelse(d$person == 1, s1 - s2, s2 - s1)
  I1 <- d[[paste0("I1_", outcome)]]
  d$distance <- ifelse(d$person == 1, I1, 1 - I1)
  for (v in c("corner", "midpoint", "RA")) {
    a <- d[[paste0(v, "1_", source)]]; b <- d[[paste0(v, "2_", source)]]
    d[[paste0(v, "_i")]] <- ifelse(d$person == 1, a, b)
    d[[paste0(v, "_diff")]] <- ifelse(d$person == 1, a - b, b - a)
  }
  ra <- c("RA_i", "RA_diff"); shares <- c("corner_i", "corner_diff", "midpoint_i", "midpoint_diff")
  rhs <- list("treatment", c("treatment", group_controls, friend, missing_controls),
              c("treatment", group_controls, friend, missing_controls, ra, shares),
              c("treatment", group_ng, friend, missing_controls, ra, shares))
  fe <- c("class", "class", "class", "id_fe")
  bind_rows(lapply(1:4, function(s) as.data.frame(as.list(c(specification = s,
    fit_absorbed(d, "distance", rhs[[s]], fe[s]))))))
}

for (measure in c("ccei", "hm", "maxmpi")) {
  for (r in seq_len(repetitions)) {
    path <- file.path(chunk_dir, sprintf("%s_%04d.csv", measure, r))
    if (file.exists(path)) { message("[resume] ", basename(path)); next }
    measures <- make_measures(r, measure)
    results <- bind_rows(lapply(c("both_high", "both_low", "diff"), function(definition) {
      bind_rows(
        estimate_direction(measures, "A", "B", definition) |> mutate(direction = "A_to_B"),
        estimate_direction(measures, "B", "A", definition) |> mutate(direction = "B_to_A")
      ) |> mutate(definition = definition)
    }),
    fit_pair_gap(measures, "A", "B") |> mutate(direction = "A_to_B", definition = "pair_diff"),
    fit_pair_gap(measures, "B", "A") |> mutate(direction = "B_to_A", definition = "pair_diff")
    ) |> mutate(measure = measure, repetition = r)
    write_csv(results, path)
    message(sprintf("[saved] %s %d/%d", measure, r, repetitions))
  }
}

files <- list.files(chunk_dir, pattern = "\\.csv$", full.names = TRUE)
results <- bind_rows(lapply(files, read_csv, show_col_types = FALSE))
write_csv(results, file.path(output_root, paste0("disjoint_repetitions_", repetitions, ".csv")))
summary <- results |>
  group_by(measure, definition, specification) |>
  summarise(median = median(coefficient), lower = quantile(coefficient, .025),
            upper = quantile(coefficient, .975), share_negative = mean(coefficient < 0),
            n_estimates = n(), .groups = "drop")
write_csv(summary, file.path(output_root, "disjoint_repetitions_summary.csv"))
write_dta(as.data.frame(summary), file.path(output_root, "disjoint_repetitions_summary.dta"))
cat("Completed CCEI, HM, and MaxMPI disjoint-choice tests.\n")
