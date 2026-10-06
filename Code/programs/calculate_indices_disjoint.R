# programs/calculate_indices_disjoint.R
# Last updated: 2026-10-06
# Purpose: calculate reusable disjoint-choice inputs for Table A7; no regressions.
# Run from Code: Rscript programs/calculate_indices_disjoint.R
# Then run the Table A7 section of 08_Tables_Appendix.do.
# Inputs: data/panel_final.dta (balanced pair roster), base_raw.dta, end_raw.dta,
#         programs/calculate_rp_indices.R and build_placebo_donor_matrices.R.
# Outputs: results/tests/disjoint_choice/indices/split_0001.dta, ...,
#          a cache manifest, split-half benchmark summaries, and run_config.dta.
# Settings: DISJOINT_REPS (500), DISJOINT_CORES (1), DISJOINT_OUTPUT_DIR (optional).
# Both halves use all non-own same-wave donors for M, with undefined donor
# distances imputed to 0.5. Full donor matrices are not retained for this test.
# Sections: 1 inputs/cache; 2 independent 9/9 partitions; 3 CCEI, distance and
# choice shares; 4 outcome-half M through the shared builder; 5 resumable exports.

build_disjoint_choice_inputs <- function(
    code_dir = getwd(), repetitions = as.integer(Sys.getenv("DISJOINT_REPS", "500")),
    cores = as.integer(Sys.getenv("DISJOINT_CORES", "1")),
    output_dir = Sys.getenv("DISJOINT_OUTPUT_DIR", file.path(code_dir, "results", "tests", "disjoint_choice"))) {
  # 1. Inputs and cache identity; increasing the repetition count can resume.
  code_dir <- normalizePath(code_dir, winslash = "/", mustWork = TRUE)
  local_library <- file.path(code_dir, ".R-library")
  if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
  stopifnot(length(repetitions) == 1L, is.finite(repetitions), repetitions >= 1L,
            length(cores) == 1L, is.finite(cores), cores >= 1L)
  source(file.path(code_dir, "programs", "calculate_rp_indices.R"), local = TRUE)
  source(file.path(code_dir, "programs", "build_placebo_donor_matrices.R"), local = TRUE)
  seed <- 20260812L
  plain <- function(x) trimws(as.character(x))
  pairs <- as.data.frame(haven::read_dta(file.path(code_dir, "data", "panel_final.dta")))
  roster_columns <- c("group_id", "id_mover_base", "id_nonmover_base", "id_mover_end", "id_nonmover_end")
  pairs <- pairs[, roster_columns]
  pairs[] <- lapply(pairs, plain)
  pairs <- pairs[order(pairs$group_id), ]
  rownames(pairs) <- NULL
  stopifnot(nrow(pairs) > 1L, !anyNA(pairs), !anyDuplicated(pairs$group_id))
  base <- rp_load_wave(file.path(code_dir, "data", "base_raw.dta")); base$post <- 0L
  end <- rp_load_wave(file.path(code_dir, "data", "end_raw.dta")); end$post <- 1L
  raw <- rbind(base, end)
  raw <- raw[raw$group_id %in% pairs$group_id, ]
  individual <- split(raw[raw$round_number <= 18L, ], paste(raw$id[raw$round_number <= 18L], raw$post[raw$round_number <= 18L]))
  collective <- raw[raw$round_number >= 19L & raw$mover == 1L, ]
  collective <- split(collective, paste(collective$group_id, collective$post))
  blocks <- list()
  for (wave in c("base", "end")) {
    post <- as.integer(wave == "end")
    for (k in seq_len(nrow(pairs))) {
      id1 <- pairs[[paste0("id_mover_", wave)]][k]
      id2 <- pairs[[paste0("id_nonmover_", wave)]][k]
      choices <- list(one = individual[[paste(id1, post)]], two = individual[[paste(id2, post)]],
                      group = collective[[paste(pairs$group_id[k], post)]])
      choices <- lapply(choices, function(d) d[order(d$round_number), ])
      stopifnot(all(vapply(choices, nrow, integer(1)) == 18L), id1 != id2)
      blocks[[length(blocks) + 1L]] <- c(list(group_id = pairs$group_id[k], post = post,
                                            wave = wave, id1 = id1, id2 = id2), choices)
    }
  }
  identity_files <- file.path(code_dir, c("data/base_raw.dta", "data/end_raw.dta",
    "programs/calculate_indices_disjoint.R", "programs/calculate_rp_indices.R", "programs/build_placebo_donor_matrices.R"))
  manifest <- list(version = 1L, seed = seed, roster = pairs,
                   hashes = unname(tools::md5sum(identity_files)))
  manifest_path <- file.path(output_dir, "split_config.rds")
  index_dir <- file.path(output_dir, "indices")
  if (file.exists(manifest_path)) {
    if (!identical(readRDS(manifest_path), manifest)) {
      stop("Split-choice cache differs from these inputs or code. Choose a new DISJOINT_OUTPUT_DIR.")
    }
  } else {
    cached <- list.files(output_dir, recursive = TRUE)
    if (any(basename(cached) != "README.md")) stop("Existing split-choice cache has no manifest.")
    dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(manifest, manifest_path)
  }

  distance <- function(one, two, group) {
    cost <- function(i) rp_cost_ccei(rp_expenditure(rbind(i, group)),
                                    c(rep("I", nrow(i)), rep("G", nrow(group))))
    rp_index(cost(one), cost(two), cost(rbind(one, two)))
  }
  shares <- function(d) c(corner = mean(d$coord_x == 0 | d$coord_y == 0),
                          mid = mean(d$coord_x == d$coord_y))

  for (repetition in seq_len(repetitions)) {
    path <- file.path(index_dir, sprintf("split_%04d.dta", repetition))
    if (file.exists(path)) {
      saved <- as.data.frame(haven::read_dta(path))
      stopifnot(nrow(saved) == 4L * nrow(pairs), !anyDuplicated(saved[c("id", "post")]),
                all(saved$repetition == repetition))
      message("[resume] ", basename(path)); next
    }
    # 2. Independently split each member's and the group's 18 choices into 9/9.
    halves <- lapply(seq_along(blocks), function(k) {
      set.seed(seed + repetition * 100000L + k)
      b <- blocks[[k]]
      selected <- lapply(b[c("one", "two", "group")], function(d) sort(sample.int(18L, 9L)))
      list(A = Map(function(d, rows) d[rows, ], b[c("one", "two", "group")], selected),
           B = Map(function(d, rows) d[setdiff(seq_len(18L), rows), ], b[c("one", "two", "group")], selected))
    })
    result <- NULL
    for (half in c("A", "B")) {
      half_pairs <- pairs
      half_raw <- list(base = list(), end = list())
      member_rows <- vector("list", length(blocks))
      for (k in seq_along(blocks)) {
        # 3. Calculate own-pair distances, individual CCEIs and exact-choice shares.
        b <- blocks[[k]]; h <- halves[[k]][[half]]
        I1 <- distance(h$one, h$two, h$group)
        score1 <- 1 - rp_cost_ccei(rp_expenditure(h$one))
        score2 <- 1 - rp_cost_ccei(rp_expenditure(h$two))
        c1 <- shares(h$one); c2 <- shares(h$two)
        member_rows[[k]] <- data.frame(group_id = b$group_id, post = b$post,
          id = c(b$id1, b$id2), partner_id = c(b$id2, b$id1),
          ccei = c(score1, score2), partner_ccei = c(score2, score1),
          I = c(I1, 1 - I1), corner_share = c(c1["corner"], c2["corner"]),
          corner_diff = c(c1["corner"] - c2["corner"], c2["corner"] - c1["corner"]),
          mid_share = c(c1["mid"], c2["mid"]),
          mid_diff = c(c1["mid"] - c2["mid"], c2["mid"] - c1["mid"]))
        row <- match(b$group_id, half_pairs$group_id)
        half_pairs[row, paste0("Ihat_1g_", b$wave)] <- I1
        half_pairs[row, paste0("Ihat_2g_", b$wave)] <- 1 - I1
        half_raw[[b$wave]][[length(half_raw[[b$wave]]) + 1L]] <- do.call(rbind, h)
      }
      d <- do.call(rbind, member_rows)
      # 4. Recompute M using only this half's individual and donor-group choices.
      benchmark <- build_placebo_donor_matrix("ccei", code_dir, half_pairs,
        do.call(rbind, half_raw$base), do.call(rbind, half_raw$end),
        output_dir = file.path(output_dir, "benchmarks", sprintf("split_%04d_%s", repetition, half)),
        chunk_size = 64L, cores = cores, choices_per_member = 9L, save_donor_matrix = FALSE)
      matched <- match(paste(d$id, d$post), paste(benchmark$id, benchmark$post))
      stopifnot(!anyNA(matched), all(benchmark$n_all == nrow(pairs) - 1L))
      d$M <- benchmark$M_all_imp[matched]
      d$n_donors <- benchmark$n_all[matched]
      names(d)[!names(d) %in% c("group_id", "post", "id", "partner_id")] <-
        paste0(names(d)[!names(d) %in% c("group_id", "post", "id", "partner_id")], "_", half)
      result <- if (is.null(result)) d else merge(result, d, by = c("group_id", "post", "id", "partner_id"))
    }
    # 5. Save one member-wave input file per repetition; old coefficient caches are unused.
    result$repetition <- repetition
    stopifnot(nrow(result) == 4L * nrow(pairs), !anyDuplicated(result[c("id", "post")]))
    temporary <- paste0(path, ".tmp")
    haven::write_dta(result, temporary, version = 14)
    if (!file.rename(temporary, path)) stop("Could not save split inputs: ", path)
    message(sprintf("[saved] split %d/%d", repetition, repetitions))
  }
  haven::write_dta(data.frame(repetitions = repetitions, n_pairs = nrow(pairs), seed = seed),
                   file.path(output_dir, "run_config.dta"), version = 14)
  message("Completed split-choice inputs. Run the Table A7 section in 08_Tables_Appendix.do.")
}

build_disjoint_choice_inputs()
