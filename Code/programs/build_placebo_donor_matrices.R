# Latest update: 2026-10-06
# Purpose: shared CCEI, HM, MaxMPI, and risk-aversion donor benchmarks for 01.
# Inputs: the balanced wide pair panel and loaded base/end choice data from 01.
# Outputs: resumable donor chunks, a donor matrix, and member-wave M summaries.
# Sections: 1 inputs/roster; 2 cache identity; 3 donor distances; 4 resumable
# calculation; 5 own-pair validation; 6 non-own donor averages and exports.
# Full benchmarks use every non-own pair in the same wave, without a time cap.
# Undefined distances are imputed to 0.5 in M_all_imp; M_all_drop excludes them.

build_placebo_donor_matrix <- function(
    measure, package_dir, pairs, base, end,
    output_dir = file.path(package_dir, "results", "benchmarks", measure),
    chunk_size = 8L, cores = 1L, max_targets = 0L, max_donors = 0L,
    cost_timeout = 0) {
  # 1. Validate inputs and reconstruct the balanced pair-wave roster.
  stopifnot(measure %in% c("ccei", "hm", "maxmpi", "ra"),
            length(chunk_size) == 1L, is.finite(chunk_size), chunk_size >= 1L,
            chunk_size == as.integer(chunk_size),
            length(cores) == 1L, is.finite(cores), cores >= 1L,
            cores == as.integer(cores),
            length(max_targets) == 1L, is.finite(max_targets), max_targets >= 0L,
            max_targets == as.integer(max_targets),
            length(max_donors) == 1L, is.finite(max_donors), max_donors >= 0L,
            max_donors == as.integer(max_donors),
            length(cost_timeout) == 1L, is.finite(cost_timeout), cost_timeout >= 0)
  numerical_source <- file.path(package_dir, "programs", "calculate_rp_indices.R")
  if (measure != "ra") source(numerical_source, local = TRUE)
  plain <- function(x) trimws(as.character(x))
  pairs <- as.data.frame(pairs)
  required <- c("group_id", unlist(lapply(c("base", "end"), function(wave) {
    c(paste0(c("id_mover_", "id_nonmover_"), wave),
      if (measure == "ra") paste0(c("RA_1_", "RA_2_", "RA_g_"), wave) else {
        prefix <- if (measure == "ccei") "Ihat_" else paste0("Ihat_", measure, "_")
        paste0(prefix, c("1g_", "2g_"), wave)
      })
  })))
  missing <- setdiff(required, names(pairs))
  if (length(missing)) stop("Wide pair input lacks: ", paste(missing, collapse = ", "))
  pairs$group_id <- plain(pairs$group_id)
  if (!nrow(pairs) || anyNA(pairs$group_id) || anyDuplicated(pairs$group_id)) {
    stop("Expected one nonmissing row per balanced pair.")
  }
  roster_full <- do.call(rbind, lapply(c("base", "end"), function(wave) {
    mover <- plain(pairs[[paste0("id_mover_", wave)]])
    nonmover <- plain(pairs[[paste0("id_nonmover_", wave)]])
    if (anyNA(mover) || anyNA(nonmover) || any(mover == nonmover)) {
      stop("Expected two distinct members for each pair in ", wave, ".")
    }
    if (any(substr(mover, 1L, 5L) != substr(nonmover, 1L, 5L))) {
      stop("Pair members must share the same class prefix in ", wave, ".")
    }
    mover_first <- mover < nonmover
    if (measure == "ra") {
      ra_mover <- as.numeric(pairs[[paste0("RA_1_", wave)]])
      ra_nonmover <- as.numeric(pairs[[paste0("RA_2_", wave)]])
      ra_group <- as.numeric(pairs[[paste0("RA_g_", wave)]])
      numerator1 <- (ra_mover - ra_group)^2
      numerator2 <- (ra_nonmover - ra_group)^2
      denominator <- numerator1 + numerator2
      actual_mover <- ifelse(is.finite(denominator) & denominator > 0,
                             numerator1 / denominator, NA_real_)
      actual_nonmover <- ifelse(is.finite(denominator) & denominator > 0,
                                numerator2 / denominator, NA_real_)
    } else {
      prefix <- if (measure == "ccei") "Ihat_" else paste0("Ihat_", measure, "_")
      actual_mover <- as.numeric(pairs[[paste0(prefix, "1g_", wave)]])
      actual_nonmover <- as.numeric(pairs[[paste0(prefix, "2g_", wave)]])
    }
    roster <- data.frame(
      target_group_id = pairs$group_id, post = as.integer(wave == "end"),
      target_class = substr(ifelse(mover_first, mover, nonmover), 1L, 5L),
      member1_id = ifelse(mover_first, mover, nonmover),
      member2_id = ifelse(mover_first, nonmover, mover),
      actual1 = ifelse(mover_first, actual_mover, actual_nonmover),
      actual2 = ifelse(mover_first, actual_nonmover, actual_mover),
      stringsAsFactors = FALSE
    )
    if (measure == "ra") {
      roster$RA1 <- ifelse(mover_first, ra_mover, ra_nonmover)
      roster$RA2 <- ifelse(mover_first, ra_nonmover, ra_mover)
      roster$RA_g <- ra_group
    }
    roster
  }))
  roster_full <- roster_full[order(roster_full$post, roster_full$target_group_id), , drop = FALSE]
  rownames(roster_full) <- NULL
  roster <- if (max_targets > 0L) head(roster_full, max_targets) else roster_full
  raw <- NULL
  if (measure != "ra") {
    choice_columns <- c("group_id", "id", "round_number", "mover",
                        "coord_x", "coord_y", "intercept_x", "intercept_y")
    choices <- function(d, post) {
      missing <- setdiff(choice_columns, names(d))
      if (length(missing)) stop("Choice input lacks: ", paste(missing, collapse = ", "))
      d <- as.data.frame(d[d$group_id %in% pairs$group_id, choice_columns])
      for (name in c("group_id", "id")) d[[name]] <- plain(d[[name]])
      for (name in setdiff(choice_columns, c("group_id", "id"))) d[[name]] <- as.numeric(d[[name]])
      d$post <- post
      d
    }
    raw <- rbind(choices(base, 0L), choices(end, 1L))
    raw <- raw[order(raw$post, raw$group_id, raw$id, raw$round_number), , drop = FALSE]
    rownames(raw) <- NULL
  }

  # 2. Reuse chunks only when the inputs, numerical code, and settings match.
  manifest <- list(version = 1L, measure = measure, chunk_size = as.integer(chunk_size),
                   max_targets = as.integer(max_targets), max_donors = as.integer(max_donors),
                   cost_timeout = as.numeric(cost_timeout), roster = roster_full, choices = raw,
                   numerical_code = if (measure != "ra") unname(tools::md5sum(numerical_source)),
                   builder_code = unname(tools::md5sum(file.path(package_dir, "programs", "build_placebo_donor_matrices.R"))))
  manifest_path <- file.path(output_dir, "benchmark_config.rds")
  chunk_dir <- file.path(output_dir, "donor_matrix_chunks")
  matrix_path <- file.path(output_dir, "placebo_donor_matrix.csv")
  analysis_csv <- file.path(output_dir, "placebo_normalized_member_wave.csv")
  analysis_dta <- file.path(output_dir, "placebo_normalized_member_wave.dta")
  if (file.exists(manifest_path)) {
    if (!identical(readRDS(manifest_path), manifest)) {
      stop("Benchmark cache does not match these inputs/settings. Use a new output_dir: ", output_dir)
    }
  } else {
    cached <- c(matrix_path, analysis_csv, analysis_dta,
                list.files(chunk_dir, pattern = "^donor_matrix_chunk_.*[.]csv$", full.names = TRUE))
    if (any(file.exists(cached))) {
      stop("Existing benchmark cache has no input/config identity; it cannot be reused safely. ",
           "Keep it for comparison and choose a new output_dir: ", output_dir)
    }
    dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(manifest, manifest_path)
  }
  id_columns <- c("group_id", "target_group_id", "target_class", "class", "member1_id",
                  "member2_id", "donor_group_id", "donor_class", "id", "partner_id")
  read_output <- function(path) {
    header <- names(read.csv(path, nrows = 0L, check.names = FALSE))
    classes <- rep(NA_character_, length(header))
    names(classes) <- header
    classes[header %in% id_columns] <- "character"
    classes[header == "post"] <- "integer"
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = classes)
  }
  if (all(file.exists(c(matrix_path, analysis_csv, analysis_dta)))) {
    message("[complete] ", measure, " benchmark outputs already exist")
    return(invisible(read_output(analysis_csv)))
  }

  # 3. Calculate target-member distances against each same-wave donor pair.
  if (measure != "ra") {
    individual_raw <- raw[raw$round_number <= 18L, , drop = FALSE]
    group_raw <- raw[raw$round_number >= 19L & raw$mover == 1L, , drop = FALSE]
    individual <- split(individual_raw, paste(individual_raw$group_id, individual_raw$post, individual_raw$id, sep = "|"))
    groups <- split(group_raw, paste(group_raw$group_id, group_raw$post, sep = "|"))
    individual <- lapply(individual, function(d) d[order(d$round_number), , drop = FALSE])
    groups <- lapply(groups, function(d) d[order(d$round_number), , drop = FALSE])
    cost <- switch(measure,
      ccei = function(EX, side) as.numeric(rp_cost_ccei(EX, side)),
      hm = function(EX, side) as.numeric(rp_cost_hm(EX, side)),
      maxmpi = function(EX, side) {
        run <- function() {
          if (cost_timeout > 0) setTimeLimit(elapsed = cost_timeout, transient = TRUE)
          on.exit(if (cost_timeout > 0) setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
          z <- rp_cost_mpi(EX, side)
          if (!isTRUE(z$exhausted)) stop("MaxMPI search did not exhaust its branch-and-bound tree.")
          as.numeric(z$value)
        }
        tryCatch(run(), error = function(e) {
          if (cost_timeout > 0 && grepl("time limit", conditionMessage(e), ignore.case = TRUE)) return(NA_real_)
          stop(e)
        })
      }
    )
    cross_cost <- function(i, g) {
      cost(rp_expenditure(rbind(i, g)), c(rep("I", nrow(i)), rep("G", nrow(g))))
    }
  }
  target_rows <- function(target) {
    if (measure != "ra") {
      i1 <- individual[[paste(target$target_group_id, target$post, target$member1_id, sep = "|")]]
      i2 <- individual[[paste(target$target_group_id, target$post, target$member2_id, sep = "|")]]
      if (is.null(i1) || nrow(i1) != 18L || is.null(i2) || nrow(i2) != 18L) {
        stop("Individual choices are incomplete for target ", target$target_group_id, " / ", target$post)
      }
    }
    donors <- roster_full[roster_full$post == target$post, , drop = FALSE]
    if (max_donors > 0L) {
      own <- donors[donors$target_group_id == target$target_group_id, , drop = FALSE]
      other <- donors[donors$target_group_id != target$target_group_id, , drop = FALSE]
      seed <- sum(utf8ToInt(paste(target$target_group_id, target$post, sep = "|"))) * 1009
      set.seed(seed %% .Machine$integer.max)
      sampled <- sample.int(nrow(other), min(max_donors, nrow(other)), replace = FALSE)
      donors <- rbind(own, other[sampled, , drop = FALSE])
    }
    rows <- lapply(seq_len(nrow(donors)), function(j) {
      donor <- donors[j, , drop = FALSE]
      own <- donor$target_group_id == target$target_group_id
      if (measure == "ra") {
        c1 <- (target$RA1 - donor$RA_g)^2
        c2 <- (target$RA2 - donor$RA_g)^2
        c12 <- c1 + c2
        ih1 <- if (is.finite(c12) && c12 > 0) c1 / c12 else NA_real_
        ih2 <- if (is.finite(c12) && c12 > 0) c2 / c12 else NA_real_
      } else {
        gg <- groups[[paste(donor$target_group_id, donor$post, sep = "|")]]
        if (is.null(gg) || nrow(gg) != 18L) stop("Collective choices are incomplete for donor ", donor$target_group_id)
        c1 <- cross_cost(i1, gg)
        c2 <- cross_cost(i2, gg)
        c12 <- cross_cost(rbind(i1, i2), gg)
        ih1 <- rp_index(c1, c2, c12)
        ih2 <- ifelse(is.na(ih1), NA_real_, 1 - ih1)
        # Review-only capped pilots use the already calculated actual index.
        if (measure == "maxmpi" && cost_timeout > 0 && own) {
          ih1 <- target$actual1
          ih2 <- target$actual2
        }
      }
      data.frame(target_group_id = target$target_group_id, post = target$post,
                 target_class = target$target_class, member1_id = target$member1_id,
                 member2_id = target$member2_id, donor_group_id = donor$target_group_id,
                 donor_class = donor$target_class, is_own = as.integer(own),
                 same_class = as.integer(donor$target_class == target$target_class),
                 cost1 = c1, cost2 = c2, cost12 = c12, Ihat1_donor = ih1,
                 Ihat2_donor = ih2, degenerate = as.integer(is.na(ih1)), stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  }

  # 4. Resume complete chunks; parallel workers must all succeed before saving.
  started <- Sys.time()
  chunk_starts <- seq.int(1L, nrow(roster), by = chunk_size)
  chunk_paths <- character(length(chunk_starts))
  dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
  for (k in seq_along(chunk_starts)) {
    indices <- seq.int(chunk_starts[k], min(chunk_starts[k] + chunk_size - 1L, nrow(roster)))
    chunk_path <- file.path(chunk_dir, sprintf("donor_matrix_chunk_%04d.csv", k - 1L))
    chunk_paths[k] <- chunk_path
    n_donors <- vapply(roster$post[indices], function(post) {
      n_wave <- sum(roster_full$post == post)
      if (max_donors > 0L) 1L + min(max_donors, n_wave - 1L) else n_wave
    }, numeric(1))
    if (file.exists(chunk_path)) {
      saved <- tryCatch(read_output(chunk_path), error = function(e) NULL)
      if (!is.null(saved) && nrow(saved) == sum(n_donors) &&
          all(c("target_group_id", "post", "donor_group_id") %in% names(saved)) &&
          !anyDuplicated(saved[c("target_group_id", "post", "donor_group_id")]) &&
          setequal(paste(saved$target_group_id, saved$post),
                   paste(roster$target_group_id[indices], roster$post[indices]))) {
        message("[resume] ", basename(chunk_path))
        next
      }
      message("[rebuild] ", basename(chunk_path), " is incomplete")
    }
    worker <- function(i) target_rows(roster[i, , drop = FALSE])
    results <- if (cores > 1L && .Platform$OS.type != "windows") {
      parallel::mclapply(indices, worker, mc.cores = min(cores, length(indices)), mc.preschedule = TRUE)
    } else lapply(indices, worker)
    failed <- which(!vapply(results, is.data.frame, logical(1)))
    if (length(failed)) stop("Benchmark worker failed: ", paste(results[failed], collapse = "; "))
    result <- do.call(rbind, results)
    if (nrow(result) != sum(n_donors)) stop("Benchmark worker returned an incomplete chunk.")
    temporary <- paste0(chunk_path, ".tmp")
    write.csv(result, temporary, row.names = FALSE, na = "")
    if (!file.rename(temporary, chunk_path)) stop("Could not save benchmark chunk: ", chunk_path)
    message("[saved] ", basename(chunk_path), ": ", format(nrow(result), big.mark = ","), " rows")
  }
  matrix <- do.call(rbind, lapply(chunk_paths, read_output))

  # 5. The own-pair diagonal must reproduce the indices calculated in 01.
  diagonal <- matrix[matrix$is_own == 1L, , drop = FALSE]
  match_rows <- match(paste(diagonal$target_group_id, diagonal$post),
                      paste(roster$target_group_id, roster$post))
  if (nrow(diagonal) != nrow(roster) || anyNA(match_rows) || anyDuplicated(match_rows)) {
    stop("Expected one own-pair diagonal row per target pair-wave.")
  }
  calculated <- c(diagonal$Ihat1_donor, diagonal$Ihat2_donor)
  stored <- c(roster$actual1[match_rows], roster$actual2[match_rows])
  if (any(is.na(calculated) != is.na(stored))) stop("Diagonal missing-value patterns do not match the actual indices.")
  difference <- abs(calculated - stored)
  max_error <- if (all(is.na(difference))) NA_real_ else max(difference, na.rm = TRUE)
  if (!is.na(max_error) && max_error > 1e-10) stop("Diagonal does not reproduce the actual indices: max error = ", max_error)
  message("Diagonal validation passed; max error = ", format(max_error, scientific = TRUE))

  # 6. Average non-own distances for all, same-class, and other-class donor pools.
  pool_summary <- function(x, pool) {
    valid <- !is.na(x)
    values <- c(if (length(x)) mean(replace(x, !valid, 0.5)) else NA_real_,
                if (any(valid)) mean(x[valid]) else NA_real_,
                length(x), sum(valid), if (length(x)) mean(!valid) else NA_real_)
    names(values) <- paste0(c("M_", "M_", "n_", "nvalid_", "degfrac_"),
                            pool, c("_imp", "_drop", "", "", ""))
    values
  }
  blocks <- split(matrix, paste(matrix$target_group_id, matrix$post, sep = "|"))
  analysis <- do.call(rbind, lapply(blocks, function(block) {
    own <- block[block$is_own == 1L, , drop = FALSE]
    donors <- block[block$is_own == 0L, , drop = FALSE]
    do.call(rbind, lapply(1:2, function(member) {
      values <- donors[[paste0("Ihat", member, "_donor")]]
      costs <- donors[[paste0("cost", member)]]
      own_cost <- own[[paste0("cost", member)]]
      row <- list(group_id = own$target_group_id, post = own$post, class = own$target_class,
                  id = own[[paste0("member", member, "_id")]],
                  partner_id = own[[paste0("member", 3L - member, "_id")]],
                  cost_own = own_cost, costN_own = own$cost12,
                  I_actual = own[[paste0("Ihat", member, "_donor")]])
      for (pool in c("all", "cls", "outclass")) {
        selected <- switch(pool, all = rep(TRUE, nrow(donors)),
                           cls = donors$same_class == 1L, outclass = donors$same_class == 0L)
        row <- c(row, as.list(pool_summary(values[selected], pool)))
      }
      row$P_all <- if (length(costs)) mean(own_cost <= costs) else NA_real_
      row$P_cls <- if (any(donors$same_class == 1L)) mean(own_cost <= costs[donors$same_class == 1L]) else NA_real_
      as.data.frame(row, stringsAsFactors = FALSE)
    }))
  }))
  for (pool in c("all", "cls")) {
    key <- paste(analysis$group_id, analysis$post, analysis$id, sep = "|")
    partner_key <- paste(analysis$group_id, analysis$post, analysis$partner_id, sep = "|")
    analysis[[paste0("P_partner_", pool)]] <- analysis[[paste0("P_", pool)]][match(partner_key, key)]
    analysis[[paste0("Pdiff_", pool)]] <- analysis[[paste0("P_", pool)]] - analysis[[paste0("P_partner_", pool)]]
  }
  for (pool in c("all", "cls", "outclass")) {
    for (rule in c("imp", "drop")) {
      analysis[[paste0("Istar_", pool, "_", rule)]] <- analysis$I_actual - analysis[[paste0("M_", pool, "_", rule)]]
    }
  }
  analysis <- analysis[order(analysis$post, analysis$group_id, analysis$id), , drop = FALSE]
  rownames(analysis) <- NULL
  write.csv(matrix, matrix_path, row.names = FALSE, na = "")
  write.csv(analysis, analysis_csv, row.names = FALSE, na = "")
  stata <- analysis
  names(stata)[names(stata) == "class"] <- "_class"
  haven::write_dta(stata, analysis_dta, version = 14)
  message("Normalized member-wave data saved: ", analysis_dta)
  message("Total elapsed time: ", format(Sys.time() - started))
  invisible(analysis)
}
