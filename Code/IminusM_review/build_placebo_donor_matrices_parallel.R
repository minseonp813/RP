build_placebo_donor_matrix <- function(measure, package_dir) {
  stopifnot(measure %in% c("ccei", "hm", "maxmpi"))

  suppressPackageStartupMessages({
    library(haven)
    library(igraph)
  })

  source(file.path(package_dir, "programs", "calculate_rp_indices.R"))

  data_dir <- file.path(package_dir, "data")
  folder <- if (measure == "ccei") "placebo_normalized" else paste0("placebo_normalized_", measure)
  default_output <- file.path(package_dir, "results", folder)
  output_dir <- Sys.getenv("PLACEBO_OUTPUT_DIR", default_output)
  chunk_dir <- file.path(output_dir, "donor_matrix_chunks")
  dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)

  matrix_path <- file.path(output_dir, "placebo_donor_matrix.csv")
  analysis_dta <- file.path(output_dir, "placebo_normalized_member_wave.dta")
  force_measure <- Sys.getenv("PLACEBO_REBUILD_MEASURE", "")
  first_chunk_path <- file.path(chunk_dir, "donor_matrix_chunk_0000.csv")
  first_chunk <- if (file.exists(first_chunk_path)) tryCatch(
    read.csv(first_chunk_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  ) else NULL
  full_wave_complete <- !is.null(first_chunk) && nrow(first_chunk) >= 652L &&
    "same_class" %in% names(first_chunk) && any(first_chunk$same_class == 0L)
  if (file.exists(matrix_path) && file.exists(analysis_dta) &&
      full_wave_complete && force_measure != measure) {
    message("[complete] ", measure, " outputs already exist; skipping")
    return(invisible(NULL))
  }

  chunk_size <- as.integer(Sys.getenv("PLACEBO_TARGET_CHUNK_SIZE", "1"))
  n_cores <- as.integer(Sys.getenv("PLACEBO_CORES", "1"))
  max_targets <- as.integer(Sys.getenv("PLACEBO_MAX_TARGETS", "0"))
  max_donors <- as.integer(Sys.getenv("PLACEBO_MAX_DONORS", "0"))
  overwrite <- Sys.getenv("PLACEBO_OVERWRITE_CHUNKS", "0") == "1"
  cost_timeout <- as.numeric(Sys.getenv("PLACEBO_COST_TIMEOUT", "0"))
  if (is.na(chunk_size) || chunk_size < 1L) stop("PLACEBO_TARGET_CHUNK_SIZE must be positive.")
  if (is.na(n_cores) || n_cores < 1L) stop("PLACEBO_CORES must be positive.")
  if (is.na(cost_timeout) || cost_timeout < 0) stop("PLACEBO_COST_TIMEOUT cannot be negative.")

  panel <- as.data.frame(read_dta(file.path(data_dir, "panel_individual.dta")))
  expected_index <- switch(measure, ccei = "Ihat_ig", hm = "Ihat_hm_ig",
                           maxmpi = "Ihat_maxmpi_ig")
  required <- c("group_id", "class", "post", "id", expected_index)
  missing <- setdiff(required, names(panel))
  if (length(missing)) stop("panel_individual.dta lacks: ", paste(missing, collapse = ", "))

  plain <- function(x) trimws(as.character(x))
  panel$group_id <- plain(panel$group_id)
  panel$id <- plain(panel$id)
  panel$class <- plain(panel$class)
  panel$post <- as.integer(panel$post)

  base <- rp_load_wave(file.path(data_dir, "base_raw.dta"))
  end <- rp_load_wave(file.path(data_dir, "end_raw.dta"))
  base$post <- 0L
  end$post <- 1L
  raw <- rbind(base, end)
  raw$group_id <- plain(raw$group_id)
  raw$id <- plain(raw$id)

  roster_parts <- split(panel, interaction(panel$group_id, panel$post, drop = TRUE))
  roster <- do.call(rbind, lapply(roster_parts, function(d) {
    ids <- sort(unique(d$id))
    if (length(ids) != 2L) stop("Expected two members for group-wave ", d$group_id[1], " / ", d$post[1])
    data.frame(
      target_group_id = d$group_id[1], post = d$post[1], target_class = d$class[1],
      member1_id = ids[1], member2_id = ids[2], stringsAsFactors = FALSE
    )
  }))
  roster <- roster[order(roster$post, roster$target_group_id), , drop = FALSE]
  rownames(roster) <- NULL
  if (!is.na(max_targets) && max_targets > 0L) roster <- head(roster, max_targets)

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
      tryCatch(
        run(),
        error = function(e) {
          if (cost_timeout > 0 && grepl("time limit", conditionMessage(e), ignore.case = TRUE)) {
            return(NA_real_)
          }
          stop(e)
        }
      )
    }
  )

  cross_cost <- function(individual_choices, group_choices) {
    combined <- rbind(individual_choices, group_choices)
    EX <- rp_expenditure(combined)
    side <- c(rep("I", nrow(individual_choices)), rep("G", nrow(group_choices)))
    cost(EX, side)
  }

  target_rows <- function(target) {
    key1 <- paste(target$target_group_id, target$post, target$member1_id, sep = "|")
    key2 <- paste(target$target_group_id, target$post, target$member2_id, sep = "|")
    i1 <- individual[[key1]]
    i2 <- individual[[key2]]
    if (is.null(i1) || nrow(i1) != 18L || is.null(i2) || nrow(i2) != 18L) {
      stop("Individual choices are incomplete for target ", target$target_group_id, " / ", target$post)
    }

    donors <- roster_full[roster_full$post == target$post, , drop = FALSE]
    if (!is.na(max_donors) && max_donors > 0L) {
      own <- donors[donors$target_group_id == target$target_group_id, , drop = FALSE]
      other <- donors[donors$target_group_id != target$target_group_id, , drop = FALSE]
      # A deterministic target-specific sample avoids the ordering bias of taking
      # the first donors and makes review builds exactly reproducible.
      seed <- sum(utf8ToInt(paste(target$target_group_id, target$post, sep = "|"))) * 1009L
      set.seed(seed %% .Machine$integer.max)
      sampled <- sample.int(nrow(other), min(max_donors, nrow(other)), replace = FALSE)
      donors <- rbind(own, other[sampled, , drop = FALSE])
    }

    rows <- vector("list", nrow(donors))
    for (j in seq_len(nrow(donors))) {
      donor <- donors[j, , drop = FALSE]
      gg <- groups[[paste(donor$target_group_id, donor$post, sep = "|")]]
      if (is.null(gg) || nrow(gg) != 18L) stop("Collective choices are incomplete for donor ", donor$target_group_id)
      c1 <- cross_cost(i1, gg)
      c2 <- cross_cost(i2, gg)
      c12 <- cross_cost(rbind(i1, i2), gg)
      ih1 <- rp_index(c1, c2, c12)
      rows[[j]] <- data.frame(
        target_group_id = target$target_group_id,
        post = target$post,
        target_class = target$target_class,
        member1_id = target$member1_id,
        member2_id = target$member2_id,
        donor_group_id = donor$target_group_id,
        donor_class = donor$target_class,
        is_own = as.integer(donor$target_group_id == target$target_group_id),
        same_class = as.integer(donor$target_class == target$target_class),
        cost1 = c1,
        cost2 = c2,
        cost12 = c12,
        Ihat1_donor = ih1,
        Ihat2_donor = ifelse(is.na(ih1), NA_real_, 1 - ih1),
        degenerate = as.integer(is.na(ih1)),
        stringsAsFactors = FALSE
      )
    }
    do.call(rbind, rows)
  }

  roster_full_parts <- split(panel, interaction(panel$group_id, panel$post, drop = TRUE))
  roster_full <- do.call(rbind, lapply(roster_full_parts, function(d) {
    ids <- sort(unique(d$id))
    data.frame(
      target_group_id = d$group_id[1], post = d$post[1], target_class = d$class[1],
      member1_id = ids[1], member2_id = ids[2], stringsAsFactors = FALSE
    )
  }))
  roster_full <- roster_full[order(roster_full$post, roster_full$target_group_id), , drop = FALSE]
  rownames(roster_full) <- NULL

  started <- Sys.time()
  chunk_starts <- seq.int(1L, nrow(roster), by = chunk_size)
  chunk_paths <- character(length(chunk_starts))
  for (k in seq_along(chunk_starts)) {
    first <- chunk_starts[k]
    last <- min(first + chunk_size - 1L, nrow(roster))
    chunk_path <- file.path(chunk_dir, sprintf("donor_matrix_chunk_%04d.csv", k - 1L))
    chunk_paths[k] <- chunk_path
    expected_rows <- 0L
    for (i in first:last) {
      n_wave <- sum(roster_full$post == roster$post[i])
      expected_rows <- expected_rows + if (!is.na(max_donors) && max_donors > 0L) {
        1L + min(max_donors, n_wave - 1L)
      } else n_wave
    }
    if (file.exists(chunk_path) && !overwrite) {
      saved_chunk <- tryCatch(
        read.csv(chunk_path, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) NULL
      )
      valid_chunk <- !is.null(saved_chunk) && nrow(saved_chunk) == expected_rows &&
        all(c("target_group_id", "post", "donor_group_id") %in% names(saved_chunk))
      if (valid_chunk) {
        message("[resume] ", basename(chunk_path))
        next
      }
      message("[rebuild] ", basename(chunk_path), " is not a complete full-wave donor chunk")
    }
    target_indices <- first:last
    target_results <- if (n_cores > 1L && .Platform$OS.type != "windows") {
      parallel::mclapply(
        target_indices,
        function(i) target_rows(roster[i, , drop = FALSE]),
        mc.cores = min(n_cores, length(target_indices)),
        mc.preschedule = TRUE
      )
    } else {
      lapply(target_indices, function(i) target_rows(roster[i, , drop = FALSE]))
    }
    result <- do.call(rbind, target_results)
    write.csv(result, chunk_path, row.names = FALSE, na = "")
    message(sprintf("[saved] %s: %s rows", basename(chunk_path), format(nrow(result), big.mark = ",")))
  }

  matrix <- do.call(rbind, lapply(chunk_paths, read.csv, stringsAsFactors = FALSE, check.names = FALSE))
  if (measure == "maxmpi" && cost_timeout > 0) {
    diagonal_rows <- which(matrix$is_own == 1L)
    panel_key <- paste(panel$group_id, panel$post, panel$id, sep = "|")
    key1 <- paste(matrix$target_group_id[diagonal_rows], matrix$post[diagonal_rows],
                  matrix$member1_id[diagonal_rows], sep = "|")
    key2 <- paste(matrix$target_group_id[diagonal_rows], matrix$post[diagonal_rows],
                  matrix$member2_id[diagonal_rows], sep = "|")
    matrix$Ihat1_donor[diagonal_rows] <- panel[[expected_index]][match(key1, panel_key)]
    matrix$Ihat2_donor[diagonal_rows] <- panel[[expected_index]][match(key2, panel_key)]
    matrix$degenerate[diagonal_rows] <- as.integer(is.na(matrix$Ihat1_donor[diagonal_rows]))
  }
  write.csv(matrix, matrix_path, row.names = FALSE, na = "")

  diagonal <- matrix[matrix$is_own == 1L, , drop = FALSE]
  check <- rbind(
    data.frame(group_id = diagonal$target_group_id, post = diagonal$post, id = diagonal$member1_id, calculated = diagonal$Ihat1_donor),
    data.frame(group_id = diagonal$target_group_id, post = diagonal$post, id = diagonal$member2_id, calculated = diagonal$Ihat2_donor)
  )
  expected <- panel[, c("group_id", "post", "id", expected_index)]
  names(expected)[4] <- "stored"
  check <- merge(check, expected, by = c("group_id", "post", "id"), all.x = TRUE, sort = FALSE)
  if (any(is.na(check$calculated) != is.na(check$stored))) stop("Diagonal missing-value patterns do not match ", expected_index, ".")
  difference <- abs(check$calculated - check$stored)
  max_error <- if (all(is.na(difference))) NA_real_ else max(difference, na.rm = TRUE)
  if (!is.na(max_error) && max_error > 1e-10) stop("Diagonal does not reproduce ", expected_index, ": max error = ", max_error)

  message("Diagonal validation passed; max error = ", format(max_error, scientific = TRUE))
  message("Donor matrix saved: ", matrix_path)

  pool_summary <- function(x, prefix) {
    valid <- !is.na(x)
    values <- c(
      mean(replace(x, !valid, 0.5)),
      if (any(valid)) mean(x[valid]) else NA_real_,
      length(x), sum(valid), mean(!valid)
    )
    names(values) <- paste0(c("M_", "M_", "n_", "nvalid_", "degfrac_"),
                            prefix, c("_imp", "_drop", "", "", ""))
    values
  }

  blocks <- split(matrix, paste(matrix$target_group_id, matrix$post, sep = "|"))
  analysis_rows <- vector("list", length(blocks) * 2L)
  z <- 0L
  for (block in blocks) {
    own <- block[block$is_own == 1L, , drop = FALSE]
    donors <- block[block$is_own == 0L, , drop = FALSE]
    if (nrow(own) != 1L) stop("Expected one diagonal row per target pair-wave.")
    for (member in 1:2) {
      z <- z + 1L
      other <- 3L - member
      all_I <- donors[[paste0("Ihat", member, "_donor")]]
      cls_I <- all_I[donors$same_class == 1L]
      own_cost <- own[[paste0("cost", member)]]
      all_cost <- donors[[paste0("cost", member)]]
      cls_cost <- all_cost[donors$same_class == 1L]
      row <- c(
        group_id = own$target_group_id,
        post = own$post,
        class = own$target_class,
        id = own[[paste0("member", member, "_id")]],
        partner_id = own[[paste0("member", other, "_id")]],
        cost_own = own_cost,
        costN_own = own$cost12,
        I_actual = own[[paste0("Ihat", member, "_donor")]],
        pool_summary(all_I, "all"),
        pool_summary(cls_I, "cls"),
        P_all = mean(own_cost <= all_cost),
        P_cls = mean(own_cost <= cls_cost)
      )
      analysis_rows[[z]] <- as.list(row)
    }
  }
  analysis <- as.data.frame(do.call(rbind, lapply(analysis_rows, as.data.frame)),
                            stringsAsFactors = FALSE)
  numeric_columns <- setdiff(names(analysis), c("group_id", "class", "id", "partner_id"))
  analysis[numeric_columns] <- lapply(analysis[numeric_columns], as.numeric)

  for (pool in c("all", "cls")) {
    partner <- analysis[, c("group_id", "post", "id", paste0("P_", pool))]
    names(partner)[3:4] <- c("partner_id", paste0("P_partner_", pool))
    analysis <- merge(analysis, partner, by = c("group_id", "post", "partner_id"),
                      all.x = TRUE, sort = FALSE)
    analysis[[paste0("Pdiff_", pool)]] <- analysis[[paste0("P_", pool)]] -
      analysis[[paste0("P_partner_", pool)]]
    for (rule in c("imp", "drop")) {
      analysis[[paste0("Istar_", pool, "_", rule)]] <- analysis$I_actual -
        analysis[[paste0("M_", pool, "_", rule)]]
    }
  }

  analysis_csv <- file.path(output_dir, "placebo_normalized_member_wave.csv")
  write.csv(analysis, analysis_csv, row.names = FALSE, na = "")
  stata <- analysis
  names(stata)[names(stata) == "class"] <- "_class"
  write_dta(stata, analysis_dta, version = 14)
  message("Normalized member-wave data saved: ", analysis_dta)
  message("Total elapsed time: ", format(Sys.time() - started))
  invisible(matrix)
}
