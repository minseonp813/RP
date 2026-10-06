# 1_1_calculate_indices.R
# Latest update: 2026-10-06
# Purpose: Clean choices and calculate individual/group rationality, cross-choice
#          distances, risk aversion and non-own donor benchmarks M.
# Inputs: data/riskpreference_pre.dta and data/riskpreference_post.dta;
#         programs/calculate_rp_indices.R, calculate_cei.py and the shared
#         programs/build_placebo_donor_matrices.R.
# Outputs: data/base_raw.dta, data/end_raw.dta and data/panel_final.dta
#          (one balanced pair per row, members and waves in separate columns);
#          results/benchmarks/<measure>/ donor matrices, resumable chunks and
#          member-wave benchmarks for ccei, hm, maxmpi and ra.
# Sections:
#   1. Set up packages and load the two choice waves.
#   2. Identify pairs and retain complete individual/collective choice records.
#   3. Select the balanced pair roster, apply exclusions and check the sample.
#   4. Calculate CCEI, HM, MaxMPI, own-pair distances and risk aversion.
#   5. Validate cross costs, member distances and exact MaxMPI completion.
#   6. Calculate collective CEIV/CEI and save the actual-index checkpoint.
#   7. Build all four M benchmarks from choices and map results by member ID.
#   8. Save the wide index and benchmark panel for 2_1_build_panel.R.
# Benchmark section 7 uses every non-own pair in the same wave (651 donors in
# the current sample), without a solver time cap. It can be costly; completed
# results/chunks are reused only when inputs and calculation settings match.
# Pre-integration scripts and panels are preserved locally in
# Archive/M_before_integration_2026-10-06/. Existing placebo/review outputs stay
# in their original folders for comparison with results/benchmarks/.

# ----------------------------------------------------------------------------
# 1. Setup and choice inputs
# ----------------------------------------------------------------------------
rm(list = ls())
library(readxl)
library(tidyverse)
library(dplyr)
library(haven)
script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", grep("^--file=", script_args, value = TRUE))
if (length(script_file) == 1) setwd(dirname(normalizePath(script_file)))
base_raw <- read_dta("data/riskpreference_pre.dta")
end_raw <- read_dta("data/riskpreference_post.dta")
base_raw <- base_raw %>%
  mutate(partner_id = partner)
end_raw <- end_raw %>%
  mutate(partner_id = partner)
keep_cols <- c(
  "id",
  "partner_id",
  "mover",
  "t",
  "coord_x",
  "coord_y",
  "intercept_x",
  "intercept_y",
  "round_number",
  "game_type"
)
base_raw <- base_raw %>%
  select(all_of(keep_cols))
end_raw <- end_raw %>%
  select(all_of(keep_cols))
# ----------------------------------------------------------------------------
# 2. Clean pair identifiers and complete choices
# ----------------------------------------------------------------------------
clean_pair_raw <- function(df) {
  df <- df %>%
    mutate(
      id = as.character(id),
      partner_id = as.character(partner_id),
      round_number = as.integer(round_number),
      mover = as.integer(mover)
    )
  df <- df %>%
    group_by(id) %>%
    mutate(
      partner_id = partner_id[round_number == 19][1],
      mover = mover[round_number == 19][1]
    ) %>%
    ungroup()
  df <- df %>%
    mutate(
      big_id = pmax(id, partner_id, na.rm = TRUE),
      small_id = pmin(id, partner_id, na.rm = TRUE),
      group_id = ifelse(
        !is.na(partner_id) & id != partner_id,
        paste0(big_id, small_id),
        NA_character_
      )
    )
  pair_map <- df %>%
    filter(
      round_number == 1,
      !is.na(group_id),
      nchar(group_id) == 14
    ) %>%
    distinct(
      big_id,
      small_id,
      group_id
    )
  df <- df %>%
    select(
      id,
      partner_id,
      mover,
      t,
      coord_x,
      coord_y,
      intercept_x,
      intercept_y,
      round_number,
      game_type,
      group_id
    ) %>%
    rename(old_group_id = group_id)
  df <- df %>%
    left_join(
      pair_map,
      by = c("id" = "big_id")
    ) %>%
    mutate(
      new_group_from_big = group_id,
      partner_from_big = small_id
    ) %>%
    select(-group_id, -small_id)
  df <- df %>%
    left_join(
      pair_map,
      by = c("id" = "small_id")
    ) %>%
    mutate(
      new_group_from_small = group_id,
      partner_from_small = big_id
    ) %>%
    select(-group_id, -big_id)
  df <- df %>%
    mutate(
      group_id = coalesce(new_group_from_big, new_group_from_small),
      partner_id = coalesce(partner_from_big, partner_from_small)
    ) %>%
    select(
      -new_group_from_big,
      -new_group_from_small,
      -partner_from_big,
      -partner_from_small
    )
  df <- df %>%
    filter(!is.na(group_id)) %>%
    select(-old_group_id) %>%
    arrange(id, round_number)
  good_groups <- df %>%
    count(group_id, id, name = "n_rows") %>%
    group_by(group_id) %>%
    summarise(
      n_members = n_distinct(id),
      both_have_36 = all(n_rows == 36),
      .groups = "drop"
    ) %>%
    filter(
      n_members == 2,
      both_have_36
    ) %>%
    pull(group_id)
  df <- df %>%
    filter(group_id %in% good_groups) %>%
    arrange(group_id, id, round_number)
  return(df)
}
base_raw <- clean_pair_raw(base_raw)
end_raw <- clean_pair_raw(end_raw)
# Save cleaned choice data.
write_dta(
  base_raw,
  "data/base_raw.dta"
)
write_dta(
  end_raw,
  "data/end_raw.dta"
)
# ----------------------------------------------------------------------------
# 3. Balanced pair roster, exclusions and sample checks
# ----------------------------------------------------------------------------
base_pair <- base_raw %>%
  filter(round_number == 1, mover == 1) %>%
  select(group_id, id_mover = id, partner_id) %>%
  rename(id_nonmover = partner_id)
end_pair <- end_raw %>%
  filter(round_number == 1, mover == 1) %>%
  select(group_id, id_mover = id, partner_id) %>%
  rename(id_nonmover = partner_id)
panel_final <- base_pair %>%
  inner_join(
    end_pair,
    by = "group_id",
    suffix = c("_base", "_end")
  ) %>%
  arrange(group_id)
# Drop problematic groups.
drop_groups <- c(
  "11106161110601",
  "21204152120413",
  "21204162120405",
  "21204212120411",
  "16106101610601",
  "24101122410102",
  "24102242410208",
  "24102252410217",
  "26104032610401"
)
panel_final <- panel_final %>%
  filter(!group_id %in% drop_groups) %>%
  arrange(group_id)
# Save the pair panel.
write_dta(
  panel_final,
  "data/panel_final.dta"
)


# Check the sample.
panel_final %>%
  mutate(same_mover = id_mover_base == id_mover_end) %>%
  count(same_mover)
nrow(panel_final)
base_raw %>%
  filter(group_id %in% panel_final$group_id) %>%
  count(group_id, id, name = "n_rows") %>%
  count(n_rows)
end_raw %>%
  filter(group_id %in% panel_final$group_id) %>%
  count(group_id, id, name = "n_rows") %>%
  count(n_rows)






# ----------------------------------------------------------------------------
# 4. Individual/group indices, own-pair distances and risk aversion
# ----------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(dplyr)
library(haven)

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", grep("^--file=", script_args, value = TRUE))
if (length(script_file) == 1) setwd(dirname(normalizePath(script_file)))

if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("The igraph package is required. Install it with install.packages('igraph').")
}
source("programs/calculate_rp_indices.R")

base_raw <- rp_load_wave("data/base_raw.dta")
end_raw <- rp_load_wave("data/end_raw.dta")
panel_final <- read_dta("data/panel_final.dta") %>%
  mutate(
    group_id = as.character(group_id),
    id_mover_base = as.character(id_mover_base),
    id_nonmover_base = as.character(id_nonmover_base),
    id_mover_end = as.character(id_mover_end),
    id_nonmover_end = as.character(id_nonmover_end)
  )

old_generated_vars <- grep(
  "^(ccei_|maxmpi_|hm_|cei_|Ihat_|M_|n_(ccei|hm|maxmpi|ra)_|nvalid_(ccei|hm|maxmpi|ra)_|degfrac_(ccei|hm|maxmpi|ra)_|c_Ng_|c_maxmpi_|c_hm_|c_ccei_|RA_|high_|High)",
  names(panel_final),
  value = TRUE
)
if (length(old_generated_vars) > 0) {
  panel_final <- panel_final %>% select(-all_of(old_generated_vars))
}

pairs <- panel_final %>%
  select(
    group_id,
    id_mover_base,
    id_nonmover_base,
    id_mover_end,
    id_nonmover_end
  )

measure_results <- lapply(c("ccei", "maxmpi", "hm"), function(measure) {
  out <- rp_compute_measure(measure, base_raw, end_raw, pairs)
  out$I_mover <- rp_index(out$c_mover, out$c_nonmover, out$c_N)
  out
})
names(measure_results) <- c("ccei", "maxmpi", "hm")

add_measure_to_panel <- function(panel, result, measure) {
  for (wave in c("base", "end")) {
    block <- result[result$wave == wave, ]
    block <- block[match(panel$group_id, block$group_id), ]
    if (!identical(as.character(block$group_id), as.character(panel$group_id))) {
      stop(measure, " ", wave, ": group matching failed.")
    }

    if (measure == "ccei") {
      panel[[paste0("ccei_1_", wave)]] <- block$score_1
      panel[[paste0("ccei_2_", wave)]] <- block$score_2
      panel[[paste0("ccei_g_", wave)]] <- block$score_g
      panel[[paste0("ccei_1g_", wave)]] <- block$score_1g
      panel[[paste0("ccei_2g_", wave)]] <- block$score_2g
      panel[[paste0("c_ccei_1g_", wave)]] <- block$c_mover
      panel[[paste0("c_ccei_2g_", wave)]] <- block$c_nonmover
      panel[[paste0("c_Ng_", wave)]] <- block$c_N
      panel[[paste0("Ihat_1g_", wave)]] <- block$I_mover
      panel[[paste0("Ihat_2g_", wave)]] <- 1 - block$I_mover
    } else {
      panel[[paste0(measure, "_1_", wave)]] <- block$score_1
      panel[[paste0(measure, "_2_", wave)]] <- block$score_2
      panel[[paste0(measure, "_g_", wave)]] <- block$score_g
      panel[[paste0(measure, "_1g_", wave)]] <- block$score_1g
      panel[[paste0(measure, "_2g_", wave)]] <- block$score_2g
      panel[[paste0("c_", measure, "_1g_", wave)]] <- block$c_mover
      panel[[paste0("c_", measure, "_2g_", wave)]] <- block$c_nonmover
      panel[[paste0("c_", measure, "_Ng_", wave)]] <- block$c_N
      panel[[paste0("Ihat_", measure, "_1g_", wave)]] <- block$I_mover
      panel[[paste0("Ihat_", measure, "_2g_", wave)]] <- 1 - block$I_mover
    }

    panel[[paste0("RA_1_", wave)]] <- block$ra_1
    panel[[paste0("RA_2_", wave)]] <- block$ra_2
    panel[[paste0("RA_g_", wave)]] <- block$ra_g
  }
  panel
}

for (measure in names(measure_results)) {
  panel_final <- add_measure_to_panel(
    panel_final,
    measure_results[[measure]],
    measure
  )
}

# ----------------------------------------------------------------------------
# 5. Validate actual cross-choice indices
# ----------------------------------------------------------------------------
for (measure in c("ccei", "maxmpi", "hm")) {
  for (wave in c("base", "end")) {
    if (measure == "ccei") {
      i1 <- panel_final[[paste0("Ihat_1g_", wave)]]
      i2 <- panel_final[[paste0("Ihat_2g_", wave)]]
      total_cost <- panel_final[[paste0("c_Ng_", wave)]]
      c1 <- panel_final[[paste0("c_ccei_1g_", wave)]]
      c2 <- panel_final[[paste0("c_ccei_2g_", wave)]]
    } else {
      i1 <- panel_final[[paste0("Ihat_", measure, "_1g_", wave)]]
      i2 <- panel_final[[paste0("Ihat_", measure, "_2g_", wave)]]
      total_cost <- panel_final[[paste0("c_", measure, "_Ng_", wave)]]
      c1 <- panel_final[[paste0("c_", measure, "_1g_", wave)]]
      c2 <- panel_final[[paste0("c_", measure, "_2g_", wave)]]
    }

    defined <- !is.na(i1) & !is.na(i2)
    stopifnot(
      all(c1 <= total_cost + 1e-9),
      all(c2 <= total_cost + 1e-9),
      all(i1[defined] >= -1e-9 & i1[defined] <= 1 + 1e-9),
      all(i2[defined] >= -1e-9 & i2[defined] <= 1 + 1e-9),
      all(abs(i1[defined] + i2[defined] - 1) < 1e-9)
    )
  }
}

if (!all(measure_results$maxmpi$exhausted)) {
  stop("MaxMPI search did not exhaust every branch-and-bound problem.")
}

# ----------------------------------------------------------------------------
# 6. Collective CEIV and untempered CEI
# ----------------------------------------------------------------------------
python_candidates <- unique(c(
  Sys.getenv("CEI_PYTHON", unset = ""),
  unname(Sys.which(c("python", "python3"))),
  file.path(Sys.getenv("USERPROFILE", unset = ""), "anaconda3", "python.exe"),
  file.path(Sys.getenv("USERPROFILE", unset = ""), "miniconda3", "python.exe")
))
python_candidates <- python_candidates[
  nzchar(python_candidates) & file.exists(python_candidates)
]
if (length(python_candidates) == 0) {
  stop("Python was not found. Set CEI_PYTHON to the Anaconda python.exe path.")
}

cei_temp <- tempfile(fileext = ".csv")
cei_status <- system2(
  python_candidates[1],
  c(
    "programs/calculate_cei.py",
    "--root", shQuote(normalizePath(".", winslash = "/")),
    "--output", shQuote(normalizePath(cei_temp, winslash = "/", mustWork = FALSE))
  )
)
if (cei_status != 0 || !file.exists(cei_temp)) {
  stop("CEI calculation failed.")
}

cei_long <- readr::read_csv(
  cei_temp,
  col_types = readr::cols(
    group_id = readr::col_character(),
    wave = readr::col_character(),
    .default = readr::col_double()
  ),
  show_col_types = FALSE
)
unlink(cei_temp)

stopifnot(
  nrow(cei_long) == 2 * nrow(panel_final),
  !anyDuplicated(cei_long[c("group_id", "wave")]),
  all(cei_long$wave %in% c("base", "end")),
  all(cei_long$cei_g > 0 & cei_long$cei_g <= 1),
  all(cei_long$cei_g_untempered > 0 & cei_long$cei_g_untempered <= 1)
)

cei_wide <- cei_long %>%
  pivot_wider(
    id_cols = group_id,
    names_from = wave,
    values_from = c(
      cei_g,
      cei_g_untempered,
      cei_n_viol,
      cei_n_viol_untempered
    ),
    names_glue = "{.value}_{wave}"
  )

panel_final <- panel_final %>%
  left_join(cei_wide, by = "group_id")

if (any(is.na(panel_final$cei_g_base)) || any(is.na(panel_final$cei_g_end))) {
  stop("CEI group matching failed.")
}

# Save actual indices before the expensive M calculation so section 7 can resume.
write_dta(panel_final, "data/panel_final.dta")

# ----------------------------------------------------------------------------
# 7. Non-own donor benchmarks M
# ----------------------------------------------------------------------------
# This section can be run separately from Code after the checkpoint above exists.
source("programs/calculate_rp_indices.R")
source("programs/build_placebo_donor_matrices.R")
package_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
panel_final <- haven::read_dta("data/panel_final.dta")
base_raw <- rp_load_wave("data/base_raw.dta")
end_raw <- rp_load_wave("data/end_raw.dta")
benchmark_cores <- as.integer(Sys.getenv("PLACEBO_CORES", "1"))

for (measure in c("ccei", "hm", "maxmpi", "ra")) {
  benchmark <- build_placebo_donor_matrix(
    measure, package_dir, pairs = panel_final, base = base_raw, end = end_raw,
    output_dir = file.path(package_dir, "results", "benchmarks", measure),
    chunk_size = 8L, cores = benchmark_cores,
    max_targets = 0L, max_donors = 0L, cost_timeout = 0
  )
  stopifnot(
    nrow(benchmark) == 4L * nrow(panel_final),
    !anyDuplicated(benchmark[c("group_id", "post", "id")]),
    all(benchmark$n_all == nrow(panel_final) - 1L)
  )

  # Keep the primary benchmark, missing-donor alternative and pool diagnostics.
  columns <- c(
    M_all_imp = paste0("M_", measure),
    M_all_drop = paste0("M_", measure, "_drop"),
    M_cls_imp = paste0("M_", measure, "_sameclass"),
    M_cls_drop = paste0("M_", measure, "_sameclass_drop"),
    M_outclass_imp = paste0("M_", measure, "_outclass"),
    M_outclass_drop = paste0("M_", measure, "_outclass_drop"),
    n_all = paste0("n_", measure, "_donors"),
    nvalid_all = paste0("nvalid_", measure, "_donors"),
    degfrac_all = paste0("degfrac_", measure)
  )
  stopifnot(all(names(columns) %in% names(benchmark)))
  benchmark_key <- paste(benchmark$group_id, benchmark$post, benchmark$id, sep = "|")

  # Builder order is sorted ID; panel roles are mover/nonmover within each wave.
  for (wave in c("base", "end")) {
    post <- as.integer(wave == "end")
    for (member in 1:2) {
      role <- if (member == 1L) "mover" else "nonmover"
      id <- panel_final[[paste0("id_", role, "_", wave)]]
      rows <- match(paste(panel_final$group_id, post, id, sep = "|"), benchmark_key)
      stopifnot(!anyNA(rows))
      for (field in names(columns)) {
        panel_final[[paste0(columns[[field]], "_", member, "_", wave)]] <-
          benchmark[[field]][rows]
      }
      if (measure == "ra") {
        panel_final[[paste0("Ihat_ra_", member, "g_", wave)]] <- benchmark$I_actual[rows]
      }
    }
    stopifnot(all(abs(
      panel_final[[paste0("M_", measure, "_1_", wave)]] +
        panel_final[[paste0("M_", measure, "_2_", wave)]] - 1
    ) < 1e-7))
  }
}

# ----------------------------------------------------------------------------
# 8. Save the index and benchmark panel
# ----------------------------------------------------------------------------
write_dta(panel_final, "data/panel_final.dta")
