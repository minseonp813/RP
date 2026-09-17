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
# Create group IDs and keep complete pairs.
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
# Build the balanced pair panel.
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






# Calculate the three rationality measures and Cross indices.

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
  "^(ccei_|maxmpi_|hm_|cei_|Ihat_|c_Ng_|c_maxmpi_|c_hm_|c_ccei_|RA_|high_|High)",
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

# Calculate CEI.
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

write_dta(panel_final, "data/panel_final.dta")
