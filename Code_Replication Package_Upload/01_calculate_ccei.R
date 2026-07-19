# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("rstudioapi")
# install.packages("haven")
rm(list = ls())
library(rstudioapi)
library(readxl)
library(tidyverse)
library(dplyr)
library(haven)
setwd(dirname(getSourceEditorContext()$path))
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
############################################################
# Create group_id and keep only valid complete pair data
############################################################
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
############################################################
# Save cleaned baseline/endline round-level data as dta
############################################################
write_dta(
  base_raw,
  "data/base_raw.dta"
)
write_dta(
  end_raw,
  "data/end_raw.dta"
)
############################################################
# Build balanced pair-level panel
############################################################
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
############################################################
# Drop budget-violating groups and groups excluded in old pipeline
############################################################
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
############################################################
# Save balanced pair-level panel as dta
############################################################
write_dta(
  panel_final,
  "data/panel_final.dta"
)
############################################################
# Final checks
############################################################
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






################################################################
################################################################
################################################################
################################################################
################################################################



############################################################
# Compute GARP CCEI, FGARP CCEI, RevMaxMPI, I indices, and RA
# Input:
#   data/base_raw.dta
#   data/end_raw.dta
#   data/panel_final.dta
# Output:
#   data/panel_final.dta
############################################################

# install.packages("tidyverse")
# install.packages("rstudioapi")
# install.packages("haven")
# install.packages("revpref")

rm(list = ls())

library(rstudioapi)
library(tidyverse)
library(dplyr)
library(haven)
library(revpref)

setwd(dirname(getSourceEditorContext()$path))

source("programs/ccei_garp.R")
source("programs/garp.R")
source("programs/ccei_fgarp.R")
source("programs/fgarp.R")
source("programs/warshall.R")

base_raw <- read_dta("data/base_raw.dta")
end_raw <- read_dta("data/end_raw.dta")
panel_final <- read_dta("data/panel_final.dta")

base_raw <- base_raw %>%
  mutate(
    id = as.character(id),
    partner_id = as.character(partner_id),
    group_id = as.character(group_id),
    round_number = as.integer(round_number),
    mover = as.integer(mover),
    coord_x = as.numeric(coord_x),
    coord_y = as.numeric(coord_y),
    intercept_x = as.numeric(intercept_x),
    intercept_y = as.numeric(intercept_y)
  )

end_raw <- end_raw %>%
  mutate(
    id = as.character(id),
    partner_id = as.character(partner_id),
    group_id = as.character(group_id),
    round_number = as.integer(round_number),
    mover = as.integer(mover),
    coord_x = as.numeric(coord_x),
    coord_y = as.numeric(coord_y),
    intercept_x = as.numeric(intercept_x),
    intercept_y = as.numeric(intercept_y)
  )

panel_final <- panel_final %>%
  mutate(
    group_id = as.character(group_id),
    id_mover_base = as.character(id_mover_base),
    id_nonmover_base = as.character(id_nonmover_base),
    id_mover_end = as.character(id_mover_end),
    id_nonmover_end = as.character(id_nonmover_end)
  )

old_generated_vars <- grep(
  "^(ccei_|I_hg_|I_lg_|RA_|high_|f_ccei_|f_I_|f_high_|max_mpi_|rev_max_mpi_|I_rev_max_mpi_|I_max_mpi_)",
  names(panel_final),
  value = TRUE
)

if (length(old_generated_vars) > 0) {
  panel_final <- panel_final %>%
    select(-all_of(old_generated_vars))
}

fgarp_pi <- matrix(1 / 2, 2, 1)

safe_ratio <- function(num, den) {
  ifelse(
    is.na(num) | is.na(den) | abs(den) < 1e-10,
    NA_real_,
    num / den
  )
}

row_mean_na <- function(mat) {
  out <- rowMeans(mat, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

compute_ccei_from_subset <- function(sub) {
  
  if (nrow(sub) == 0) return(NA_real_)
  
  sub <- sub %>%
    filter(
      !is.na(coord_x),
      !is.na(coord_y),
      !is.na(intercept_x),
      !is.na(intercept_y),
      intercept_x != 0,
      intercept_y != 0
    )
  
  if (nrow(sub) == 0) return(NA_real_)
  
  p <- rbind(
    1 / sub$intercept_x,
    1 / sub$intercept_y
  )
  
  x <- rbind(
    sub$coord_x,
    sub$coord_y
  )
  
  ccei_garp(p, x)
}

compute_f_ccei_from_subset <- function(sub) {
  
  if (nrow(sub) == 0) return(NA_real_)
  
  sub <- sub %>%
    filter(
      !is.na(coord_x),
      !is.na(coord_y),
      !is.na(intercept_x),
      !is.na(intercept_y),
      intercept_x != 0,
      intercept_y != 0
    )
  
  if (nrow(sub) == 0) return(NA_real_)
  
  p <- rbind(
    1 / sub$intercept_x,
    1 / sub$intercept_y
  )
  
  x <- rbind(
    sub$coord_x,
    sub$coord_y
  )
  
  ccei_fgarp(p, x, fgarp_pi)
}

compute_max_mpi_from_subset <- function(sub) {
  
  if (nrow(sub) == 0) return(NA_real_)
  
  sub <- sub %>%
    filter(
      !is.na(coord_x),
      !is.na(coord_y),
      !is.na(intercept_x),
      !is.na(intercept_y),
      intercept_x != 0,
      intercept_y != 0
    )
  
  if (nrow(sub) == 0) return(NA_real_)
  
  p <- cbind(
    1 / sub$intercept_x,
    1 / sub$intercept_y
  )
  
  q <- cbind(
    sub$coord_x,
    sub$coord_y
  )
  
  out <- tryCatch(
    revpref::mpi(p, q),
    error = function(e) NA_real_
  )
  
  out_vec <- suppressWarnings(as.numeric(unlist(out)))
  
  if (length(out_vec) >= 2) {
    return(out_vec[2])
  } else {
    return(NA_real_)
  }
}

############################################################
# 1. GARP CCEI block
############################################################

compute_ccei_wave_measures <- function(raw, panel, suffix, id_mover_col, id_nonmover_col) {
  
  id1_col <- id_mover_col
  id2_col <- id_nonmover_col
  
  ccei_1_col <- paste0("ccei_1_", suffix)
  ccei_2_col <- paste0("ccei_2_", suffix)
  ccei_g_col <- paste0("ccei_g_", suffix)
  ccei_1g_col <- paste0("ccei_1g_", suffix)
  ccei_2g_col <- paste0("ccei_2g_", suffix)
  ccei_hg_col <- paste0("ccei_hg_", suffix)
  ccei_lg_col <- paste0("ccei_lg_", suffix)
  ccei_hlg_col <- paste0("ccei_hlg_", suffix)
  high_col <- paste0("high_", suffix)
  I_hg_col <- paste0("I_hg_", suffix)
  I_lg_col <- paste0("I_lg_", suffix)
  RA_g_col <- paste0("RA_g_", suffix)
  RA_1_col <- paste0("RA_1_", suffix)
  RA_2_col <- paste0("RA_2_", suffix)
  raw_high_col <- paste0("high_", suffix)
  
  panel[[ccei_1_col]] <- NA_real_
  panel[[ccei_2_col]] <- NA_real_
  
  raw_indiv <- raw %>%
    filter(round_number >= 1, round_number <= 18)
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1 <- raw_indiv[raw_indiv$id == id1, ]
    sub2 <- raw_indiv[raw_indiv$id == id2, ]
    
    panel[[ccei_1_col]][i] <- compute_ccei_from_subset(sub1)
    panel[[ccei_2_col]][i] <- compute_ccei_from_subset(sub2)
  }
  
  ccei_g_1_col <- paste0("ccei_g_1_", suffix)
  ccei_g_2_col <- paste0("ccei_g_2_", suffix)
  
  panel[[ccei_g_1_col]] <- NA_real_
  panel[[ccei_g_2_col]] <- NA_real_
  
  raw_group <- raw %>%
    filter(
      round_number >= 19,
      round_number <= 36,
      id != partner_id,
      !is.na(partner_id)
    )
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1 <- raw_group[raw_group$id == id1, ]
    sub2 <- raw_group[raw_group$id == id2, ]
    
    panel[[ccei_g_1_col]][i] <- compute_ccei_from_subset(sub1)
    panel[[ccei_g_2_col]][i] <- compute_ccei_from_subset(sub2)
  }
  
  cat("\n", suffix, ": GARP group CCEI mover/nonmover equality check\n")
  print(
    all.equal(
      panel[[ccei_g_1_col]],
      panel[[ccei_g_2_col]],
      tolerance = 1e-12
    )
  )
  
  panel[[ccei_g_col]] <- panel[[ccei_g_1_col]]
  
  panel[[ccei_g_1_col]] <- NULL
  panel[[ccei_g_2_col]] <- NULL
  
  panel[[ccei_1g_col]] <- NA_real_
  panel[[ccei_2g_col]] <- NA_real_
  
  raw_indiv_group <- raw %>%
    filter(
      round_number >= 1,
      round_number <= 36,
      id != partner_id
    )
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1 <- raw_indiv_group[raw_indiv_group$id == id1, ]
    sub2 <- raw_indiv_group[raw_indiv_group$id == id2, ]
    
    panel[[ccei_1g_col]][i] <- compute_ccei_from_subset(sub1)
    panel[[ccei_2g_col]][i] <- compute_ccei_from_subset(sub2)
  }
  
  raw_work <- raw %>%
    filter(id != partner_id)
  
  raw_work[[raw_high_col]] <- 0L
  panel[[high_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    c1 <- panel[[ccei_1_col]][i]
    c2 <- panel[[ccei_2_col]][i]
    c1g <- panel[[ccei_1g_col]][i]
    c2g <- panel[[ccei_2g_col]][i]
    
    high_id <- id1
    high_binary <- 1
    
    if (!is.na(c1) && !is.na(c2)) {
      if (c2 > c1) {
        high_id <- id2
        high_binary <- 0
      } else if (c1 == c2 && !is.na(c1g) && !is.na(c2g)) {
        if (c2g > c1g) {
          high_id <- id2
          high_binary <- 0
        }
      }
    }
    
    panel[[high_col]][i] <- high_binary
    raw_work[[raw_high_col]][raw_work$id == high_id] <- 1L
  }
  
  data_high <- raw_work[raw_work[[raw_high_col]] == 1, ]
  
  data_high <- data_high[
    (data_high$round_number >= 1 & data_high$round_number <= 18) |
      (data_high$round_number >= 19 & data_high$round_number <= 36),
  ]
  
  panel[[ccei_hg_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1 <- data_high[data_high$id == id1, ]
    sub2 <- data_high[data_high$id == id2, ]
    
    val1 <- compute_ccei_from_subset(sub1)
    val2 <- compute_ccei_from_subset(sub2)
    
    if (!is.na(val1)) {
      panel[[ccei_hg_col]][i] <- val1
    } else if (!is.na(val2)) {
      panel[[ccei_hg_col]][i] <- val2
    }
  }
  
  data_low <- raw_work[raw_work[[raw_high_col]] == 0, ]
  
  data_low <- data_low[
    (data_low$round_number >= 1 & data_low$round_number <= 18) |
      (data_low$round_number >= 19 & data_low$round_number <= 36),
  ]
  
  panel[[ccei_lg_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1 <- data_low[data_low$id == id1, ]
    sub2 <- data_low[data_low$id == id2, ]
    
    val1 <- compute_ccei_from_subset(sub1)
    val2 <- compute_ccei_from_subset(sub2)
    
    if (!is.na(val1)) {
      panel[[ccei_lg_col]][i] <- val1
    } else if (!is.na(val2)) {
      panel[[ccei_lg_col]][i] <- val2
    }
  }
  
  data_all <- raw[
    (raw$round_number >= 1 & raw$round_number <= 18) |
      (raw$round_number >= 19 & raw$round_number <= 36),
  ]
  
  data_all <- data_all[data_all$id != data_all$partner_id, ]
  
  data_all$group_id <- paste0(
    pmax(data_all$id, data_all$partner_id),
    pmin(data_all$id, data_all$partner_id)
  )
  
  data_indiv <- data_all[data_all$round_number <= 18, ]
  data_group <- data_all[data_all$round_number >= 19 & data_all$mover == 1, ]
  
  data_combined <- rbind(data_indiv, data_group)
  
  panel[[ccei_hlg_col]] <- NA_real_
  
  for (g in unique(panel$group_id)) {
    
    sub <- data_combined[data_combined$group_id == g, ]
    
    if (nrow(sub) == 0) next
    
    panel[[ccei_hlg_col]][panel$group_id == g] <- compute_ccei_from_subset(sub)
  }
  
  den <- panel[[ccei_g_col]] - panel[[ccei_hlg_col]]
  
  Ihg1 <- safe_ratio(panel[[ccei_g_col]] - panel[[ccei_hg_col]], den)
  Ilg1 <- 1 - Ihg1
  
  Ilg2 <- safe_ratio(panel[[ccei_g_col]] - panel[[ccei_lg_col]], den)
  Ihg2 <- 1 - Ilg2
  
  panel[[I_hg_col]] <- row_mean_na(cbind(Ihg1, Ihg2))
  panel[[I_lg_col]] <- row_mean_na(cbind(Ilg1, Ilg2))
  
  df_group_RA <- raw %>%
    filter(round_number >= 19, round_number <= 36) %>%
    mutate(
      x_expensive = ifelse(intercept_x < intercept_y, coord_x, coord_y),
      RA = ifelse(
        coord_x + coord_y == 0,
        NA_real_,
        x_expensive / (coord_x + coord_y)
      )
    ) %>%
    group_by(id) %>%
    summarise(RA = round(mean(RA, na.rm = TRUE), 5), .groups = "drop")
  
  panel[[RA_g_col]] <- df_group_RA$RA[match(panel[[id1_col]], df_group_RA$id)]
  
  df_indiv_RA <- raw %>%
    filter(round_number >= 1, round_number <= 18) %>%
    mutate(
      x_expensive = ifelse(intercept_x < intercept_y, coord_x, coord_y),
      RA = ifelse(
        coord_x + coord_y == 0,
        NA_real_,
        x_expensive / (coord_x + coord_y)
      )
    ) %>%
    group_by(id) %>%
    summarise(RA = round(mean(RA, na.rm = TRUE), 5), .groups = "drop")
  
  panel[[RA_1_col]] <- df_indiv_RA$RA[match(panel[[id1_col]], df_indiv_RA$id)]
  panel[[RA_2_col]] <- df_indiv_RA$RA[match(panel[[id2_col]], df_indiv_RA$id)]
  
  return(panel)
}

############################################################
# 2. FGARP CCEI block
############################################################

compute_fgarp_wave_measures <- function(raw, panel, suffix, id_mover_col, id_nonmover_col) {
  
  id1_col <- id_mover_col
  id2_col <- id_nonmover_col
  
  f1_col <- paste0("f_ccei_1_", suffix)
  f2_col <- paste0("f_ccei_2_", suffix)
  fg_col <- paste0("f_ccei_g_", suffix)
  f1g_col <- paste0("f_ccei_1g_", suffix)
  f2g_col <- paste0("f_ccei_2g_", suffix)
  fhg_col <- paste0("f_ccei_hg_", suffix)
  flg_col <- paste0("f_ccei_lg_", suffix)
  fhlg_col <- paste0("f_ccei_hlg_", suffix)
  fhigh_col <- paste0("f_high_", suffix)
  fI_hg_col <- paste0("f_I_hg_", suffix)
  fI_lg_col <- paste0("f_I_lg_", suffix)
  
  raw <- raw %>%
    mutate(
      id = as.character(id),
      partner_id = as.character(partner_id),
      mover = as.integer(mover),
      group_id = as.character(group_id)
    )
  
  panel[[f1_col]] <- NA_real_
  panel[[f2_col]] <- NA_real_
  
  raw_indiv <- raw %>%
    filter(round_number >= 1, round_number <= 18)
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1 <- raw_indiv[raw_indiv$id == id1, ]
    sub2 <- raw_indiv[raw_indiv$id == id2, ]
    
    panel[[f1_col]][i] <- compute_f_ccei_from_subset(sub1)
    panel[[f2_col]][i] <- compute_f_ccei_from_subset(sub2)
  }
  
  fg_1 <- rep(NA_real_, nrow(panel))
  fg_2 <- rep(NA_real_, nrow(panel))
  
  raw_group <- raw %>%
    filter(
      round_number >= 19,
      round_number <= 36,
      id != partner_id,
      !is.na(partner_id)
    )
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1 <- raw_group[raw_group$id == id1, ]
    sub2 <- raw_group[raw_group$id == id2, ]
    
    fg_1[i] <- compute_f_ccei_from_subset(sub1)
    fg_2[i] <- compute_f_ccei_from_subset(sub2)
  }
  
  cat("\n", suffix, ": FGARP group CCEI mover/nonmover equality check\n")
  print(
    all.equal(
      fg_1,
      fg_2,
      tolerance = 1e-12
    )
  )
  
  panel[[fg_col]] <- fg_1
  
  panel[[f1g_col]] <- NA_real_
  panel[[f2g_col]] <- NA_real_
  
  raw_indiv_group <- raw %>%
    filter(
      round_number >= 1,
      round_number <= 36,
      id != partner_id
    )
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1 <- raw_indiv_group[raw_indiv_group$id == id1, ]
    sub2 <- raw_indiv_group[raw_indiv_group$id == id2, ]
    
    panel[[f1g_col]][i] <- compute_f_ccei_from_subset(sub1)
    panel[[f2g_col]][i] <- compute_f_ccei_from_subset(sub2)
  }
  
  raw_work <- raw %>%
    filter(id != partner_id)
  
  raw_work$f_high_tmp <- 0L
  raw_work$f_low_tmp <- 0L
  
  panel[[fhigh_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    f1 <- panel[[f1_col]][i]
    f2 <- panel[[f2_col]][i]
    f1g <- panel[[f1g_col]][i]
    f2g <- panel[[f2g_col]][i]
    
    high_id <- id1
    low_id <- id2
    high_binary <- 1
    
    if (!is.na(f1) && !is.na(f2)) {
      if (f2 > f1) {
        high_id <- id2
        low_id <- id1
        high_binary <- 0
      } else if (f1 == f2 && !is.na(f1g) && !is.na(f2g)) {
        if (f2g > f1g) {
          high_id <- id2
          low_id <- id1
          high_binary <- 0
        }
      }
    }
    
    panel[[fhigh_col]][i] <- high_binary
    
    raw_work$f_high_tmp[raw_work$id == high_id] <- 1L
    raw_work$f_low_tmp[raw_work$id == low_id] <- 1L
  }
  
  data_high <- raw_work[raw_work$f_high_tmp == 1, ]
  
  data_high <- data_high[
    (data_high$round_number >= 1 & data_high$round_number <= 18) |
      (data_high$round_number >= 19 & data_high$round_number <= 36),
  ]
  
  panel[[fhg_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1 <- data_high[data_high$id == id1, ]
    sub2 <- data_high[data_high$id == id2, ]
    
    val1 <- compute_f_ccei_from_subset(sub1)
    val2 <- compute_f_ccei_from_subset(sub2)
    
    if (!is.na(val1)) {
      panel[[fhg_col]][i] <- val1
    } else if (!is.na(val2)) {
      panel[[fhg_col]][i] <- val2
    }
  }
  
  data_low <- raw_work[raw_work$f_low_tmp == 1, ]
  
  data_low <- data_low[
    (data_low$round_number >= 1 & data_low$round_number <= 18) |
      (data_low$round_number >= 19 & data_low$round_number <= 36),
  ]
  
  panel[[flg_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1 <- data_low[data_low$id == id1, ]
    sub2 <- data_low[data_low$id == id2, ]
    
    val1 <- compute_f_ccei_from_subset(sub1)
    val2 <- compute_f_ccei_from_subset(sub2)
    
    if (!is.na(val1)) {
      panel[[flg_col]][i] <- val1
    } else if (!is.na(val2)) {
      panel[[flg_col]][i] <- val2
    }
  }
  
  data_all <- raw[
    (raw$round_number >= 1 & raw$round_number <= 18) |
      (raw$round_number >= 19 & raw$round_number <= 36),
  ]
  
  data_all <- data_all[data_all$id != data_all$partner_id, ]
  
  data_all$group_id <- paste0(
    pmax(data_all$id, data_all$partner_id),
    pmin(data_all$id, data_all$partner_id)
  )
  
  data_indiv <- data_all[data_all$round_number <= 18, ]
  data_group <- data_all[data_all$round_number >= 19 & data_all$mover == 1, ]
  
  data_combined <- rbind(data_indiv, data_group)
  
  panel[[fhlg_col]] <- NA_real_
  
  for (g in unique(panel$group_id)) {
    
    sub <- data_combined[data_combined$group_id == g, ]
    
    if (nrow(sub) == 0) next
    
    panel[[fhlg_col]][panel$group_id == g] <- compute_f_ccei_from_subset(sub)
  }
  
  den <- panel[[fg_col]] - panel[[fhlg_col]]
  
  Ihg1 <- safe_ratio(panel[[fg_col]] - panel[[fhg_col]], den)
  Ilg1 <- 1 - Ihg1
  
  Ilg2 <- safe_ratio(panel[[fg_col]] - panel[[flg_col]], den)
  Ihg2 <- 1 - Ilg2
  
  panel[[fI_hg_col]] <- row_mean_na(cbind(Ihg1, Ihg2))
  panel[[fI_lg_col]] <- row_mean_na(cbind(Ilg1, Ilg2))
  
  return(panel)
}

############################################################
# 3. RevMaxMPI block
# max_mpi: lower is better
# rev_max_mpi = 1 - max_mpi: higher is better
# I index is computed using rev_max_mpi, same direction as CCEI
############################################################

compute_rev_max_mpi_wave_measures <- function(raw, panel, suffix, id_mover_col, id_nonmover_col) {
  
  id1_col <- id_mover_col
  id2_col <- id_nonmover_col
  
  max_1_col <- paste0("max_mpi_1_", suffix)
  max_2_col <- paste0("max_mpi_2_", suffix)
  max_g_col <- paste0("max_mpi_g_", suffix)
  max_1g_col <- paste0("max_mpi_1g_", suffix)
  max_2g_col <- paste0("max_mpi_2g_", suffix)
  max_hg_col <- paste0("max_mpi_hg_", suffix)
  max_lg_col <- paste0("max_mpi_lg_", suffix)
  max_hlg_col <- paste0("max_mpi_hlg_", suffix)
  
  rev_1_col <- paste0("rev_max_mpi_1_", suffix)
  rev_2_col <- paste0("rev_max_mpi_2_", suffix)
  rev_g_col <- paste0("rev_max_mpi_g_", suffix)
  rev_1g_col <- paste0("rev_max_mpi_1g_", suffix)
  rev_2g_col <- paste0("rev_max_mpi_2g_", suffix)
  rev_hg_col <- paste0("rev_max_mpi_hg_", suffix)
  rev_lg_col <- paste0("rev_max_mpi_lg_", suffix)
  rev_hlg_col <- paste0("rev_max_mpi_hlg_", suffix)
  rev_high_col <- paste0("rev_max_mpi_high_", suffix)
  
  I_rev_hg_col <- paste0("I_rev_max_mpi_hg_", suffix)
  I_rev_lg_col <- paste0("I_rev_max_mpi_lg_", suffix)
  
  raw <- raw %>%
    mutate(
      id = as.character(id),
      partner_id = as.character(partner_id),
      mover = as.integer(mover),
      group_id = as.character(group_id)
    )
  
  panel[[max_1_col]] <- NA_real_
  panel[[max_2_col]] <- NA_real_
  
  indiv_raw <- raw %>%
    filter(
      round_number >= 1,
      round_number <= 18,
      id != partner_id
    )
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1 <- indiv_raw[indiv_raw$id == id1, ]
    sub2 <- indiv_raw[indiv_raw$id == id2, ]
    
    panel[[max_1_col]][i] <- compute_max_mpi_from_subset(sub1)
    panel[[max_2_col]][i] <- compute_max_mpi_from_subset(sub2)
  }
  
  panel[[max_g_col]] <- NA_real_
  
  group_raw <- raw %>%
    filter(
      round_number >= 19,
      round_number <= 36,
      id != partner_id,
      !is.na(partner_id),
      partner_id != "0"
    )
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    
    sub_g <- group_raw[group_raw$id == id1, ]
    
    panel[[max_g_col]][i] <- compute_max_mpi_from_subset(sub_g)
  }
  
  panel[[max_1g_col]] <- NA_real_
  panel[[max_2g_col]] <- NA_real_
  
  full_raw <- raw %>%
    filter(
      round_number >= 1,
      round_number <= 36,
      id != partner_id
    )
  
  for (i in seq_len(nrow(panel))) {
    
    id1 <- panel[[id1_col]][i]
    id2 <- panel[[id2_col]][i]
    
    sub1g <- full_raw[full_raw$id == id1, ]
    sub2g <- full_raw[full_raw$id == id2, ]
    
    panel[[max_1g_col]][i] <- compute_max_mpi_from_subset(sub1g)
    panel[[max_2g_col]][i] <- compute_max_mpi_from_subset(sub2g)
  }
  
  data_all <- raw[
    (raw$round_number >= 1 & raw$round_number <= 18) |
      (raw$round_number >= 19 & raw$round_number <= 36),
  ]
  
  data_all <- data_all[data_all$id != data_all$partner_id, ]
  
  data_all$group_id <- paste0(
    pmax(data_all$id, data_all$partner_id),
    pmin(data_all$id, data_all$partner_id)
  )
  
  data_indiv <- data_all[data_all$round_number <= 18, ]
  data_group <- data_all[data_all$round_number >= 19 & data_all$mover == 1, ]
  
  data_hlg <- rbind(data_indiv, data_group)
  
  panel[[max_hlg_col]] <- NA_real_
  
  for (g in unique(panel$group_id)) {
    
    sub_hlg <- data_hlg[data_hlg$group_id == g, ]
    
    panel[[max_hlg_col]][panel$group_id == g] <- compute_max_mpi_from_subset(sub_hlg)
  }
  
  panel[[rev_1_col]] <- ifelse(
    is.na(panel[[max_1_col]]),
    NA_real_,
    1 - panel[[max_1_col]]
  )
  
  panel[[rev_2_col]] <- ifelse(
    is.na(panel[[max_2_col]]),
    NA_real_,
    1 - panel[[max_2_col]]
  )
  
  panel[[rev_g_col]] <- ifelse(
    is.na(panel[[max_g_col]]),
    NA_real_,
    1 - panel[[max_g_col]]
  )
  
  panel[[rev_1g_col]] <- ifelse(
    is.na(panel[[max_1g_col]]),
    NA_real_,
    1 - panel[[max_1g_col]]
  )
  
  panel[[rev_2g_col]] <- ifelse(
    is.na(panel[[max_2g_col]]),
    NA_real_,
    1 - panel[[max_2g_col]]
  )
  
  panel[[rev_hlg_col]] <- ifelse(
    is.na(panel[[max_hlg_col]]),
    NA_real_,
    1 - panel[[max_hlg_col]]
  )
  
  panel[[rev_high_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel))) {
    
    r1 <- panel[[rev_1_col]][i]
    r2 <- panel[[rev_2_col]][i]
    r1g <- panel[[rev_1g_col]][i]
    r2g <- panel[[rev_2g_col]][i]
    
    high_binary <- 1
    
    if (!is.na(r1) && !is.na(r2)) {
      if (r2 > r1) {
        high_binary <- 0
      } else if (r1 == r2 && !is.na(r1g) && !is.na(r2g)) {
        if (r2g > r1g) high_binary <- 0
      }
    }
    
    panel[[rev_high_col]][i] <- high_binary
  }
  
  panel[[max_hg_col]] <- ifelse(
    panel[[rev_high_col]] == 1,
    panel[[max_1g_col]],
    panel[[max_2g_col]]
  )
  
  panel[[max_lg_col]] <- ifelse(
    panel[[rev_high_col]] == 1,
    panel[[max_2g_col]],
    panel[[max_1g_col]]
  )
  
  panel[[rev_hg_col]] <- ifelse(
    panel[[rev_high_col]] == 1,
    panel[[rev_1g_col]],
    panel[[rev_2g_col]]
  )
  
  panel[[rev_lg_col]] <- ifelse(
    panel[[rev_high_col]] == 1,
    panel[[rev_2g_col]],
    panel[[rev_1g_col]]
  )
  
  den <- panel[[rev_g_col]] - panel[[rev_hlg_col]]
  
  Ihg1 <- safe_ratio(panel[[rev_g_col]] - panel[[rev_hg_col]], den)
  Ilg1 <- 1 - Ihg1
  
  Ilg2 <- safe_ratio(panel[[rev_g_col]] - panel[[rev_lg_col]], den)
  Ihg2 <- 1 - Ilg2
  
  panel[[I_rev_hg_col]] <- row_mean_na(cbind(Ihg1, Ihg2))
  panel[[I_rev_lg_col]] <- row_mean_na(cbind(Ilg1, Ilg2))
  
  return(panel)
}

############################################################
# Run all blocks: endline first, baseline second
############################################################

panel_final <- compute_ccei_wave_measures(
  raw = end_raw,
  panel = panel_final,
  suffix = "end",
  id_mover_col = "id_mover_end",
  id_nonmover_col = "id_nonmover_end"
)

panel_final <- compute_ccei_wave_measures(
  raw = base_raw,
  panel = panel_final,
  suffix = "base",
  id_mover_col = "id_mover_base",
  id_nonmover_col = "id_nonmover_base"
)

panel_final <- compute_fgarp_wave_measures(
  raw = end_raw,
  panel = panel_final,
  suffix = "end",
  id_mover_col = "id_mover_end",
  id_nonmover_col = "id_nonmover_end"
)

panel_final <- compute_fgarp_wave_measures(
  raw = base_raw,
  panel = panel_final,
  suffix = "base",
  id_mover_col = "id_mover_base",
  id_nonmover_col = "id_nonmover_base"
)

panel_final <- compute_rev_max_mpi_wave_measures(
  raw = end_raw,
  panel = panel_final,
  suffix = "end",
  id_mover_col = "id_mover_end",
  id_nonmover_col = "id_nonmover_end"
)

panel_final <- compute_rev_max_mpi_wave_measures(
  raw = base_raw,
  panel = panel_final,
  suffix = "base",
  id_mover_col = "id_mover_base",
  id_nonmover_col = "id_nonmover_base"
)

panel_final %>%
  select(
    group_id,
    ccei_1_base,
    ccei_2_base,
    ccei_g_base,
    ccei_1g_base,
    ccei_2g_base,
    ccei_hg_base,
    ccei_lg_base,
    ccei_hlg_base,
    I_hg_base,
    I_lg_base,
    RA_g_base,
    RA_1_base,
    RA_2_base,
    f_ccei_1_base,
    f_ccei_2_base,
    f_ccei_g_base,
    f_ccei_1g_base,
    f_ccei_2g_base,
    f_ccei_hg_base,
    f_ccei_lg_base,
    f_ccei_hlg_base,
    f_I_hg_base,
    f_I_lg_base,
    max_mpi_1_base,
    max_mpi_2_base,
    max_mpi_g_base,
    max_mpi_1g_base,
    max_mpi_2g_base,
    max_mpi_hg_base,
    max_mpi_lg_base,
    max_mpi_hlg_base,
    rev_max_mpi_1_base,
    rev_max_mpi_2_base,
    rev_max_mpi_g_base,
    rev_max_mpi_1g_base,
    rev_max_mpi_2g_base,
    rev_max_mpi_hg_base,
    rev_max_mpi_lg_base,
    rev_max_mpi_hlg_base,
    I_rev_max_mpi_hg_base,
    I_rev_max_mpi_lg_base,
    ccei_1_end,
    ccei_2_end,
    ccei_g_end,
    ccei_1g_end,
    ccei_2g_end,
    ccei_hg_end,
    ccei_lg_end,
    ccei_hlg_end,
    I_hg_end,
    I_lg_end,
    RA_g_end,
    RA_1_end,
    RA_2_end,
    f_ccei_1_end,
    f_ccei_2_end,
    f_ccei_g_end,
    f_ccei_1g_end,
    f_ccei_2g_end,
    f_ccei_hg_end,
    f_ccei_lg_end,
    f_ccei_hlg_end,
    f_I_hg_end,
    f_I_lg_end,
    max_mpi_1_end,
    max_mpi_2_end,
    max_mpi_g_end,
    max_mpi_1g_end,
    max_mpi_2g_end,
    max_mpi_hg_end,
    max_mpi_lg_end,
    max_mpi_hlg_end,
    rev_max_mpi_1_end,
    rev_max_mpi_2_end,
    rev_max_mpi_g_end,
    rev_max_mpi_1g_end,
    rev_max_mpi_2g_end,
    rev_max_mpi_hg_end,
    rev_max_mpi_lg_end,
    rev_max_mpi_hlg_end,
    I_rev_max_mpi_hg_end,
    I_rev_max_mpi_lg_end
  ) %>%
  head() %>%
  print()
write_dta(
  panel_final,
  "data/panel_final.dta"
)
