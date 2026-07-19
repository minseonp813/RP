## Byunghun Hahn, June 29
## Build Individual Panel and Run Final Checks

rm(list = ls())

library(rstudioapi)
library(tidyverse)
library(dplyr)
library(haven)
library(stringr)

# Avoid namespace conflicts from packages loaded earlier in the same R session.
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
arrange <- dplyr::arrange
rename <- dplyr::rename
transmute <- dplyr::transmute
summarise <- dplyr::summarise
count <- dplyr::count
distinct <- dplyr::distinct
left_join <- dplyr::left_join
bind_rows <- dplyr::bind_rows
case_when <- dplyr::case_when
row_number <- dplyr::row_number
all_of <- tidyselect::all_of


if (rstudioapi::isAvailable()) {
  context_path <- rstudioapi::getSourceEditorContext()$path
  if (!is.null(context_path) && !is.na(context_path) && context_path != "") {
    setwd(dirname(context_path))
  }
}

id_as_char <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_remove(x, "\\.0$")
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  return(x)
}

safe_pmax <- function(x, y) {
  out <- pmax(x, y, na.rm = TRUE)
  out[is.infinite(out)] <- NA_real_
  out
}

safe_pmin <- function(x, y) {
  out <- pmin(x, y, na.rm = TRUE)
  out[is.infinite(out)] <- NA_real_
  out
}

safe_absdiff <- function(x, y) {
  ifelse(is.na(x) | is.na(y), NA_real_, abs(x - y))
}

safe_diff <- function(x, y) {
  ifelse(is.na(x) | is.na(y), NA_real_, x - y)
}

value_or_na <- function(df, nm) {
  if (nm %in% names(df)) {
    df[[nm]]
  } else {
    rep(NA_real_, nrow(df))
  }
}

panel_final <- read_dta("data/panel_final.dta")
panel_group <- read_dta("data/panel_group.dta")
network_panel_clean <- read_dta("data/network_panel_clean.dta")

if (!"class" %in% names(panel_final)) {
  stop("panel_final must contain class. Run the updated merge script first.")
}

make_individual_core <- function(df, suffix, person_num) {
  endline_value <- ifelse(suffix == "end", 1L, 0L)
  id1_col <- paste0("id_mover_", suffix)
  id2_col <- paste0("id_nonmover_", suffix)
  
  if (person_num == 1) {
    my_id_col <- id1_col
    partner_id_col <- id2_col
    my_num <- 1
    partner_num <- 2
    mover_value <- 1L
  } else {
    my_id_col <- id2_col
    partner_id_col <- id1_col
    my_num <- 2
    partner_num <- 1
    mover_value <- 0L
  }
  
  high <- value_or_na(df, paste0("high_", suffix))
  f_high <- value_or_na(df, paste0("f_high_", suffix))
  min_high <- value_or_na(df, paste0("min_mpi_high_", suffix))
  max_high <- value_or_na(df, paste0("max_mpi_high_", suffix))
  
  high_i <- if (person_num == 1) high else 1 - high
  f_high_i <- if (person_num == 1) f_high else 1 - f_high
  min_high_i <- if (person_num == 1) min_high else 1 - min_high
  max_high_i <- if (person_num == 1) max_high else 1 - max_high
  
  tibble(
    group_id = id_as_char(df$group_id),
    class = id_as_char(df$class),
    endline = endline_value,
    post = endline_value,
    time = endline_value,
    person = person_num,
    id = id_as_char(df[[my_id_col]]),
    partner_id = id_as_char(df[[partner_id_col]]),
    mover = mover_value,
    ccei_i = value_or_na(df, paste0("ccei_", my_num, "_", suffix)),
    ccei_j = value_or_na(df, paste0("ccei_", partner_num, "_", suffix)),
    ccei_g = value_or_na(df, paste0("ccei_g_", suffix)),
    ccei_ig = value_or_na(df, paste0("ccei_", my_num, "g_", suffix)),
    ccei_jg = value_or_na(df, paste0("ccei_", partner_num, "g_", suffix)),
    ccei_hg = value_or_na(df, paste0("ccei_hg_", suffix)),
    ccei_lg = value_or_na(df, paste0("ccei_lg_", suffix)),
    ccei_hlg = value_or_na(df, paste0("ccei_hlg_", suffix)),
    HighCCEI = high_i,
    I_ig = ifelse(
      high_i == 1,
      value_or_na(df, paste0("I_hg_", suffix)),
      value_or_na(df, paste0("I_lg_", suffix))
    ),
    I_hg = value_or_na(df, paste0("I_hg_", suffix)),
    I_lg = value_or_na(df, paste0("I_lg_", suffix)),
    f_ccei_i = value_or_na(df, paste0("f_ccei_", my_num, "_", suffix)),
    f_ccei_j = value_or_na(df, paste0("f_ccei_", partner_num, "_", suffix)),
    f_ccei_g = value_or_na(df, paste0("f_ccei_g_", suffix)),
    f_ccei_ig = value_or_na(df, paste0("f_ccei_", my_num, "g_", suffix)),
    f_ccei_jg = value_or_na(df, paste0("f_ccei_", partner_num, "g_", suffix)),
    f_ccei_hg = value_or_na(df, paste0("f_ccei_hg_", suffix)),
    f_ccei_lg = value_or_na(df, paste0("f_ccei_lg_", suffix)),
    f_ccei_hlg = value_or_na(df, paste0("f_ccei_hlg_", suffix)),
    HighF_CCEI = f_high_i,
    f_I_ig = ifelse(
      f_high_i == 1,
      value_or_na(df, paste0("f_I_hg_", suffix)),
      value_or_na(df, paste0("f_I_lg_", suffix))
    ),
    f_I_hg = value_or_na(df, paste0("f_I_hg_", suffix)),
    f_I_lg = value_or_na(df, paste0("f_I_lg_", suffix)),
    RA_i = value_or_na(df, paste0("RA_", my_num, "_", suffix)),
    RA_j = value_or_na(df, paste0("RA_", partner_num, "_", suffix)),
    RA_g = value_or_na(df, paste0("RA_g_", suffix)),
    min_mpi_i = value_or_na(df, paste0("min_mpi_", my_num, "_", suffix)),
    min_mpi_j = value_or_na(df, paste0("min_mpi_", partner_num, "_", suffix)),
    min_mpi_g = value_or_na(df, paste0("min_mpi_g_", suffix)),
    min_mpi_ig = value_or_na(df, paste0("min_mpi_", my_num, "g_", suffix)),
    min_mpi_jg = value_or_na(df, paste0("min_mpi_", partner_num, "g_", suffix)),
    min_mpi_hg = value_or_na(df, paste0("min_mpi_hg_", suffix)),
    min_mpi_lg = value_or_na(df, paste0("min_mpi_lg_", suffix)),
    min_mpi_hlg = value_or_na(df, paste0("min_mpi_hlg_", suffix)),
    HighMinMPI = min_high_i,
    I_min_mpi_ig = ifelse(
      min_high_i == 1,
      value_or_na(df, paste0("I_min_mpi_hg_", suffix)),
      value_or_na(df, paste0("I_min_mpi_lg_", suffix))
    ),
    I_min_mpi_hg = value_or_na(df, paste0("I_min_mpi_hg_", suffix)),
    I_min_mpi_lg = value_or_na(df, paste0("I_min_mpi_lg_", suffix)),
    max_mpi_i = value_or_na(df, paste0("max_mpi_", my_num, "_", suffix)),
    max_mpi_j = value_or_na(df, paste0("max_mpi_", partner_num, "_", suffix)),
    max_mpi_g = value_or_na(df, paste0("max_mpi_g_", suffix)),
    max_mpi_ig = value_or_na(df, paste0("max_mpi_", my_num, "g_", suffix)),
    max_mpi_jg = value_or_na(df, paste0("max_mpi_", partner_num, "g_", suffix)),
    max_mpi_hg = value_or_na(df, paste0("max_mpi_hg_", suffix)),
    max_mpi_lg = value_or_na(df, paste0("max_mpi_lg_", suffix)),
    max_mpi_hlg = value_or_na(df, paste0("max_mpi_hlg_", suffix)),
    HighMaxMPI = max_high_i,
    I_max_mpi_ig = ifelse(
      max_high_i == 1,
      value_or_na(df, paste0("I_max_mpi_hg_", suffix)),
      value_or_na(df, paste0("I_max_mpi_lg_", suffix))
    ),
    I_max_mpi_hg = value_or_na(df, paste0("I_max_mpi_hg_", suffix)),
    I_max_mpi_lg = value_or_na(df, paste0("I_max_mpi_lg_", suffix)),
    rev_max_mpi_i = value_or_na(df, paste0("rev_max_mpi_", my_num, "_", suffix)),
    rev_max_mpi_j = value_or_na(df, paste0("rev_max_mpi_", partner_num, "_", suffix)),
    rev_max_mpi_g = value_or_na(df, paste0("rev_max_mpi_g_", suffix)),
    rev_max_mpi_ig = value_or_na(df, paste0("rev_max_mpi_", my_num, "g_", suffix)),
    rev_max_mpi_jg = value_or_na(df, paste0("rev_max_mpi_", partner_num, "g_", suffix)),
    rev_max_mpi_hg = value_or_na(df, paste0("rev_max_mpi_hg_", suffix)),
    rev_max_mpi_lg = value_or_na(df, paste0("rev_max_mpi_lg_", suffix)),
    rev_max_mpi_hlg = value_or_na(df, paste0("rev_max_mpi_hlg_", suffix)),
    HighRevMaxMPI = max_high_i,
    I_rev_max_mpi_ig = ifelse(
      max_high_i == 1,
      value_or_na(df, paste0("I_rev_max_mpi_hg_", suffix)),
      value_or_na(df, paste0("I_rev_max_mpi_lg_", suffix))
    ),
    I_rev_max_mpi_hg = value_or_na(df, paste0("I_rev_max_mpi_hg_", suffix)),
    I_rev_max_mpi_lg = value_or_na(df, paste0("I_rev_max_mpi_lg_", suffix)),
    friendship_i_to_j = value_or_na(df, paste0("friendship_", my_num, "_", suffix)),
    friendship_j_to_i = value_or_na(df, paste0("friendship_", partner_num, "_", suffix)),
    mutual_friendship = value_or_na(df, paste0("mutual_friendship_", suffix)),
    oneway_friendship = value_or_na(df, paste0("oneway_friendship_", suffix)),
    oneside_friendship = value_or_na(df, paste0("oneside_friendship_", suffix)),
    none_friendship = value_or_na(df, paste0("none_friendship_", suffix)),
    friendship = value_or_na(df, paste0("friendship_", suffix)),
    inclass_friendship_i_to_j = value_or_na(df, paste0("inclass_friendship_", my_num, "_", suffix)),
    inclass_friendship_j_to_i = value_or_na(df, paste0("inclass_friendship_", partner_num, "_", suffix)),
    inclass_mutual_friendship = value_or_na(df, paste0("inclass_mutual_friendship_", suffix)),
    inclass_oneway_friendship = value_or_na(df, paste0("inclass_oneway_friendship_", suffix)),
    inclass_oneside_friendship = value_or_na(df, paste0("inclass_oneside_friendship_", suffix)),
    inclass_none_friendship = value_or_na(df, paste0("inclass_none_friendship_", suffix)),
    inclass_friendship = value_or_na(df, paste0("inclass_friendship_", suffix))
  )
}

make_person_var_long <- function(df, suffix, person_num) {
  id1_col <- paste0("id_mover_", suffix)
  id2_col <- paste0("id_nonmover_", suffix)
  
  if (person_num == 1) {
    my_id_col <- id1_col
    my_num <- 1
    partner_num <- 2
  } else {
    my_id_col <- id2_col
    my_num <- 2
    partner_num <- 1
  }
  
  endline_value <- ifelse(suffix == "end", 1L, 0L)
  
  time_cols_my <- names(df)[str_detect(names(df), paste0("_", my_num, "_", suffix, "$"))]
  time_cols_partner <- names(df)[str_detect(names(df), paste0("_", partner_num, "_", suffix, "$"))]
  time_base_my <- str_replace(time_cols_my, paste0("_", my_num, "_", suffix, "$"), "")
  time_base_partner <- str_replace(time_cols_partner, paste0("_", partner_num, "_", suffix, "$"), "")
  time_common <- intersect(time_base_my, time_base_partner)
  
  invariant_cols_my <- names(df)[str_detect(names(df), paste0("_", my_num, "$"))]
  invariant_cols_partner <- names(df)[str_detect(names(df), paste0("_", partner_num, "$"))]
  invariant_base_my <- str_replace(invariant_cols_my, paste0("_", my_num, "$"), "")
  invariant_base_partner <- str_replace(invariant_cols_partner, paste0("_", partner_num, "$"), "")
  invariant_common <- intersect(invariant_base_my, invariant_base_partner)
  
  exclude_base_names <- c(
    "ccei",
    "f_ccei",
    "RA",
    "min_mpi",
    "max_mpi",
    "rev_max_mpi",
    "I_min_mpi_hg",
    "I_min_mpi_lg",
    "I_max_mpi_hg",
    "I_max_mpi_lg",
    "I_rev_max_mpi_hg",
    "I_rev_max_mpi_lg",
    "friendship",
    "inclass_friendship"
  )
  
  time_common <- setdiff(time_common, exclude_base_names)
  invariant_common <- setdiff(invariant_common, exclude_base_names)
  
  out <- tibble(
    group_id = id_as_char(df$group_id),
    endline = endline_value,
    post = endline_value,
    time = endline_value,
    person = person_num,
    id = id_as_char(df[[my_id_col]])
  )
  
  for (v in invariant_common) {
    my_col <- paste0(v, "_", my_num)
    partner_col <- paste0(v, "_", partner_num)
    out[[paste0(v, "_i")]] <- df[[my_col]]
    out[[paste0(v, "_j")]] <- df[[partner_col]]
  }
  
  for (v in time_common) {
    my_col <- paste0(v, "_", my_num, "_", suffix)
    partner_col <- paste0(v, "_", partner_num, "_", suffix)
    out[[paste0(v, "_i")]] <- df[[my_col]]
    out[[paste0(v, "_j")]] <- df[[partner_col]]
  }
  
  return(out)
}

panel_individual_core <- bind_rows(
  make_individual_core(panel_final, suffix = "base", person_num = 1),
  make_individual_core(panel_final, suffix = "base", person_num = 2),
  make_individual_core(panel_final, suffix = "end", person_num = 1),
  make_individual_core(panel_final, suffix = "end", person_num = 2)
)

panel_individual_personvars <- bind_rows(
  make_person_var_long(panel_final, suffix = "base", person_num = 1),
  make_person_var_long(panel_final, suffix = "base", person_num = 2),
  make_person_var_long(panel_final, suffix = "end", person_num = 1),
  make_person_var_long(panel_final, suffix = "end", person_num = 2)
)

panel_individual <- panel_individual_core %>%
  left_join(
    panel_individual_personvars,
    by = c("group_id", "endline", "post", "time", "person", "id")
  ) %>%
  arrange(group_id, endline, person)

class_check <- panel_individual %>%
  mutate(
    class_from_id = substr(id_as_char(id), 1, 5),
    class_from_partner = substr(id_as_char(partner_id), 1, 5)
  ) %>%
  filter(
    is.na(class) | class == "" |
      class != class_from_id |
      class != class_from_partner
  ) %>%
  select(group_id, endline, person, id, partner_id, class, class_from_id, class_from_partner)

if (nrow(class_check) > 0) {
  cat("\nClass mismatch in panel_individual:\n")
  print(class_check, n = Inf, width = Inf)
  stop("Class variable in panel_individual is inconsistent with id or partner_id.")
}

cat("\nClass variable carried into panel_individual.\n")
cat("Number of classes:", n_distinct(panel_individual$class), "\n")

############################################################
# Fill one-period time-invariant variables across baseline/endline rows
#
# Some variables are collected only in one period, but they are demographic
# or time-invariant controls. The old panel_individual used these variables
# in both baseline and endline rows. Therefore we carry the nonmissing value
# to the corresponding missing period before constructing diff/dist variables.
############################################################

fill_time_invariant_across_periods <- function(df, stems) {
  
  for (v in stems) {
    
    vi <- paste0(v, "_i")
    vj <- paste0(v, "_j")
    
    if (!all(c(vi, vj) %in% names(df))) {
      next
    }
    
    value_lookup <- bind_rows(
      df %>%
        select(id, value = all_of(vi)),
      df %>%
        select(id = partner_id, value = all_of(vj))
    ) %>%
      filter(!is.na(id), id != "", !is.na(value)) %>%
      group_by(id) %>%
      summarise(value_fill = first(value), .groups = "drop")
    
    if (nrow(value_lookup) == 0) {
      next
    }
    
    df <- df %>%
      left_join(value_lookup, by = "id") %>%
      mutate(
        !!vi := coalesce(.data[[vi]], value_fill)
      ) %>%
      select(-value_fill)
    
    df <- df %>%
      left_join(value_lookup, by = c("partner_id" = "id")) %>%
      mutate(
        !!vj := coalesce(.data[[vj]], value_fill)
      ) %>%
      select(-value_fill)
  }
  
  return(df)
}

time_invariant_one_period_vars <- c(
  "height",
  "weight",
  "brothers",
  "sisters",
  "birthorder",
  "fatherage",
  "motherage",
  "fathersch",
  "mothersch",
  "schchoice1_str",
  "schchoice2_str",
  "giftcard"
)

panel_individual <- fill_time_invariant_across_periods(
  df = panel_individual,
  stems = time_invariant_one_period_vars
)

cat("\nCheck one-period time-invariant variables after fill:\n")
panel_individual %>%
  summarise(
    male_i_n = ifelse("male_i" %in% names(panel_individual), sum(!is.na(male_i)), NA_integer_),
    height_i_n = ifelse("height_i" %in% names(panel_individual), sum(!is.na(height_i)), NA_integer_),
    weight_i_n = ifelse("weight_i" %in% names(panel_individual), sum(!is.na(weight_i)), NA_integer_),
    brothers_i_n = ifelse("brothers_i" %in% names(panel_individual), sum(!is.na(brothers_i)), NA_integer_),
    sisters_i_n = ifelse("sisters_i" %in% names(panel_individual), sum(!is.na(sisters_i)), NA_integer_),
    birthorder_i_n = ifelse("birthorder_i" %in% names(panel_individual), sum(!is.na(birthorder_i)), NA_integer_),
    giftcard_i_n = ifelse("giftcard_i" %in% names(panel_individual), sum(!is.na(giftcard_i)), NA_integer_)
  ) %>%
  print(width = Inf)

i_vars <- names(panel_individual)[str_ends(names(panel_individual), "_i")]

for (iv in i_vars) {
  base_v <- str_remove(iv, "_i$")
  jv <- paste0(base_v, "_j")
  diff_v <- paste0(base_v, "_diff")
  dist_v <- paste0(base_v, "_dist")
  
  if (jv %in% names(panel_individual)) {
    if (is.numeric(panel_individual[[iv]]) && is.numeric(panel_individual[[jv]])) {
      panel_individual[[diff_v]] <- safe_diff(panel_individual[[iv]], panel_individual[[jv]])
      panel_individual[[dist_v]] <- safe_absdiff(panel_individual[[iv]], panel_individual[[jv]])
    }
  }
}

panel_individual <- panel_individual %>%
  mutate(
    ccei_pair_max = safe_pmax(ccei_i, ccei_j),
    ccei_pair_min = safe_pmin(ccei_i, ccei_j),
    f_ccei_pair_max = safe_pmax(f_ccei_i, f_ccei_j),
    f_ccei_pair_min = safe_pmin(f_ccei_i, f_ccei_j),
    min_mpi_pair_lowest = safe_pmin(min_mpi_i, min_mpi_j),
    min_mpi_pair_highest = safe_pmax(min_mpi_i, min_mpi_j),
    max_mpi_pair_lowest = safe_pmin(max_mpi_i, max_mpi_j),
    max_mpi_pair_highest = safe_pmax(max_mpi_i, max_mpi_j),
    rev_max_mpi_pair_max = safe_pmax(rev_max_mpi_i, rev_max_mpi_j),
    rev_max_mpi_pair_min = safe_pmin(rev_max_mpi_i, rev_max_mpi_j),
    HighCCEI_post = HighCCEI * post,
    HighF_CCEI_post = HighF_CCEI * post,
    HighMinMPI_post = HighMinMPI * post,
    HighMaxMPI_post = HighMaxMPI * post,
    HighRevMaxMPI_post = HighRevMaxMPI * post,
    new2_I_ig = I_ig,
    new2_f_I_ig = f_I_ig,
    new2_I_min_mpi_ig = I_min_mpi_ig,
    new2_I_max_mpi_ig = I_max_mpi_ig,
    new2_I_rev_max_mpi_ig = I_rev_max_mpi_ig,
    rmpi_i = rev_max_mpi_i,
    rmpi_j = rev_max_mpi_j,
    rmpi_g = rev_max_mpi_g,
    rmpi_ig = rev_max_mpi_ig,
    rmpi_jg = rev_max_mpi_jg,
    I_rmpi_ig = I_rev_max_mpi_ig,
    HighRMPI = HighRevMaxMPI,
    HighRMPI_post = HighRMPI * post
  )

if ("inclass_friends_i" %in% names(panel_individual)) {
  panel_individual <- panel_individual %>%
    mutate(
      inclass_n_friends_i = inclass_friends_i,
      inclass_n_friends_j = inclass_friends_j,
      inclass_n_diff = inclass_friends_diff,
      inclass_n_dist = inclass_friends_dist
    )
}

if ("inclass_popularity_i" %in% names(panel_individual)) {
  panel_individual <- panel_individual %>%
    mutate(
      inclass_pop_diff = inclass_popularity_diff,
      inclass_pop_dist = inclass_popularity_dist
    )
}


############################################################
# Missing indicators + zero imputation for selected non-redundant variables
#
# We do NOT impute every *_i, *_j, *_diff, *_dist variable.
# To avoid redundant controls and invalid long Stata names, this block only
# uses the selected ML-style variables:
#   - own value: *_i
#   - pair difference: *_diff
#   - no *_j
#   - no *_dist
#   - no item-level components such as *_1, *_2, *_3
#   - no pair-direction friendship variables
############################################################

selected_imputation_vars <- c(
  # Group 1: cognitive and non-cognitive skills
  "mathscore_i",
  "mathscore_diff",
  "outgoing_i",
  "outgoing_diff",
  "opened_i",
  "opened_diff",
  "agreeable_i",
  "agreeable_diff",
  "conscientious_i",
  "conscientious_diff",
  "stable_i",
  "stable_diff",
  
  # Group 2: demographic and friendship
  "male_i",
  "male_diff",
  "height_i",
  "height_diff",
  "weight_i",
  "weight_diff",
  "brothers_i",
  "brothers_diff",
  "sisters_i",
  "sisters_diff",
  "birthorder_i",
  "birthorder_diff",
  "fatherage_i",
  "fatherage_diff",
  "motherage_i",
  "motherage_diff",
  "fathersch_i",
  "fathersch_diff",
  "mothersch_i",
  "mothersch_diff",
  "giftcard_i",
  "giftcard_diff",
  "inclass_n_friends_i",
  "inclass_n_diff",
  "inclass_popularity_i",
  "inclass_popularity_diff",
  
  # Group 3: interaction and perceived classroom environment
  "pblclass_horizontal_i",
  "pblclass_horizontal_diff",
  "teacher_induce_i",
  "teacher_induce_diff",
  "peer_sociable_i",
  "peer_sociable_diff",
  "peer_fair_i",
  "peer_fair_diff",
  "peer_helpful_i",
  "peer_helpful_diff",
  "peer_selfish_i",
  "peer_selfish_diff",
  "peer_reciprocal_i",
  "peer_reciprocal_diff",
  "class_sociable_i",
  "class_sociable_diff",
  "class_belonged_i",
  "class_belonged_diff",
  "class_outcast_i",
  "class_outcast_diff",
  "class_harass_i",
  "class_harass_diff",
  
  # Group 4: additional controls
  "ccei_i",
  "ccei_diff",
  "RA_i",
  "RA_diff"
)

selected_imputation_vars <- unique(selected_imputation_vars)
selected_imputation_vars <- selected_imputation_vars[selected_imputation_vars %in% names(panel_individual)]
selected_imputation_vars <- selected_imputation_vars[
  sapply(panel_individual[, selected_imputation_vars, drop = FALSE], is.numeric)
]

missing_dummy_names <- paste0(selected_imputation_vars, "_missing")

if (any(nchar(missing_dummy_names) > 32)) {
  too_long <- missing_dummy_names[nchar(missing_dummy_names) > 32]
  stop(
    paste0(
      "Some selected missing dummy names are longer than 32 characters: ",
      paste(too_long, collapse = ", ")
    )
  )
}

if (any(duplicated(missing_dummy_names))) {
  dup_names <- missing_dummy_names[duplicated(missing_dummy_names)]
  stop(
    paste0(
      "Duplicate missing dummy names: ",
      paste(unique(dup_names), collapse = ", ")
    )
  )
}

missing_imputation_summary <- tibble(
  var = selected_imputation_vars,
  missing_dummy = missing_dummy_names,
  n_missing_before = sapply(
    selected_imputation_vars,
    function(v) sum(is.na(panel_individual[[v]]))
  )
)

for (k in seq_along(selected_imputation_vars)) {
  v <- selected_imputation_vars[k]
  miss_v <- missing_dummy_names[k]
  panel_individual[[miss_v]] <- as.integer(is.na(panel_individual[[v]]))
  panel_individual[[v]][is.na(panel_individual[[v]])] <- 0
}

cat("\nMissing indicators and zero imputation applied to selected variables:\n")
missing_imputation_summary %>%
  filter(n_missing_before > 0) %>%
  arrange(desc(n_missing_before), var) %>%
  print(n = Inf, width = Inf)

cat("\nSelected variables not found or not numeric, therefore not imputed:\n")
print(
  setdiff(
    unique(c(
      "mathscore_i", "mathscore_diff",
      "outgoing_i", "outgoing_diff",
      "opened_i", "opened_diff",
      "agreeable_i", "agreeable_diff",
      "conscientious_i", "conscientious_diff",
      "stable_i", "stable_diff",
      "height_i", "height_diff",
      "weight_i", "weight_diff",
      "brothers_i", "brothers_diff",
      "sisters_i", "sisters_diff",
      "birthorder_i", "birthorder_diff",
      "fatherage_i", "fatherage_diff",
      "motherage_i", "motherage_diff",
      "fathersch_i", "fathersch_diff",
      "mothersch_i", "mothersch_diff",
      "giftcard_i", "giftcard_diff",
      "inclass_n_friends_i", "inclass_n_diff",
      "inclass_popularity_i", "inclass_popularity_diff",
      "pblclass_horizontal_i", "pblclass_horizontal_diff",
      "teacher_induce_i", "teacher_induce_diff",
      "peer_sociable_i", "peer_sociable_diff",
      "peer_fair_i", "peer_fair_diff",
      "peer_helpful_i", "peer_helpful_diff",
      "peer_selfish_i", "peer_selfish_diff",
      "peer_reciprocal_i", "peer_reciprocal_diff",
      "class_sociable_i", "class_sociable_diff",
      "class_belonged_i", "class_belonged_diff",
      "class_outcast_i", "class_outcast_diff",
      "class_harass_i", "class_harass_diff",
      "ccei_i", "ccei_diff",
      "RA_i", "RA_diff"
    )),
    selected_imputation_vars
  )
)

dir.create("data/checks", showWarnings = FALSE, recursive = TRUE)
write_csv(
  missing_imputation_summary,
  "data/checks/panel_individual_selected_missing_imputation_summary.csv"
)

write_dta(panel_individual, "data/panel_individual.dta")


final_check <- tibble(
  file = c(
    "panel_final",
    "panel_group",
    "panel_individual",
    "network_panel_clean"
  ),
  n_rows = c(
    nrow(panel_final),
    nrow(panel_group),
    nrow(panel_individual),
    nrow(network_panel_clean)
  ),
  n_cols = c(
    ncol(panel_final),
    ncol(panel_group),
    ncol(panel_individual),
    ncol(network_panel_clean)
  )
)

print(final_check, width = Inf)

network_check <- panel_final %>%
  summarise(
    n = n(),
    mutual_base = sum(mutual_friendship_base == 1, na.rm = TRUE),
    oneway_base = sum(oneway_friendship_base == 1, na.rm = TRUE),
    none_base = sum(none_friendship_base == 1, na.rm = TRUE),
    full_check_base = sum(mutual_friendship_base + oneway_friendship_base + none_friendship_base, na.rm = TRUE),
    inclass_mutual_base = sum(inclass_mutual_friendship_base == 1, na.rm = TRUE),
    inclass_oneway_base = sum(inclass_oneway_friendship_base == 1, na.rm = TRUE),
    inclass_none_base = sum(inclass_none_friendship_base == 1, na.rm = TRUE),
    inclass_check_base = sum(inclass_mutual_friendship_base + inclass_oneway_friendship_base + inclass_none_friendship_base, na.rm = TRUE),
    mutual_end = sum(mutual_friendship_end == 1, na.rm = TRUE),
    oneway_end = sum(oneway_friendship_end == 1, na.rm = TRUE),
    none_end = sum(none_friendship_end == 1, na.rm = TRUE),
    full_check_end = sum(mutual_friendship_end + oneway_friendship_end + none_friendship_end, na.rm = TRUE),
    inclass_mutual_end = sum(inclass_mutual_friendship_end == 1, na.rm = TRUE),
    inclass_oneway_end = sum(inclass_oneway_friendship_end == 1, na.rm = TRUE),
    inclass_none_end = sum(inclass_none_friendship_end == 1, na.rm = TRUE),
    inclass_check_end = sum(inclass_mutual_friendship_end + inclass_oneway_friendship_end + inclass_none_friendship_end, na.rm = TRUE),
    missing_all_friends_1_base = sum(is.na(all_friends_1_base)),
    missing_all_friends_2_base = sum(is.na(all_friends_2_base)),
    missing_all_friends_1_end = sum(is.na(all_friends_1_end)),
    missing_all_friends_2_end = sum(is.na(all_friends_2_end))
  )

print(network_check, width = Inf)
