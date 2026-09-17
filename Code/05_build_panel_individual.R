# Build the individual panel.

rm(list = ls())

library(tidyverse)
library(dplyr)
library(haven)
library(stringr)

# Fix namespace conflicts.
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


script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", grep("^--file=", script_args, value = TRUE))
if (length(script_file) == 1) setwd(dirname(normalizePath(script_file)))

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

required_ihat_cols <- c(
  "ccei_1_base", "ccei_2_base", "ccei_1_end", "ccei_2_end",
  "maxmpi_1_base", "maxmpi_2_base", "maxmpi_1_end", "maxmpi_2_end",
  "hm_1_base", "hm_2_base", "hm_1_end", "hm_2_end",
  "cei_g_base", "cei_g_end",
  "cei_g_untempered_base", "cei_g_untempered_end",
  "cei_n_viol_base", "cei_n_viol_end",
  "cei_n_viol_untempered_base", "cei_n_viol_untempered_end",
  "Ihat_1g_base", "Ihat_2g_base",
  "Ihat_1g_end", "Ihat_2g_end",
  "Ihat_maxmpi_1g_base", "Ihat_maxmpi_2g_base",
  "Ihat_maxmpi_1g_end", "Ihat_maxmpi_2g_end",
  "Ihat_hm_1g_base", "Ihat_hm_2g_base",
  "Ihat_hm_1g_end", "Ihat_hm_2g_end",
  "c_Ng_base", "c_Ng_end",
  "c_maxmpi_Ng_base", "c_maxmpi_Ng_end",
  "c_hm_Ng_base", "c_hm_Ng_end"
)
missing_ihat_cols <- setdiff(required_ihat_cols, names(panel_final))
if (length(missing_ihat_cols) > 0) {
  stop("Missing Ihat columns: ", paste(missing_ihat_cols, collapse = ", "))
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

  ccei_i <- value_or_na(df, paste0("ccei_", my_num, "_", suffix))
  ccei_j <- value_or_na(df, paste0("ccei_", partner_num, "_", suffix))
  maxmpi_i <- value_or_na(df, paste0("maxmpi_", my_num, "_", suffix))
  maxmpi_j <- value_or_na(df, paste0("maxmpi_", partner_num, "_", suffix))
  hm_i <- value_or_na(df, paste0("hm_", my_num, "_", suffix))
  hm_j <- value_or_na(df, paste0("hm_", partner_num, "_", suffix))

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
    ccei_i = ccei_i,
    ccei_j = ccei_j,
    ccei_g = value_or_na(df, paste0("ccei_g_", suffix)),
    ccei_ig = value_or_na(df, paste0("ccei_", my_num, "g_", suffix)),
    ccei_jg = value_or_na(df, paste0("ccei_", partner_num, "g_", suffix)),
    c_ccei_ig = value_or_na(df, paste0("c_ccei_", my_num, "g_", suffix)),
    c_ccei_jg = value_or_na(df, paste0("c_ccei_", partner_num, "g_", suffix)),
    HighCCEI_both_high = ifelse(
      is.na(ccei_i) | is.na(ccei_j),
      NA_real_,
      as.numeric(ccei_i >= ccei_j)
    ),
    HighCCEI_both_low = ifelse(
      is.na(ccei_i) | is.na(ccei_j),
      NA_real_,
      as.numeric(ccei_i > ccei_j)
    ),
    ccei_gap_ij = ccei_i - ccei_j,
    Ihat_ig = value_or_na(df, paste0("Ihat_", my_num, "g_", suffix)),
    c_Ng = value_or_na(df, paste0("c_Ng_", suffix)),
    maxmpi_i = maxmpi_i,
    maxmpi_j = maxmpi_j,
    maxmpi_g = value_or_na(df, paste0("maxmpi_g_", suffix)),
    maxmpi_ig = value_or_na(df, paste0("maxmpi_", my_num, "g_", suffix)),
    maxmpi_jg = value_or_na(df, paste0("maxmpi_", partner_num, "g_", suffix)),
    c_maxmpi_ig = value_or_na(df, paste0("c_maxmpi_", my_num, "g_", suffix)),
    c_maxmpi_jg = value_or_na(df, paste0("c_maxmpi_", partner_num, "g_", suffix)),
    c_maxmpi_Ng = value_or_na(df, paste0("c_maxmpi_Ng_", suffix)),
    Ihat_maxmpi_ig = value_or_na(df, paste0("Ihat_maxmpi_", my_num, "g_", suffix)),
    HighMaxMPI_both_high = ifelse(
      is.na(maxmpi_i) | is.na(maxmpi_j),
      NA_real_,
      as.numeric(maxmpi_i <= maxmpi_j)
    ),
    HighMaxMPI_both_low = ifelse(
      is.na(maxmpi_i) | is.na(maxmpi_j),
      NA_real_,
      as.numeric(maxmpi_i < maxmpi_j)
    ),
    maxmpi_gap_ij = maxmpi_j - maxmpi_i,
    hm_i = hm_i,
    hm_j = hm_j,
    hm_g = value_or_na(df, paste0("hm_g_", suffix)),
    hm_ig = value_or_na(df, paste0("hm_", my_num, "g_", suffix)),
    hm_jg = value_or_na(df, paste0("hm_", partner_num, "g_", suffix)),
    c_hm_ig = value_or_na(df, paste0("c_hm_", my_num, "g_", suffix)),
    c_hm_jg = value_or_na(df, paste0("c_hm_", partner_num, "g_", suffix)),
    c_hm_Ng = value_or_na(df, paste0("c_hm_Ng_", suffix)),
    Ihat_hm_ig = value_or_na(df, paste0("Ihat_hm_", my_num, "g_", suffix)),
    HighHM_both_high = ifelse(
      is.na(hm_i) | is.na(hm_j),
      NA_real_,
      as.numeric(hm_i <= hm_j)
    ),
    HighHM_both_low = ifelse(
      is.na(hm_i) | is.na(hm_j),
      NA_real_,
      as.numeric(hm_i < hm_j)
    ),
    hm_gap_ij = hm_j - hm_i,
    cei_g = value_or_na(df, paste0("cei_g_", suffix)),
    cei_g_untempered = value_or_na(df, paste0("cei_g_untempered_", suffix)),
    cei_n_viol = value_or_na(df, paste0("cei_n_viol_", suffix)),
    cei_n_viol_untempered = value_or_na(
      df,
      paste0("cei_n_viol_untempered_", suffix)
    ),
    RA_i = value_or_na(df, paste0("RA_", my_num, "_", suffix)),
    RA_j = value_or_na(df, paste0("RA_", partner_num, "_", suffix)),
    RA_g = value_or_na(df, paste0("RA_g_", suffix)),
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
    "maxmpi",
    "hm",
    "RA",
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
  arrange(group_id, endline, person) %>%
  mutate(
    cei_full = as.numeric(cei_g >= 1 - 1e-9),
    ccei_full = as.numeric(ccei_g == 1),
    cei_type = case_when(
      cei_full == 1 & ccei_full == 1 ~ 1,
      cei_full == 1 & ccei_full == 0 ~ 2,
      cei_full == 0 & ccei_full == 0 ~ 3,
      TRUE ~ 4
    ),
    cei_type_a = as.numeric(cei_type == 1),
    cei_type_b = as.numeric(cei_type == 2),
    cei_type_c = as.numeric(cei_type == 3),
    cei_type_d = as.numeric(cei_type == 4)
  )

stopifnot(
  all(panel_individual$cei_g > 0 & panel_individual$cei_g <= 1),
  all(panel_individual$cei_g_untempered > 0 & panel_individual$cei_g_untempered <= 1),
  all(panel_individual$cei_type_a + panel_individual$cei_type_b +
        panel_individual$cei_type_c + panel_individual$cei_type_d == 1)
)

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

# Fill time-invariant variables.

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
  "giftcard",
  "class_study",
  "class_dislike",
  "class_lonely",
  paste0("selfesteem_", 1:10),
  "selfesteem",
  "outgoing",
  "agreeable",
  "conscientious",
  "stable",
  "opened"
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
    HighCCEI_both_high_post = HighCCEI_both_high * post,
    HighCCEI_both_low_post = HighCCEI_both_low * post,
    ccei_gap_ij_post = ccei_gap_ij * post,
    HighMaxMPI_both_high_post = HighMaxMPI_both_high * post,
    HighMaxMPI_both_low_post = HighMaxMPI_both_low * post,
    maxmpi_gap_ij_post = maxmpi_gap_ij * post,
    HighHM_both_high_post = HighHM_both_high * post,
    HighHM_both_low_post = HighHM_both_low * post,
    hm_gap_ij_post = hm_gap_ij * post
  )

# Check the CCEI definitions.
defined_ccei <- !is.na(panel_individual$ccei_i) & !is.na(panel_individual$ccei_j)
equal_ccei <- defined_ccei & panel_individual$ccei_i == panel_individual$ccei_j
unequal_ccei <- defined_ccei & panel_individual$ccei_i != panel_individual$ccei_j
stopifnot(
  all(panel_individual$HighCCEI_both_high[equal_ccei] == 1),
  all(panel_individual$HighCCEI_both_low[equal_ccei] == 0),
  all(
    panel_individual$HighCCEI_both_high[unequal_ccei] ==
      panel_individual$HighCCEI_both_low[unequal_ccei]
  ),
  all(
    panel_individual$ccei_gap_ij[defined_ccei] ==
      panel_individual$ccei_i[defined_ccei] - panel_individual$ccei_j[defined_ccei]
  )
)

baseline_only_check_vars <- c(
  "class_study", "class_dislike", "class_lonely",
  paste0("selfesteem_", 1:10), "selfesteem",
  "outgoing", "agreeable", "conscientious", "stable", "opened"
)

for (v in baseline_only_check_vars) {
  nm <- paste0(v, "_i")
  if (!nm %in% names(panel_individual)) stop("Missing baseline-only variable: ", nm)
  chk <- panel_individual %>%
    select(id, post, value = all_of(nm)) %>%
    distinct(id, post, .keep_all = TRUE) %>%
    pivot_wider(names_from = post, values_from = value, names_prefix = "post_")
  same <- (is.na(chk$post_0) & is.na(chk$post_1)) |
    (!is.na(chk$post_0) & !is.na(chk$post_1) & chk$post_0 == chk$post_1)
  if (!all(same)) stop("Baseline-only variable changed across waves: ", nm)
}

# Check the MaxMPI and HM definitions.
for (measure in c("maxmpi", "hm")) {
  label <- if (measure == "maxmpi") "MaxMPI" else "HM"
  score_i <- panel_individual[[paste0(measure, "_i")]]
  score_j <- panel_individual[[paste0(measure, "_j")]]
  defined <- !is.na(score_i) & !is.na(score_j)
  equal <- defined & score_i == score_j
  unequal <- defined & score_i != score_j
  high_ties <- panel_individual[[paste0("High", label, "_both_high")]]
  low_ties <- panel_individual[[paste0("High", label, "_both_low")]]

  stopifnot(
    all(high_ties[equal] == 1),
    all(low_ties[equal] == 0),
    all(high_ties[unequal] == low_ties[unequal]),
    all(panel_individual[[paste0(measure, "_gap_ij")]][defined] ==
          score_j[defined] - score_i[defined])
  )
}

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


# Impute selected controls.

selected_imputation_vars <- c(
  # Skills.
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
  
  # Demographics and friendship.
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
  
  # Classroom environment.
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
  
  # Additional controls.
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

for (index_var in c("Ihat_ig", "Ihat_maxmpi_ig", "Ihat_hm_ig")) {
  ihat_individual_check <- panel_individual %>%
    group_by(group_id, endline) %>%
    summarise(
      n_members = n(),
      n_defined = sum(!is.na(.data[[index_var]])),
      Ihat_sum = ifelse(n_defined == 2, sum(.data[[index_var]]), NA_real_),
      .groups = "drop"
    )

  ihat_individual_problem <- ihat_individual_check %>%
    filter(
      n_members != 2 |
        n_defined == 1 |
        (!is.na(Ihat_sum) & abs(Ihat_sum - 1) > 1e-8)
    )

  if (nrow(ihat_individual_problem) > 0) {
    cat("\nInvalid index mapping for ", index_var, ":\n", sep = "")
    print(ihat_individual_problem, n = Inf, width = Inf)
    stop(index_var, " mapping failed; panel_individual.dta was not written.")
  }

  cat(
    "\n", index_var, " mapped. Defined rows: ",
    sum(!is.na(panel_individual[[index_var]])),
    " Undefined rows: ",
    sum(is.na(panel_individual[[index_var]])),
    "\n",
    sep = ""
  )
}

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

