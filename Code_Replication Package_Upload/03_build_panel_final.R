## Byunghun Hahn, June 29
## Merge clean modules into panel_final and save

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


setwd(dirname(getSourceEditorContext()$path))

id_as_char <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_remove(x, "\\.0$")
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  return(x)
}

class_from_id <- function(x) {
  x <- id_as_char(x)
  out <- substr(x, 1, 5)
  out <- ifelse(is.na(x) | x == "" | nchar(x) < 5, NA_character_, out)
  return(out)
}

num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
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

check_row_unchanged <- function(df_before, df_after, step_name) {
  if (nrow(df_before) != nrow(df_after)) {
    stop(paste0("Row count changed in: ", step_name))
  }
}

make_network_all <- function(network_raw) {
  network_raw %>%
    transmute(
      id = id_as_char(.data[["id"]]),
      obj_id = id_as_char(.data[["obj_id"]]),
      t = as.integer(num(.data[["t"]]))
    ) %>%
    filter(
      t %in% c(0, 1),
      !is.na(id),
      id != ""
    ) %>%
    mutate(
      id_class = substr(id, 1, 5),
      obj_class = substr(obj_id, 1, 5),
      inclass = as.integer(!is.na(obj_id) & obj_id != "" & id_class == obj_class)
    )
}

make_network_edges <- function(network_all) {
  network_all %>%
    filter(
      !is.na(obj_id),
      obj_id != "",
      id != obj_id
    ) %>%
    distinct(t, id, obj_id, .keep_all = TRUE) %>%
    mutate(friend_nomination = 1L)
}

make_network_degree <- function(network_all, network_edges, t_value, suffix = NULL) {
  id_list <- bind_rows(
    network_all %>%
      filter(t == t_value) %>%
      select(id),
    network_edges %>%
      filter(t == t_value) %>%
      select(id = obj_id)
  ) %>%
    distinct(id) %>%
    filter(!is.na(id), id != "")
  
  all_outdegree_df <- network_edges %>%
    filter(t == t_value) %>%
    group_by(id) %>%
    summarise(all_friends = n_distinct(obj_id), .groups = "drop")
  
  inclass_outdegree_df <- network_edges %>%
    filter(t == t_value, inclass == 1) %>%
    group_by(id) %>%
    summarise(inclass_friends = n_distinct(obj_id), .groups = "drop")
  
  all_indegree_df <- network_edges %>%
    filter(t == t_value) %>%
    group_by(obj_id) %>%
    summarise(all_popularity = n_distinct(id), .groups = "drop") %>%
    rename(id = obj_id)
  
  inclass_indegree_df <- network_edges %>%
    filter(t == t_value, inclass == 1) %>%
    group_by(obj_id) %>%
    summarise(inclass_popularity = n_distinct(id), .groups = "drop") %>%
    rename(id = obj_id)
  
  out <- id_list %>%
    left_join(all_outdegree_df, by = "id") %>%
    left_join(inclass_outdegree_df, by = "id") %>%
    left_join(all_indegree_df, by = "id") %>%
    left_join(inclass_indegree_df, by = "id") %>%
    mutate(
      all_friends = ifelse(is.na(all_friends), 0L, as.integer(all_friends)),
      inclass_friends = ifelse(is.na(inclass_friends), 0L, as.integer(inclass_friends)),
      all_popularity = ifelse(is.na(all_popularity), 0L, as.integer(all_popularity)),
      inclass_popularity = ifelse(is.na(inclass_popularity), 0L, as.integer(inclass_popularity))
    )
  
  if (!is.null(suffix)) {
    out <- out %>%
      rename(
        !!paste0("all_friends_", suffix) := all_friends,
        !!paste0("inclass_friends_", suffix) := inclass_friends,
        !!paste0("all_popularity_", suffix) := all_popularity,
        !!paste0("inclass_popularity_", suffix) := inclass_popularity
      )
  }
  
  return(out)
}

load_clean_module <- function(path) {
  read_dta(path) %>%
    mutate(id = id_as_char(id))
}

noncog_pre_clean <- load_clean_module("data/noncog_pre_clean.dta")
male_clean <- load_clean_module("data/male_clean.dta")
noncog_post_clean <- load_clean_module("data/noncog_post_clean.dta")
cog_pre_clean <- load_clean_module("data/cog_pre_clean.dta")
cog_post_clean <- load_clean_module("data/cog_post_clean.dta")
risksurvey_pre_clean <- load_clean_module("data/risksurvey_pre_clean.dta")
risksurvey_post_clean <- load_clean_module("data/risksurvey_post_clean.dta")
rat_pre_clean <- load_clean_module("data/rat_pre_clean.dta")
rat_post_clean <- load_clean_module("data/rat_post_clean.dta")
network_raw <- read_dta("data/network_survey.dta")
network_all <- make_network_all(network_raw)
network_edges <- make_network_edges(network_all)

panel_final <- read_dta("data/panel_final.dta")

file.copy(
  "data/panel_final.dta",
  "data/panel_final_backup_before_updated_survey_network_merge.dta",
  overwrite = TRUE
)

panel_final <- panel_final %>%
  mutate(
    group_id = id_as_char(group_id),
    id_mover_base = id_as_char(id_mover_base),
    id_nonmover_base = id_as_char(id_nonmover_base),
    id_mover_end = id_as_char(id_mover_end),
    id_nonmover_end = id_as_char(id_nonmover_end),
    class = class_from_id(id_mover_base)
  )

class_check <- panel_final %>%
  transmute(
    group_id,
    class,
    class_mover_base = class_from_id(id_mover_base),
    class_nonmover_base = class_from_id(id_nonmover_base),
    class_mover_end = class_from_id(id_mover_end),
    class_nonmover_end = class_from_id(id_nonmover_end)
  )

bad_class_rows <- class_check %>%
  filter(
    is.na(class) | class == "" |
      class != class_mover_base |
      class != class_nonmover_base |
      class != class_mover_end |
      class != class_nonmover_end
  )

if (nrow(bad_class_rows) > 0) {
  cat("\nClass mismatch across mover/nonmover or base/end ids:\n")
  print(bad_class_rows, n = Inf, width = Inf)
  stop("Class variable cannot be uniquely defined for some group rows.")
}

cat("\nClass variable created in panel_final from the first five digits of id_mover_base.\n")
cat("Number of classes:", n_distinct(panel_final$class), "\n")

height_clean <- NULL
if (file.exists("data/height.dta")) {
  height_clean <- read_dta("data/height.dta") %>%
    transmute(
      id = id_as_char(id),
      height_base = num(height),
      weight_base = num(weight)
    ) %>%
    filter(!is.na(id), id != "") %>%
    distinct(id, .keep_all = TRUE)
}

add_person_suffix <- function(varnames, person_num) {
  case_when(
    str_ends(varnames, "_base") ~ str_replace(varnames, "_base$", paste0("_", person_num, "_base")),
    str_ends(varnames, "_end") ~ str_replace(varnames, "_end$", paste0("_", person_num, "_end")),
    TRUE ~ paste0(varnames, "_", person_num)
  )
}

make_person_data <- function(clean_df, panel_id_col, person_num) {
  old_names <- names(clean_df)
  new_names <- old_names
  new_names[old_names == "id"] <- panel_id_col
  new_names[old_names != "id"] <- add_person_suffix(old_names[old_names != "id"], person_num)
  out <- clean_df
  names(out) <- new_names
  return(out)
}

generated_names_for_module <- function(clean_df, person_num) {
  vars <- setdiff(names(clean_df), "id")
  add_person_suffix(vars, person_num)
}

module_specs <- list(
  list(df = male_clean, id_col = "id_mover_base", person = 1),
  list(df = male_clean, id_col = "id_nonmover_base", person = 2),
  list(df = noncog_pre_clean, id_col = "id_mover_base", person = 1),
  list(df = noncog_pre_clean, id_col = "id_nonmover_base", person = 2),
  list(df = cog_pre_clean, id_col = "id_mover_base", person = 1),
  list(df = cog_pre_clean, id_col = "id_nonmover_base", person = 2),
  list(df = risksurvey_pre_clean, id_col = "id_mover_base", person = 1),
  list(df = risksurvey_pre_clean, id_col = "id_nonmover_base", person = 2),
  list(df = rat_pre_clean, id_col = "id_mover_base", person = 1),
  list(df = rat_pre_clean, id_col = "id_nonmover_base", person = 2),
  list(df = noncog_post_clean, id_col = "id_mover_end", person = 1),
  list(df = noncog_post_clean, id_col = "id_nonmover_end", person = 2),
  list(df = cog_post_clean, id_col = "id_mover_end", person = 1),
  list(df = cog_post_clean, id_col = "id_nonmover_end", person = 2),
  list(df = risksurvey_post_clean, id_col = "id_mover_end", person = 1),
  list(df = risksurvey_post_clean, id_col = "id_nonmover_end", person = 2),
  list(df = rat_post_clean, id_col = "id_mover_end", person = 1),
  list(df = rat_post_clean, id_col = "id_nonmover_end", person = 2)
)

if (!is.null(height_clean)) {
  module_specs <- c(
    module_specs,
    list(
      list(df = height_clean, id_col = "id_mover_base", person = 1),
      list(df = height_clean, id_col = "id_nonmover_base", person = 2)
    )
  )
}

generated_cols <- unique(unlist(
  map(
    module_specs,
    function(x) {
      generated_names_for_module(x$df, x$person)
    }
  )
))

old_survey_network_patterns <- paste0(
  "(",
  "^male_[12]$|",
  "^class_.*_[12]_(base|end)$|",
  "^teacher_.*_[12]_(base|end)$|",
  "^peer_.*_[12]_(base|end)$|",
  "^value_.*_[12]_(base|end)$|",
  "^selfesteem.*_[12]_(base|end)$|",
  "^outgoing_[12]_(base|end)$|",
  "^agreeable_[12]_(base|end)$|",
  "^conscientious_[12]_(base|end)$|",
  "^stable_[12]_(base|end)$|",
  "^opened_[12]_(base|end)$|",
  "^pbl.*_[12]_(base|end)$|",
  "^life.*_[12]_(base|end)$|",
  "^brothers_[12]_(base|end)$|",
  "^sisters_[12]_(base|end)$|",
  "^birthorder_[12]_(base|end)$|",
  "^fatherage_[12]_(base|end)$|",
  "^motherage_[12]_(base|end)$|",
  "^fathersch_[12]_(base|end)$|",
  "^mothersch_[12]_(base|end)$|",
  "^schchoice.*_[12]_(base|end)$|",
  "^giftcard_[12]_(base|end)$|",
  "^mathscore_[12]_(base|end)$|",
  "^risk_.*_[12]_(base|end)$|",
  "^RAT_.*_[12]_(base|end)$|",
  "^height_[12]_base$|",
  "^weight_[12]_base$|",
  "^height_(max|min|dist)_base$|",
  "^weight_(max|min|dist)_base$|",
  "^all_friends_[12]_(base|end)$|",
  "^inclass_friends_[12]_(base|end)$|",
  "^all_popularity_[12]_(base|end)$|",
  "^inclass_popularity_[12]_(base|end)$|",
  "^all_friends_(max|min|dist)_(base|end)$|",
  "^inclass_friends_(max|min|dist)_(base|end)$|",
  "^all_popularity_(max|min|dist)_(base|end)$|",
  "^inclass_popularity_(max|min|dist)_(base|end)$|",
  "^friendship_1_(base|end)$|",
  "^friendship_2_(base|end)$|",
  "^mutual_friendship_(base|end)$|",
  "^oneway_friendship_(base|end)$|",
  "^oneside_friendship_(base|end)$|",
  "^none_friendship_(base|end)$|",
  "^friendship_(base|end)$|",
  "^inclass_friendship_1_(base|end)$|",
  "^inclass_friendship_2_(base|end)$|",
  "^inclass_mutual_friendship_(base|end)$|",
  "^inclass_oneway_friendship_(base|end)$|",
  "^inclass_oneside_friendship_(base|end)$|",
  "^inclass_none_friendship_(base|end)$|",
  "^inclass_friendship_(base|end)$|",
  "^inclass_n_friends_[12]_(base|end)$|",
  "^inclass_n_friends_[12]$|",
  "^inclass_n_diff$|",
  "^inclass_n_dist$|",
  "^inclass_pop_diff$|",
  "^inclass_pop_dist$",
  ")"
)

cols_to_remove <- unique(c(
  generated_cols,
  names(panel_final)[str_detect(names(panel_final), old_survey_network_patterns)]
))
cols_to_remove <- intersect(cols_to_remove, names(panel_final))

panel_final <- panel_final %>%
  select(-all_of(cols_to_remove))

for (spec in module_specs) {
  person_df <- make_person_data(
    clean_df = spec$df,
    panel_id_col = spec$id_col,
    person_num = spec$person
  )
  n_before <- nrow(panel_final)
  panel_final <- panel_final %>%
    left_join(person_df, by = spec$id_col)
  if (nrow(panel_final) != n_before) {
    stop(paste0("Row count changed after merging on ", spec$id_col))
  }
}


degree_base <- make_network_degree(
  network_all = network_all,
  network_edges = network_edges,
  t_value = 0,
  suffix = "base"
)

degree_end <- make_network_degree(
  network_all = network_all,
  network_edges = network_edges,
  t_value = 1,
  suffix = "end"
)

join_degree_to_panel <- function(panel_df, degree_df, id_col, person_num, suffix) {
  degree_person <- degree_df %>%
    rename(!!id_col := id) %>%
    rename(
      !!paste0("all_friends_", person_num, "_", suffix) := !!sym(paste0("all_friends_", suffix)),
      !!paste0("inclass_friends_", person_num, "_", suffix) := !!sym(paste0("inclass_friends_", suffix)),
      !!paste0("all_popularity_", person_num, "_", suffix) := !!sym(paste0("all_popularity_", suffix)),
      !!paste0("inclass_popularity_", person_num, "_", suffix) := !!sym(paste0("inclass_popularity_", suffix))
    )
  out <- panel_df %>%
    left_join(degree_person, by = id_col)
  check_row_unchanged(panel_df, out, paste0("join_degree_to_panel_", id_col, "_", suffix))
  return(out)
}

add_pair_friendship_vars <- function(panel_df, network_edges, suffix, t_value, scope = c("all", "inclass")) {
  scope <- match.arg(scope)
  id1_col <- paste0("id_mover_", suffix)
  id2_col <- paste0("id_nonmover_", suffix)
  prefix <- ifelse(scope == "all", "", "inclass_")
  
  friendship_1_col <- paste0(prefix, "friendship_1_", suffix)
  friendship_2_col <- paste0(prefix, "friendship_2_", suffix)
  mutual_col <- paste0(prefix, "mutual_friendship_", suffix)
  oneway_col <- paste0(prefix, "oneway_friendship_", suffix)
  oneside_col <- paste0(prefix, "oneside_friendship_", suffix)
  none_col <- paste0(prefix, "none_friendship_", suffix)
  friendship_col <- paste0(prefix, "friendship_", suffix)
  
  edge_scope <- network_edges %>%
    filter(t == t_value)
  
  if (scope == "inclass") {
    edge_scope <- edge_scope %>%
      filter(inclass == 1)
  }
  
  pair_1 <- edge_scope %>%
    transmute(
      !!id1_col := id,
      !!id2_col := obj_id,
      !!friendship_1_col := 1L
    ) %>%
    distinct(.data[[id1_col]], .data[[id2_col]], .keep_all = TRUE)
  
  out <- panel_df %>%
    left_join(pair_1, by = c(id1_col, id2_col))
  check_row_unchanged(panel_df, out, paste0(scope, "_pair_1_", suffix))
  
  pair_2 <- edge_scope %>%
    transmute(
      !!id1_col := obj_id,
      !!id2_col := id,
      !!friendship_2_col := 1L
    ) %>%
    distinct(.data[[id1_col]], .data[[id2_col]], .keep_all = TRUE)
  
  out2 <- out %>%
    left_join(pair_2, by = c(id1_col, id2_col))
  check_row_unchanged(out, out2, paste0(scope, "_pair_2_", suffix))
  
  out2[[friendship_1_col]] <- ifelse(
    is.na(out2[[friendship_1_col]]),
    0L,
    as.integer(out2[[friendship_1_col]])
  )
  
  out2[[friendship_2_col]] <- ifelse(
    is.na(out2[[friendship_2_col]]),
    0L,
    as.integer(out2[[friendship_2_col]])
  )
  
  out2[[mutual_col]] <- as.integer(
    out2[[friendship_1_col]] == 1 &
      out2[[friendship_2_col]] == 1
  )
  
  out2[[oneway_col]] <- as.integer(
    out2[[friendship_1_col]] +
      out2[[friendship_2_col]] == 1
  )
  
  out2[[oneside_col]] <- out2[[oneway_col]]
  
  out2[[none_col]] <- as.integer(
    out2[[friendship_1_col]] == 0 &
      out2[[friendship_2_col]] == 0
  )
  
  out2[[friendship_col]] <- ifelse(
    out2[[mutual_col]] == 1,
    2L,
    ifelse(out2[[oneway_col]] == 1, 1L, 0L)
  )
  
  return(out2)
}

add_network_pair_derived_vars <- function(df) {
  df %>%
    mutate(
      all_friends_max_base = safe_pmax(all_friends_1_base, all_friends_2_base),
      all_friends_min_base = safe_pmin(all_friends_1_base, all_friends_2_base),
      all_friends_dist_base = safe_absdiff(all_friends_1_base, all_friends_2_base),
      inclass_friends_max_base = safe_pmax(inclass_friends_1_base, inclass_friends_2_base),
      inclass_friends_min_base = safe_pmin(inclass_friends_1_base, inclass_friends_2_base),
      inclass_friends_dist_base = safe_absdiff(inclass_friends_1_base, inclass_friends_2_base),
      all_popularity_max_base = safe_pmax(all_popularity_1_base, all_popularity_2_base),
      all_popularity_min_base = safe_pmin(all_popularity_1_base, all_popularity_2_base),
      all_popularity_dist_base = safe_absdiff(all_popularity_1_base, all_popularity_2_base),
      inclass_popularity_max_base = safe_pmax(inclass_popularity_1_base, inclass_popularity_2_base),
      inclass_popularity_min_base = safe_pmin(inclass_popularity_1_base, inclass_popularity_2_base),
      inclass_popularity_dist_base = safe_absdiff(inclass_popularity_1_base, inclass_popularity_2_base),
      all_friends_max_end = safe_pmax(all_friends_1_end, all_friends_2_end),
      all_friends_min_end = safe_pmin(all_friends_1_end, all_friends_2_end),
      all_friends_dist_end = safe_absdiff(all_friends_1_end, all_friends_2_end),
      inclass_friends_max_end = safe_pmax(inclass_friends_1_end, inclass_friends_2_end),
      inclass_friends_min_end = safe_pmin(inclass_friends_1_end, inclass_friends_2_end),
      inclass_friends_dist_end = safe_absdiff(inclass_friends_1_end, inclass_friends_2_end),
      all_popularity_max_end = safe_pmax(all_popularity_1_end, all_popularity_2_end),
      all_popularity_min_end = safe_pmin(all_popularity_1_end, all_popularity_2_end),
      all_popularity_dist_end = safe_absdiff(all_popularity_1_end, all_popularity_2_end),
      inclass_popularity_max_end = safe_pmax(inclass_popularity_1_end, inclass_popularity_2_end),
      inclass_popularity_min_end = safe_pmin(inclass_popularity_1_end, inclass_popularity_2_end),
      inclass_popularity_dist_end = safe_absdiff(inclass_popularity_1_end, inclass_popularity_2_end)
    )
}

network_panel_clean <- panel_final %>%
  select(
    group_id,
    id_mover_base,
    id_nonmover_base,
    id_mover_end,
    id_nonmover_end
  ) %>%
  distinct()

network_panel_clean <- network_panel_clean %>%
  join_degree_to_panel(
    degree_df = degree_base,
    id_col = "id_mover_base",
    person_num = 1,
    suffix = "base"
  ) %>%
  join_degree_to_panel(
    degree_df = degree_base,
    id_col = "id_nonmover_base",
    person_num = 2,
    suffix = "base"
  ) %>%
  join_degree_to_panel(
    degree_df = degree_end,
    id_col = "id_mover_end",
    person_num = 1,
    suffix = "end"
  ) %>%
  join_degree_to_panel(
    degree_df = degree_end,
    id_col = "id_nonmover_end",
    person_num = 2,
    suffix = "end"
  )

network_panel_clean <- network_panel_clean %>%
  add_pair_friendship_vars(network_edges = network_edges, suffix = "base", t_value = 0, scope = "all") %>%
  add_pair_friendship_vars(network_edges = network_edges, suffix = "base", t_value = 0, scope = "inclass") %>%
  add_pair_friendship_vars(network_edges = network_edges, suffix = "end", t_value = 1, scope = "all") %>%
  add_pair_friendship_vars(network_edges = network_edges, suffix = "end", t_value = 1, scope = "inclass") %>%
  add_network_pair_derived_vars()

write_dta(network_panel_clean, "data/network_panel_clean.dta")

network_overlap_cols <- intersect(
  setdiff(
    names(network_panel_clean),
    c(
      "group_id",
      "id_mover_base",
      "id_nonmover_base",
      "id_mover_end",
      "id_nonmover_end"
    )
  ),
  names(panel_final)
)

panel_final <- panel_final %>%
  select(-all_of(network_overlap_cols))

n_before <- nrow(panel_final)
panel_final <- panel_final %>%
  left_join(
    network_panel_clean,
    by = c(
      "group_id",
      "id_mover_base",
      "id_nonmover_base",
      "id_mover_end",
      "id_nonmover_end"
    )
  )

if (nrow(panel_final) != n_before) {
  stop("Row count changed after merging network_panel_clean into panel_final")
}


if (all(c("height_1_base", "height_2_base") %in% names(panel_final))) {
  panel_final <- panel_final %>%
    mutate(
      height_max_base = safe_pmax(height_1_base, height_2_base),
      height_min_base = safe_pmin(height_1_base, height_2_base),
      height_dist_base = safe_absdiff(height_1_base, height_2_base)
    )
}

if (all(c("weight_1_base", "weight_2_base") %in% names(panel_final))) {
  panel_final <- panel_final %>%
    mutate(
      weight_max_base = safe_pmax(weight_1_base, weight_2_base),
      weight_min_base = safe_pmin(weight_1_base, weight_2_base),
      weight_dist_base = safe_absdiff(weight_1_base, weight_2_base)
    )
}

write_dta(panel_final, "data/panel_final.dta")
