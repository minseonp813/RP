# Build the group panel.

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

panel_final <- read_dta("data/panel_final.dta")

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
  "c_Ng_base", "c_Ng_end"
)

missing_ihat_cols <- setdiff(required_ihat_cols, names(panel_final))

if (length(missing_ihat_cols) > 0) {
  stop(
    "panel_final is missing cross-partition index columns: ",
    paste(missing_ihat_cols, collapse = ", ")
  )
}

if (!"class" %in% names(panel_final)) {
  stop("panel_final must contain class. Run the updated merge script first.")
}

panel_final <- panel_final %>%
  mutate(
    group_id = as.character(group_id),
    class = as.character(class)
  )

drop_time_suffix_from_names <- function(nms, suffix) {
  str_replace(nms, paste0("_", suffix, "$"), "")
}

make_group_rows <- function(df, suffix) {
  endline_value <- ifelse(suffix == "end", 1L, 0L)
  suffix_cols <- names(df)[str_ends(names(df), paste0("_", suffix))]
  keep_cols <- c("group_id", "class", suffix_cols)
  out <- df %>%
    select(all_of(keep_cols))
  names(out) <- drop_time_suffix_from_names(names(out), suffix)
  out <- out %>%
    mutate(
      endline = endline_value,
      post = endline_value,
      time = endline_value,
      .before = 3
    )

  defined <- !is.na(out$ccei_1) & !is.na(out$ccei_2)
  out <- out %>%
    mutate(
      HighCCEI_1_both_high = ifelse(defined, as.numeric(ccei_1 >= ccei_2), NA_real_),
      HighCCEI_2_both_high = ifelse(defined, as.numeric(ccei_2 >= ccei_1), NA_real_),
      HighCCEI_1_both_low = ifelse(defined, as.numeric(ccei_1 > ccei_2), NA_real_),
      HighCCEI_2_both_low = ifelse(defined, as.numeric(ccei_2 > ccei_1), NA_real_),
      ccei_gap_12 = ccei_1 - ccei_2,
      ccei_gap_21 = ccei_2 - ccei_1
    )

  for (measure in c("maxmpi", "hm")) {
    score_1 <- out[[paste0(measure, "_1")]]
    score_2 <- out[[paste0(measure, "_2")]]
    score_defined <- !is.na(score_1) & !is.na(score_2)
    label <- if (measure == "maxmpi") "MaxMPI" else "HM"

    out[[paste0("High", label, "_1_both_high")]] <-
      ifelse(score_defined, as.numeric(score_1 <= score_2), NA_real_)
    out[[paste0("High", label, "_2_both_high")]] <-
      ifelse(score_defined, as.numeric(score_2 <= score_1), NA_real_)
    out[[paste0("High", label, "_1_both_low")]] <-
      ifelse(score_defined, as.numeric(score_1 < score_2), NA_real_)
    out[[paste0("High", label, "_2_both_low")]] <-
      ifelse(score_defined, as.numeric(score_2 < score_1), NA_real_)
    out[[paste0(measure, "_gap_12")]] <- score_2 - score_1
    out[[paste0(measure, "_gap_21")]] <- score_1 - score_2
  }
  out
}

panel_group <- bind_rows(
  make_group_rows(panel_final, "base"),
  make_group_rows(panel_final, "end")
) %>%
  arrange(group_id, endline) %>%
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

baseline_only_noncog_stems <- c(
  "class_study", "class_dislike", "class_lonely",
  paste0("selfesteem_", 1:10), "selfesteem",
  "outgoing", "agreeable", "conscientious", "stable", "opened"
)

for (v in baseline_only_noncog_stems) {
  for (p in 1:2) {
    nm <- paste0(v, "_", p)
    if (!nm %in% names(panel_group)) stop("Missing baseline-only variable: ", nm)
    base <- panel_group[panel_group$post == 0, c("group_id", nm)]
    end <- panel_group[panel_group$post == 1, c("group_id", nm)]
    names(base)[2] <- "base_value"
    names(end)[2] <- "end_value"
    chk <- inner_join(base, end, by = "group_id")
    same <- (is.na(chk$base_value) & is.na(chk$end_value)) |
      (!is.na(chk$base_value) & !is.na(chk$end_value) &
         chk$base_value == chk$end_value)
    if (!all(same)) stop("Baseline-only variable changed across waves: ", nm)
  }
}

if (any(is.na(panel_group$class) | panel_group$class == "")) {
  stop("panel_group contains missing class values.")
}

stopifnot(
  all(panel_group$cei_g > 0 & panel_group$cei_g <= 1),
  all(panel_group$cei_g_untempered > 0 & panel_group$cei_g_untempered <= 1),
  all(panel_group$cei_type_a + panel_group$cei_type_b +
        panel_group$cei_type_c + panel_group$cei_type_d == 1)
)

equal_ccei <- !is.na(panel_group$ccei_1) & !is.na(panel_group$ccei_2) &
  panel_group$ccei_1 == panel_group$ccei_2
stopifnot(
  all(panel_group$HighCCEI_1_both_high[equal_ccei] == 1),
  all(panel_group$HighCCEI_2_both_high[equal_ccei] == 1),
  all(panel_group$HighCCEI_1_both_low[equal_ccei] == 0),
  all(panel_group$HighCCEI_2_both_low[equal_ccei] == 0),
  all(panel_group$ccei_gap_12 == -panel_group$ccei_gap_21, na.rm = TRUE)
)

for (measure in c("maxmpi", "hm")) {
  label <- if (measure == "maxmpi") "MaxMPI" else "HM"
  score_1 <- panel_group[[paste0(measure, "_1")]]
  score_2 <- panel_group[[paste0(measure, "_2")]]
  equal <- !is.na(score_1) & !is.na(score_2) & score_1 == score_2
  stopifnot(
    all(panel_group[[paste0("High", label, "_1_both_high")]][equal] == 1),
    all(panel_group[[paste0("High", label, "_2_both_high")]][equal] == 1),
    all(panel_group[[paste0("High", label, "_1_both_low")]][equal] == 0),
    all(panel_group[[paste0("High", label, "_2_both_low")]][equal] == 0),
    all(
      panel_group[[paste0(measure, "_gap_12")]] ==
        -panel_group[[paste0(measure, "_gap_21")]],
      na.rm = TRUE
    )
  )
}

cat("\nCreated panel_group with one class variable.\n")
cat("Rows:", nrow(panel_group), " Columns:", ncol(panel_group), "\n")
cat("Number of classes:", n_distinct(panel_group$class), "\n")

write_dta(panel_group, "data/panel_group.dta")
