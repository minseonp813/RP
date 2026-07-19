# Byunghun Hahn, June 29
## Create Group-Level Panel and Save ##

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

panel_final <- read_dta("data/panel_final.dta")

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
  out %>%
    mutate(
      endline = endline_value,
      post = endline_value,
      time = endline_value,
      .before = 3
    )
}

panel_group <- bind_rows(
  make_group_rows(panel_final, "base"),
  make_group_rows(panel_final, "end")
) %>%
  arrange(group_id, endline)

if (any(is.na(panel_group$class) | panel_group$class == "")) {
  stop("panel_group contains missing class values.")
}

cat("\nCreated panel_group with one class variable.\n")
cat("Rows:", nrow(panel_group), " Columns:", ncol(panel_group), "\n")
cat("Number of classes:", n_distinct(panel_group$class), "\n")

write_dta(panel_group, "data/panel_group.dta")
