rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(readr)
})

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
review_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  normalizePath(getwd())
}
build_stability <- function(measure) {
  sample_size <- if (measure == "hm") 50L else 20L
  half_size <- sample_size / 2L
  matrix_file <- file.path(
    review_dir, "outputs", "benchmarks", paste0(measure, "_sample", sample_size),
    "placebo_donor_matrix.csv"
  )
  out_file <- file.path(
    review_dir, "outputs", "data", paste0(measure, "_sample_stability.dta")
  )
  matrix <- read_csv(
    matrix_file,
    col_types = cols(
      target_group_id = col_character(), member1_id = col_character(),
      member2_id = col_character(), donor_group_id = col_character(), .default = col_double()
    ),
    show_col_types = FALSE
  )
  blocks <- split(matrix, interaction(matrix$target_group_id, matrix$post, drop = TRUE))
  rows <- lapply(blocks, function(block) {
    own <- block[block$is_own == 1, , drop = FALSE]
    donors <- block[block$is_own == 0, , drop = FALSE]
    stopifnot(nrow(own) == 1L, nrow(donors) == sample_size)
    bind_rows(lapply(1:2, function(member) {
      values <- donors[[paste0("Ihat", member, "_donor")]]
      values[is.na(values)] <- .5
      tibble(
        group_id = own$target_group_id,
        post = as.integer(own$post),
        id = own[[paste0("member", member, "_id")]],
        I_actual = own[[paste0("Ihat", member, "_donor")]],
        M_half1 = mean(values[seq_len(half_size)]),
        M_half2 = mean(values[(half_size + 1L):sample_size]),
        M_fullsample = mean(values),
        Istar_half1 = I_actual - M_half1,
        Istar_half2 = I_actual - M_half2,
        Istar_fullsample = I_actual - M_fullsample
      )
    }))
  }) |>
    bind_rows() |>
    arrange(post, group_id, id)
  check <- rows |>
    group_by(group_id, post) |>
    summarise(across(starts_with("M_"), sum), across(starts_with("Istar_"), sum), .groups = "drop")
  stopifnot(max(abs(check$M_half1 - 1)) < 1e-12)
  stopifnot(max(abs(check$M_half2 - 1)) < 1e-12)
  stopifnot(max(abs(check$Istar_half1), na.rm = TRUE) < 1e-12)
  stopifnot(max(abs(check$Istar_half2), na.rm = TRUE) < 1e-12)
  stopifnot(max(abs(rows$M_fullsample - (rows$M_half1 + rows$M_half2) / 2)) < 1e-12)
  write_dta(rows, out_file, version = 14)
  cat(toupper(measure), "split-half benchmark written:", out_file, "\n")
}

measures <- trimws(strsplit(Sys.getenv("STABILITY_MEASURES", "hm"), ",")[[1]])
invisible(lapply(measures, build_stability))
