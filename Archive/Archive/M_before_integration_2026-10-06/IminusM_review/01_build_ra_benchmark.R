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
code_dir <- dirname(review_dir)
data_dir <- file.path(code_dir, "data")
out_dir <- file.path(review_dir, "outputs", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

plain <- function(x) trimws(as.character(x))

panel <- read_dta(file.path(data_dir, "panel_individual.dta")) |>
  transmute(
    group_id = plain(group_id), post = as.integer(post), class = plain(class),
    id = plain(id), partner_id = plain(partner_id),
    RA_i = as.numeric(RA_i), RA_j = as.numeric(RA_j), RA_g = as.numeric(RA_g)
  )

stopifnot(nrow(panel) == 2608L)
stopifnot(!anyDuplicated(panel[c("group_id", "post", "id")]))

roster <- panel |>
  arrange(post, group_id, id) |>
  group_by(group_id, post) |>
  summarise(
    class = first(class),
    member1_id = first(id), member2_id = last(id),
    RA1 = first(RA_i), RA2 = last(RA_i), RA_g = first(RA_g),
    .groups = "drop"
  )

stopifnot(nrow(roster) == 1304L)
stopifnot(all(table(roster$post) == 652L))

build_wave <- function(targets) {
  donors <- targets |>
    transmute(
      donor_group_id = group_id, donor_class = class,
      donor_RA_g = RA_g
    )
  cross <- merge(targets, donors, by = NULL, sort = FALSE)
  cross <- cross |>
    mutate(
      is_own = as.integer(group_id == donor_group_id),
      same_class = as.integer(class == donor_class),
      numerator1 = (RA1 - donor_RA_g)^2,
      numerator2 = (RA2 - donor_RA_g)^2,
      denominator = numerator1 + numerator2,
      degenerate = as.integer(!is.finite(denominator) | denominator == 0),
      I1_donor = if_else(degenerate == 1L, NA_real_, numerator1 / denominator),
      I2_donor = if_else(degenerate == 1L, NA_real_, numerator2 / denominator)
    ) |>
    select(
      target_group_id = group_id, post, target_class = class,
      member1_id, member2_id, donor_group_id, donor_class,
      is_own, same_class, I1_donor, I2_donor, degenerate
    )
  cross
}

matrix <- bind_rows(lapply(split(roster, roster$post), build_wave)) |>
  arrange(post, target_group_id, donor_group_id)

stopifnot(nrow(matrix) == 850208L)
stopifnot(all(table(interaction(matrix$target_group_id, matrix$post)) == 652L))

summarise_member <- function(block, member) {
  own <- block[block$is_own == 1L, , drop = FALSE]
  donor <- block[block$is_own == 0L, , drop = FALSE]
  values <- donor[[paste0("I", member, "_donor")]]
  outclass <- donor$same_class == 0L
  actual <- own[[paste0("I", member, "_donor")]]
  tibble(
    group_id = own$target_group_id,
    post = own$post,
    class = own$target_class,
    id = own[[paste0("member", member, "_id")]],
    partner_id = own[[paste0("member", 3L - member, "_id")]],
    I_actual = actual,
    M_all_imp = mean(replace(values, is.na(values), 0.5)),
    M_all_drop = mean(values, na.rm = TRUE),
    M_outclass_imp = mean(replace(values[outclass], is.na(values[outclass]), 0.5)),
    M_outclass_drop = mean(values[outclass], na.rm = TRUE),
    n_all = length(values),
    nvalid_all = sum(!is.na(values)),
    degfrac_all = mean(is.na(values))
  )
}

blocks <- split(matrix, interaction(matrix$target_group_id, matrix$post, drop = TRUE))
analysis <- bind_rows(lapply(blocks, function(block) {
  bind_rows(summarise_member(block, 1L), summarise_member(block, 2L))
})) |>
  arrange(post, group_id, id) |>
  mutate(
    Istar_all_imp = I_actual - M_all_imp,
    Istar_all_drop = I_actual - M_all_drop,
    Istar_outclass_imp = I_actual - M_outclass_imp,
    Istar_outclass_drop = I_actual - M_outclass_drop
  )

check <- analysis |>
  group_by(group_id, post) |>
  summarise(
    n = n(),
    sum_M = sum(M_all_imp),
    sum_Istar = sum(Istar_all_imp),
    .groups = "drop"
  )
stopifnot(all(check$n == 2L))
stopifnot(max(abs(check$sum_M - 1)) < 1e-12)
stopifnot(max(abs(check$sum_Istar), na.rm = TRUE) < 1e-12)
stopifnot(all(analysis$n_all == 651L))
stopifnot(sum(!is.na(analysis$I_actual)) == 2604L)

write_csv(matrix, file.path(out_dir, "ra_donor_matrix.csv"), na = "")
write_csv(analysis, file.path(out_dir, "ra_placebo_member_wave.csv"), na = "")
stata <- analysis
names(stata)[names(stata) == "class"] <- "_class"
write_dta(stata, file.path(out_dir, "ra_placebo_member_wave.dta"), version = 14)

cat("RA benchmark complete:\n")
cat("  donor comparisons:", format(nrow(matrix), big.mark = ","), "\n")
cat("  member-wave rows:", nrow(analysis), "\n")
cat("  defined actual distances:", sum(!is.na(analysis$I_actual)), "\n")

