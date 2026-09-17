rm(list = ls())

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
code_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  normalizePath(getwd())
}
replication_dir <- file.path(code_dir, "..", "Code_Replication Package_Upload")
data_dir <- file.path(replication_dir, "data")
result_dir <- file.path(code_dir, "results", "oddeven")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(haven)
library(dplyr)
library(parallel)

source(file.path(replication_dir, "programs", "warshall.R"))
source(file.path(replication_dir, "programs", "ex_cross.R"))

odd_rounds <- c(1, 3, 5, 7, 9, 11, 13, 15, 17)
even_rounds <- c(2, 4, 6, 8, 10, 12, 14, 16, 18)

base_raw <- read_dta(file.path(data_dir, "base_raw.dta"))
end_raw <- read_dta(file.path(data_dir, "end_raw.dta"))
panel <- read_dta(file.path(data_dir, "panel_individual.dta"))

panel <- panel %>%
  mutate(across(c(group_id, id), as.character),
         across(c(post, person), as.integer))

raw <- bind_rows(
  mutate(base_raw, post = 0L),
  mutate(end_raw, post = 1L)
) %>%
  mutate(across(c(group_id, id), as.character),
         round_number = as.integer(round_number)) %>%
  filter(if_all(c(coord_x, coord_y, intercept_x, intercept_y), ~ !is.na(.x)),
         intercept_x != 0, intercept_y != 0)

individual <- raw %>%
  filter(round_number %in% 1:18) %>%
  inner_join(select(panel, group_id, post, id, person),
             by = c("group_id", "post", "id")) %>%
  mutate(role = person)

group <- raw %>%
  filter(round_number %in% 19:36, mover == 1) %>%
  mutate(round_number = round_number - 18L, role = 3L) %>%
  semi_join(distinct(panel, group_id, post), by = c("group_id", "post"))

cols <- c(
  "group_id", "post", "role", "round_number",
  "coord_x", "coord_y", "intercept_x", "intercept_y"
)
round_data <- bind_rows(select(individual, all_of(cols)),
                        select(group, all_of(cols))) %>%
  arrange(group_id, post, role, round_number)

case_id <- paste(round_data$group_id, round_data$post, sep = "_")
cases <- split(round_data, case_id)
stopifnot(all(vapply(cases, nrow, integer(1)) == 54L))

calculate_ihat <- function(d, group_rounds) {
  i <- d[d$role == 1, ]
  j <- d[d$role == 2, ]
  g <- d[d$role == 3 & d$round_number %in% group_rounds, ]

  ex_i <- compute_ex_from_subsets(i, g)
  ex_j <- compute_ex_from_subsets(j, g)
  ex_ij <- compute_ex_from_subsets(rbind(i, j), g)

  c(
    member1 = ihat_from_ex(ex_i, ex_j, ex_ij),
    member2 = ihat_from_ex(ex_j, ex_i, ex_ij)
  )
}

calculate_case <- function(d) {
  all <- calculate_ihat(d, 1:18)
  odd <- calculate_ihat(d, odd_rounds)
  even <- calculate_ihat(d, even_rounds)

  data.frame(
    group_id = d$group_id[1],
    post = d$post[1],
    Ihat_1_all = all[1],
    Ihat_1_odd = odd[1],
    Ihat_1_even = even[1],
    Ihat_2_all = all[2],
    Ihat_2_odd = odd[2],
    Ihat_2_even = even[2]
  )
}

workers <- min(4L, max(1L, detectCores() - 1L))
cl <- makeCluster(workers)
clusterExport(
  cl,
  c(
    "calculate_case", "calculate_ihat", "compute_ex_from_subsets",
    "ex_cross", "cross_garp", "warshall", "ihat_from_ex",
    "odd_rounds", "even_rounds"
  ),
  envir = environment()
)
results <- parLapply(cl, cases, calculate_case)
stopCluster(cl)

pair_wide <- bind_rows(results)

for (s in c("all", "odd", "even")) {
  x <- pair_wide[[paste0("Ihat_1_", s)]]
  y <- pair_wide[[paste0("Ihat_2_", s)]]
  stopifnot(all(abs(x + y - 1) < 2e-6, na.rm = TRUE))
}

member_long <- bind_rows(
  data.frame(
    group_id = pair_wide$group_id,
    post = pair_wide$post,
    person = 1L,
    Ihat_all_check = pair_wide$Ihat_1_all,
    Ihat_ig_odd = pair_wide$Ihat_1_odd,
    Ihat_ig_even = pair_wide$Ihat_1_even
  ),
  data.frame(
    group_id = pair_wide$group_id,
    post = pair_wide$post,
    person = 2L,
    Ihat_all_check = pair_wide$Ihat_2_all,
    Ihat_ig_odd = pair_wide$Ihat_2_odd,
    Ihat_ig_even = pair_wide$Ihat_2_even
  )
)

panel_evenodd <- panel %>%
  mutate(.row_order = row_number()) %>%
  left_join(member_long, by = c("group_id", "post", "person")) %>%
  arrange(.row_order)

finite <- !is.na(panel_evenodd$Ihat_ig) & !is.na(panel_evenodd$Ihat_all_check)
stopifnot(all(panel_evenodd$Ihat_ig[finite] == panel_evenodd$Ihat_all_check[finite]))
stopifnot(identical(is.na(panel_evenodd$Ihat_ig), is.na(panel_evenodd$Ihat_all_check)))

panel_evenodd$Ihat_all_check <- NULL
panel_evenodd$.row_order <- NULL

write_dta(pair_wide, file.path(result_dir, "oddeven_Ihat_pair_wide_R.dta"))
write.csv(pair_wide, file.path(result_dir, "oddeven_Ihat_pair_wide_R.csv"), row.names = FALSE)
write_dta(
  panel_evenodd,
  file.path(data_dir, "panel_individual_evenodd_R.dta"),
  version = 14
)

cat("Saved:", file.path(data_dir, "panel_individual_evenodd_R.dta"), "\n")
cat("The recalculated all-round Ihat matches the stored Ihat_ig exactly.\n")
