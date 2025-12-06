rm(list = ls())

library(dplyr)
library(haven)
library(tidyr)
library(stringr)
library(readr)

df <- read_csv("data/RAT_Raw_Baseline.csv", 
               locale = locale(encoding = "CP949"))

df <- df %>% filter(substr(raw_id, 1, 1) != "D")

prefix_map <- c(
  B = 11,
  P = 12,
  C = 13,
  F = 14,
  M = 15,
  R = 16,
  L = 21,
  Q = 22,
  J = 23,
  A = 24,
  K = 25,
  E = 26
)

df$id <- with(df, {
  prefix <- substr(raw_id, 1, 1)                # 첫 글자
  num_part <- substr(raw_id, 2, nchar(raw_id))  # 뒤 숫자
  as.numeric(paste0(prefix_map[prefix], num_part))
})

############################

answer <- c(
  RAT01 = "우유",
  RAT02 = "단풍",
  RAT03 = "장미",
  RAT04 = "고양이",
  RAT05 = "장갑",
  RAT06 = "호박",
  RAT07 = "오리",
  RAT08 = "피리",
  RAT09 = "떡",
  RAT10 = "구름"
)

rat_cols <- paste0("RAT", sprintf("%02d", 1:10))

# 1. Generous scoring (정답을 포함하는 답안이면 정답)

for (col in rat_cols) {
  df[[paste0(col, "_gen")]] <- as.integer(
    grepl(answer[col], df[[col]], ignore.case = FALSE)
  )
}

df$RAT_score_generous <- rowSums(df[paste0(rat_cols, "_gen")], na.rm = TRUE)

# 2. Strict scoring (완전 일치해야 정답)

for (col in rat_cols) {
  df[[paste0(col, "_strict")]] <- as.integer(
    trimws(df[[col]]) == answer[col]
  )
}

df$RAT_score_strict <- rowSums(df[paste0(rat_cols, "_strict")], na.rm = TRUE)

df <- df[, c("id", "RAT_score_generous", "RAT_score_strict")]

write_dta(df, "data/RAT_score_baseline.dta")

###############################################################################

rm(list = ls())

library(dplyr)
library(haven)
library(tidyr)
library(stringr)
library(readr)

df <- read_csv("data/RAT_Raw_Endline.csv", 
               locale = locale(encoding = "CP949"))

df <- df %>% filter(substr(raw_id, 1, 1) != "D")

prefix_map <- c(
  B = 11,
  P = 12,
  C = 13,
  F = 14,
  M = 15,
  R = 16,
  L = 21,
  Q = 22,
  J = 23,
  A = 24,
  K = 25,
  E = 26
)

df$id <- with(df, {
  prefix <- substr(raw_id, 1, 1)                # 첫 글자
  num_part <- substr(raw_id, 2, nchar(raw_id))  # 뒤 숫자
  as.numeric(paste0(prefix_map[prefix], num_part))
})

answer <- c(
  RAT01 = "참새",
  RAT02 = "곰",
  RAT03 = "악어",
  RAT04 = "진주",
  RAT05 = "무지개",
  RAT06 = "제비",
  RAT07 = "솥",
  RAT08 = "별",
  RAT09 = "단추",
  RAT10 = "코끼리"
)

rat_cols <- paste0("RAT", sprintf("%02d", 1:10))

# 1. Generous scoring (정답을 포함하는 답안이면 정답)

for (col in rat_cols) {
  df[[paste0(col, "_gen")]] <- as.integer(
    grepl(answer[col], df[[col]], ignore.case = FALSE)
  )
}

df$RAT_score_generous <- rowSums(df[paste0(rat_cols, "_gen")], na.rm = TRUE)

# 2. Strict scoring (완전 일치해야 정답)

for (col in rat_cols) {
  df[[paste0(col, "_strict")]] <- as.integer(
    trimws(df[[col]]) == answer[col]
  )
}

df$RAT_score_strict <- rowSums(df[paste0(rat_cols, "_strict")], na.rm = TRUE)

df <- df[, c("id", "RAT_score_generous", "RAT_score_strict")]

write_dta(df, "data/RAT_score_endline.dta")

#########################################################################

rm(list = ls())

library(dplyr)
library(haven)
library(tidyr)
library(stringr)
library(readr)

panel_individual <- read_dta("data/finalized_panel_individual_250831.dta")
panel_final <- read_dta("data/finalized_panel_final_251203.dta")

panel_individual <- panel_individual %>%
  mutate(time = post)

make_long_for_individual <- function(df, var) {
  case1 <- df %>%
    select(group_id, id_x, partner_id_x,
           !!sym(paste0(var, "_1")),
           !!sym(paste0(var, "_2")),
           !!sym(paste0(var, "_1_end")),
           !!sym(paste0(var, "_2_end"))) %>%
    tidyr::pivot_longer(
      cols = c(paste0(var, "_1"), paste0(var, "_2"),
               paste0(var, "_1_end"), paste0(var, "_2_end")),
      names_to = "source", values_to = "val"
    ) %>%
    mutate(
      time = ifelse(grepl("_end$", source), 1, 0),
      role = case_when(
        source %in% c(paste0(var, "_1"), paste0(var, "_1_end")) ~ "i",
        TRUE ~ "j"
      ),
      id = id_x
    )
  
  case2 <- df %>%
    select(group_id, id_x, partner_id_x,
           !!sym(paste0(var, "_1")),
           !!sym(paste0(var, "_2")),
           !!sym(paste0(var, "_1_end")),
           !!sym(paste0(var, "_2_end"))) %>%
    tidyr::pivot_longer(
      cols = c(paste0(var, "_1"), paste0(var, "_2"),
               paste0(var, "_1_end"), paste0(var, "_2_end")),
      names_to = "source", values_to = "val"
    ) %>%
    mutate(
      time = ifelse(grepl("_end$", source), 1, 0),
      role = case_when(
        source %in% c(paste0(var, "_2"), paste0(var, "_2_end")) ~ "i",
        TRUE ~ "j"
      ),
      id = partner_id_x
    )
  
  out <- bind_rows(case1, case2) %>%
    select(group_id, id, time, role, val) %>%
    tidyr::pivot_wider(names_from = role, values_from = val,
                       names_prefix = paste0(var, "_"))
  
  return(out)
}

vars_RAT <- c("RAT_generous", "RAT_strict")


for (v in vars_RAT) {
  long_tbl <- make_long_for_individual(panel_final, v)
  
  panel_individual <- panel_individual %>%
    left_join(long_tbl, by = c("group_id", "id", "time"))
}


write_dta(panel_individual, "data/finalized_panel_individual_251206.dta")



##########################################################


rm(list = ls())

library(dplyr)
library(haven)
library(tidyr)
library(stringr)
library(readr)


panel_final <- read_dta("data/finalized_panel_final_250831.dta")
panel_individual <- read_dta("data/finalized_panel_individual_251206.dta")
panel_pbl <- read_dta("data/finalized_panel_pbl_250831.dta")
RAT_baseline <- read_dta("data/RAT_score_baseline.dta")
RAT_endline <- read_dta("data/RAT_score_endline.dta")


panel_final <- panel_final %>%
  mutate(
    RAT_generous_1 = NA_real_,
    RAT_generous_2 = NA_real_,
    RAT_strict_1   = NA_real_,
    RAT_strict_2   = NA_real_,
    RAT_generous_1_end = NA_real_,
    RAT_generous_2_end = NA_real_,
    RAT_strict_1_end   = NA_real_,
    RAT_strict_2_end   = NA_real_
  )

for (i in 1:nrow(RAT_baseline)) {
  id <- RAT_baseline$id[i]
  gen <- RAT_baseline$RAT_score_generous[i]
  str <- RAT_baseline$RAT_score_strict[i]
  
  match_row <- which(panel_final$id_x == id | panel_final$partner_id_x == id)
  
  if (length(match_row) == 1) {
    if (panel_final$id_x[match_row] == id) {
      panel_final$RAT_generous_1[match_row] <- gen
      panel_final$RAT_strict_1[match_row]   <- str
    } else {
      panel_final$RAT_generous_2[match_row] <- gen
      panel_final$RAT_strict_2[match_row]   <- str
    }
  }
}

for (i in 1:nrow(RAT_endline)) {
  id <- RAT_endline$id[i]
  gen <- RAT_endline$RAT_score_generous[i]
  str <- RAT_endline$RAT_score_strict[i]
  
  match_row <- which(panel_final$id_x == id | panel_final$partner_id_x == id)
  
  if (length(match_row) == 1) {
    if (panel_final$id_x[match_row] == id) {
      panel_final$RAT_generous_1_end[match_row] <- gen
      panel_final$RAT_strict_1_end[match_row]   <- str
    } else {
      panel_final$RAT_generous_2_end[match_row] <- gen
      panel_final$RAT_strict_2_end[match_row]   <- str
    }
  }
}

write_dta(panel_final, "data/finalized_panel_final_251203.dta")

##########################################################

pbl_time0 <- panel_final %>%
  select(
    group_id,
    RAT_generous_1,
    RAT_generous_2,
    RAT_strict_1,
    RAT_strict_2
  ) %>%
  mutate(time = 0L)


pbl_time1 <- panel_final %>%
  select(
    group_id,
    RAT_generous_1_end,
    RAT_generous_2_end,
    RAT_strict_1_end,
    RAT_strict_2_end
  ) %>%
  rename(
    RAT_generous_1 = RAT_generous_1_end,
    RAT_generous_2 = RAT_generous_2_end,
    RAT_strict_1   = RAT_strict_1_end,
    RAT_strict_2   = RAT_strict_2_end
  ) %>%
  mutate(time = 1L)

panel_pbl_rat <- bind_rows(pbl_time0, pbl_time1)

panel_pbl <- panel_pbl %>%
  left_join(panel_pbl_rat, by = c("group_id", "time"))

write_dta(panel_pbl, "data/finalized_panel_pbl_251206.dta")

