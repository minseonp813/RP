################################################################
# 0. 이건 SKIP.

# baseline_final_eu 초기 구성

rm(list = ls())

library(lpSolveAPI)
source("../programs/ccei_eu.R")
source("../programs/cons_eu.R")
source("../programs/eu.R")

load("../data/baseline_raw.RData")

baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)

data_filtered <- baseline_raw[
  baseline_raw$round_number == 2 &
    baseline_raw$partner_id != 0 &
    baseline_raw$id != baseline_raw$partner_id,
]

unique_rows <- data_filtered[!duplicated(data_filtered$id), ]

unique_rows$group_id <- paste0(
  pmax(unique_rows$id, unique_rows$partner_id),
  pmin(unique_rows$id, unique_rows$partner_id)
)

baseline_final_eu <- unique_rows[, c("id", "partner_id", "group_id", "mover")]

# mover 보정
raw_mover <- subset(baseline_raw, round_number == 19 & id != partner_id)
mover_info <- raw_mover[, c("id", "mover")]
mover_info <- mover_info[!duplicated(mover_info$id), ]

baseline_final_eu <- merge(baseline_final_eu, mover_info,
                           by = "id", all.x = TRUE, suffixes = c("", "_from_raw"))

baseline_final_eu$mover <- ifelse(
  is.na(baseline_final_eu$mover) | baseline_final_eu$mover == "",
  baseline_final_eu$mover_from_raw,
  baseline_final_eu$mover
)

baseline_final_eu$mover_from_raw <- NULL

save(baseline_final_eu, file = "../results/baseline_final_eu.RData")



################################################################

# 1. CCEI_1_eu, CCEI_2_eu 계산

rm(list = ls())

# EU 기반 CCEI 계산용
library(lpSolveAPI)
source("../programs/ccei_eu.R")
source("../programs/cons_eu.R")
source("../programs/eu.R")

# 데이터 로드
load("../data/baseline_raw.RData")
load("../results/baseline_final_eu.RData")

# 문자열 정리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final_eu$id <- as.character(baseline_final_eu$id)
baseline_final_eu$partner_id <- as.character(baseline_final_eu$partner_id)

# 필터: 유효한 쌍만 (라운드 조건 없음)
raw_filtered <- subset(
  baseline_raw,
  game_type == "individual" & round_number %in% 1:18 & id != partner_id
)

# 초기화
baseline_final_eu$ccei_1_eu <- NA
baseline_final_eu$ccei_2_eu <- NA
baseline_final_eu$n_1_eu <- NA
baseline_final_eu$n_2_eu <- NA

# CCEI_1_eu / CCEI_2_eu 계산
for (i in seq_len(nrow(baseline_final_eu))) {
  id1 <- baseline_final_eu$id[i]
  id2 <- baseline_final_eu$partner_id[i]
  
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    pi <- matrix(1/2, 2, 1)
    baseline_final_eu$ccei_1_eu[i] <- ccei_eu(p1, x1, pi)
    baseline_final_eu$n_1_eu[i] <- nrow(sub1)
  }
  
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    pi <- matrix(1/2, 2, 1)
    baseline_final_eu$ccei_2_eu[i] <- ccei_eu(p2, x2, pi)
    baseline_final_eu$n_2_eu[i] <- nrow(sub2)
  }
}

# 진단용 변수 제거
baseline_final_eu$n_1_eu <- NULL
baseline_final_eu$n_2_eu <- NULL

# 저장
save(baseline_final_eu, file = "../results/baseline_final_eu.RData")


###################################################################

# 2. CCEI_col_edu 계산

rm(list = ls())

library(lpSolveAPI)
source("../programs/ccei_eu.R")
source("../programs/cons_eu.R")
source("../programs/eu.R")

load("../data/baseline_raw.RData")
load("../results/baseline_final_eu.RData")

baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final_eu$id <- as.character(baseline_final_eu$id)

raw_filtered <- subset(
  baseline_raw,
  round_number %in% 19:36 &
    id != partner_id &
    partner_id != "0"
)

id_list <- sort(unique(raw_filtered$id))
cat("CCEI_g_eu 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- raw_filtered[raw_filtered$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  pi <- matrix(1/2, 2, 1)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_eu(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)  # 진단용 n
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "ccei_g_eu", "n_g_eu")
result_df$id <- as.character(result_df$id)
result_df$ccei_g_eu <- as.numeric(result_df$ccei_g_eu)
result_df$n_g_eu <- as.numeric(result_df$n_g_eu)

# 결과 병합
baseline_final_eu$ccei_g_eu <- NA
baseline_final_eu$n_g_eu <- NA

for (i in 1:nrow(result_df)) {
  id_i <- result_df$id[i]
  val  <- result_df$ccei_g_eu[i]
  nval <- result_df$n_g_eu[i]
  
  match_idx <- which(baseline_final_eu$id == id_i)
  if (length(match_idx) > 0) {
    baseline_final_eu$ccei_g_eu[match_idx] <- val
    baseline_final_eu$n_g_eu[match_idx] <- nval
  }
}

# 진단용 제거
baseline_final_eu$n_g_eu <- NULL

# 저장
save(baseline_final_eu, file = "../results/baseline_final_eu.RData")



################################################################

# 3. CCEI_1g, CCEI_2g 계산


rm(list = ls())

# EU 기반 CCEI 계산용
library(lpSolveAPI)
source("../programs/ccei_eu.R")
source("../programs/cons_eu.R")
source("../programs/eu.R")

# 데이터 로드
load("../data/baseline_raw.RData")
load("../results/baseline_final_eu.RData")

# 문자열 정리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final_eu$id <- as.character(baseline_final_eu$id)
baseline_final_eu$partner_id <- as.character(baseline_final_eu$partner_id)

# 유효 라운드 필터: 개인 1–18, 그룹 19–36
raw_filtered <- subset(
  baseline_raw,
  (game_type == "individual" & round_number %in% 1:18) |
    (game_type == "collective" & round_number %in% 19:36)
)

# 쌍 중복 제거
raw_filtered <- raw_filtered[raw_filtered$id != raw_filtered$partner_id, ]

# 결과 칼럼 초기화
baseline_final_eu$ccei_1g_eu <- NA
baseline_final_eu$ccei_2g_eu <- NA
baseline_final_eu$n_1g_eu <- NA
baseline_final_eu$n_2g_eu <- NA

# 계산 루프
for (i in seq_len(nrow(baseline_final_eu))) {
  id1 <- baseline_final_eu$id[i]
  id2 <- baseline_final_eu$partner_id[i]
  
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    pi <- matrix(1/2, 2, 1)
    baseline_final_eu$ccei_1g_eu[i] <- ccei_eu(p1, x1, pi)
    baseline_final_eu$n_1g_eu[i] <- nrow(sub1)
  }
  
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    pi <- matrix(1/2, 2, 1)
    baseline_final_eu$ccei_2g_eu[i] <- ccei_eu(p2, x2, pi)
    baseline_final_eu$n_2g_eu[i] <- nrow(sub2)
  }
}

# 진단용 제거
baseline_final_eu$n_1g_eu <- NULL
baseline_final_eu$n_2g_eu <- NULL

# 저장
save(baseline_final_eu, file = "../results/baseline_final_eu.RData")





#####################################################################

# 4. high_edu / low_edu 구분

rm(list = ls())

# 데이터 불러오기
load("../results/baseline_final_eu.RData")
load("../data/baseline_raw.RData")

# 문자열 처리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final_eu$id <- as.character(baseline_final_eu$id)
baseline_final_eu$partner_id <- as.character(baseline_final_eu$partner_id)

# 유효 그룹만 사용 (id ≠ partner_id)
baseline_raw <- baseline_raw[baseline_raw$id != baseline_raw$partner_id, ]

# 초기화
baseline_raw$high_eu <- 0
baseline_raw$low_eu <- 0

# 루프: 각 그룹마다 high/low 결정
for (i in 1:nrow(baseline_final_eu)) {
  id1 <- baseline_final_eu$id[i]
  id2 <- baseline_final_eu$partner_id[i]
  
  c1 <- as.numeric(baseline_final_eu$ccei_1_eu[i])
  c2 <- as.numeric(baseline_final_eu$ccei_2_eu[i])
  c1g <- as.numeric(baseline_final_eu$ccei_1g_eu[i])
  c2g <- as.numeric(baseline_final_eu$ccei_2g_eu[i])
  
  # 기본값
  high <- id1
  low <- id2
  
  if (!is.na(c1) && !is.na(c2)) {
    if (c1 > c2) {
      high <- id1; low <- id2
    } else if (c2 > c1) {
      high <- id2; low <- id1
    } else if (!is.na(c1g) && !is.na(c2g)) {
      if (c1g > c2g) {
        high <- id1; low <- id2
      } else if (c2g > c1g) {
        high <- id2; low <- id1
      }
      # tie일 경우 default 유지
    }
  }
  
  baseline_raw$high_eu[baseline_raw$id == high] <- 1
  baseline_raw$low_eu[baseline_raw$id == low] <- 1
}

# 저장
save(baseline_raw, file = "../data/baseline_raw.RData")




#######################################################

# 1. 데이터 불러오기
load("../data/baseline_raw.RData")
load("../results/baseline_final_eu.RData")

# 문자열 처리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_final_eu$id <- as.character(baseline_final_eu$id)
baseline_final_eu$group_id <- as.character(baseline_final_eu$group_id)

# 2. baseline_raw와 group_id 머지 (id 기준)
baseline_raw <- merge(
  baseline_raw, 
  baseline_final_eu[, c("id", "group_id")],
  by = "id", all.x = TRUE
)

# 3. 필요한 행만 선택 (id ≠ partner_id & group_id 존재)
df <- baseline_raw[baseline_raw$partner_id != "0" & !is.na(baseline_raw$group_id), ]

# 4. 중복 제거 (group 단위로 1명만 유지)
df_unique <- df[!duplicated(df$group_id), ]

# 5. high vs high_eu 테이블
tbl <- df_unique[, c("group_id", "high", "high_eu")]
tbl <- tbl[!is.na(tbl$high) & !is.na(tbl$high_eu), ]
xtab <- table(high = tbl$high, high_eu = tbl$high_eu)

# 6. 출력
cat("Confusion table of high vs high_eu by group (corrected group_id):\n")
print(xtab)

# 시각화용 테이블 구성
library(gt)

xtab_df <- as.data.frame.matrix(xtab)
total <- sum(xtab_df)

xtab_df <- tibble::rownames_to_column(xtab_df, var = "high")
xtab_df$high <- paste0("high: ", xtab_df$high)

colnames(xtab_df)[2:3] <- c("high_eu: 0", "high_eu: 1")

xtab_pct <- xtab
xtab_pct[] <- round(100 * xtab / total, 1)

xtab_df[["high_eu: 0"]] <- paste0(xtab[ , "0"], " (", xtab_pct[ , "0"], "%)")
xtab_df[["high_eu: 1"]] <- paste0(xtab[ , "1"], " (", xtab_pct[ , "1"], "%)")

row_sums <- rowSums(xtab)
col_sums <- colSums(xtab)

gt_tbl <- xtab_df %>%
  gt() %>%
  tab_header(
    title = "Confusion Matrix: High vs High_EU",
    subtitle = "Grouped by unique group_id"
  ) %>%
  cols_label(high = "") %>%
  fmt_markdown(columns = everything()) %>%
  summary_rows(
    groups = NULL,
    columns = c("high_eu: 0", "high_eu: 1"),
    fns = list(Total = ~c(
      paste0(col_sums[1], " (", round(100 * col_sums[1]/total, 1), "%)"),
      paste0(col_sums[2], " (", round(100 * col_sums[2]/total, 1), "%)")
    )),
    missing_text = ""
  ) %>%
  tab_spanner(
    label = "Predicted: high_eu",
    columns = c("high_eu: 0", "high_eu: 1")
  )

# 출력
gt_tbl



#####################################################

# 7. CCEI_hg_eu 결정


rm(list = ls())

# EU 기반
library(lpSolveAPI)
source("../programs/ccei_eu.R")
source("../programs/cons_eu.R")
source("../programs/eu.R")

data_name <- "baseline_raw"
result_name <- "garp_baseline_hg_eu"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

# 조건: high_eu + 유효 라운드
data <- data[data$high_eu == 1, ]
data <- data[data$round_number %in% c(1:18, 19:36), ]

id_list <- sort(unique(data$id))
cat("CCEI 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- data[data$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  pi <- matrix(1/2, 2, 1)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_eu(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "ccei_hg_eu", "n_used_hg_eu")
result_df$id <- as.character(result_df$id)
result_df$ccei_hg_eu <- as.numeric(result_df$ccei_hg_eu)
result_df$n_used_hg_eu <- as.numeric(result_df$n_used_hg_eu)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/baseline_final_eu.RData")

df_hg <- result_df
baseline_final_eu$id <- as.character(baseline_final_eu$id)
baseline_final_eu$partner_id <- as.character(baseline_final_eu$partner_id)
baseline_final_eu$ccei_hg_eu <- NA

for (i in 1:nrow(df_hg)) {
  id_i <- df_hg$id[i]
  val  <- df_hg$ccei_hg_eu[i]
  
  match_idx <- which(baseline_final_eu$id == id_i)
  if (length(match_idx) > 0) {
    baseline_final_eu$ccei_hg_eu[match_idx] <- val
    next
  }
  
  match_idx <- which(baseline_final_eu$partner_id == id_i)
  if (length(match_idx) > 0) {
    baseline_final_eu$ccei_hg_eu[match_idx] <- val
    next
  }
}

# ------------ 검증 --------------------------

baseline_final_eu$ccei_1g_eu <- as.numeric(baseline_final_eu$ccei_1g_eu)
baseline_final_eu$ccei_2g_eu <- as.numeric(baseline_final_eu$ccei_2g_eu)
baseline_final_eu$ccei_hg_eu <- as.numeric(baseline_final_eu$ccei_hg_eu)

epsilon <- 1e-8
baseline_final_eu$match_hg_eu <- ifelse(
  !is.na(baseline_final_eu$ccei_hg_eu) &
    (abs(baseline_final_eu$ccei_hg_eu - baseline_final_eu$ccei_1g_eu) < epsilon |
       abs(baseline_final_eu$ccei_hg_eu - baseline_final_eu$ccei_2g_eu) < epsilon),
  1, 0
)

cat("match_hg_eu 결과:\n")
print(table(baseline_final_eu$match_hg_eu))

# 저장
save(baseline_final_eu, file = "../results/baseline_final_eu.RData")





#####################################################

# 8. CCEI_lg_eu 결정

rm(list = ls())

# EU 기반
library(lpSolveAPI)
source("../programs/ccei_eu.R")
source("../programs/cons_eu.R")
source("../programs/eu.R")

data_name <- "baseline_raw"
result_name <- "garp_baseline_lg_eu"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

# 조건: low_eu + 유효 라운드
data <- data[data$low_eu == 1, ]
data <- data[data$round_number %in% c(1:18, 19:36), ]

id_list <- sort(unique(data$id))
cat("CCEI_lg_eu 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- data[data$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  pi <- matrix(1/2, 2, 1)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_eu(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "ccei_lg_eu", "n_used_lg_eu")
result_df$id <- as.character(result_df$id)
result_df$ccei_lg_eu <- as.numeric(result_df$ccei_lg_eu)
result_df$n_used_lg_eu <- as.numeric(result_df$n_used_lg_eu)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/baseline_final_eu.RData")

df_lg <- result_df
baseline_final_eu$id <- as.character(baseline_final_eu$id)
baseline_final_eu$partner_id <- as.character(baseline_final_eu$partner_id)
baseline_final_eu$ccei_lg_eu <- NA

for (i in 1:nrow(df_lg)) {
  id_i <- df_lg$id[i]
  val  <- df_lg$ccei_lg_eu[i]
  
  match_idx <- which(baseline_final_eu$id == id_i)
  if (length(match_idx) > 0) {
    baseline_final_eu$ccei_lg_eu[match_idx] <- val
    next
  }
  
  match_idx <- which(baseline_final_eu$partner_id == id_i)
  if (length(match_idx) > 0) {
    baseline_final_eu$ccei_lg_eu[match_idx] <- val
    next
  }
}

# ------------ 검증 --------------------------

baseline_final_eu$ccei_1g_eu <- as.numeric(baseline_final_eu$ccei_1g_eu)
baseline_final_eu$ccei_2g_eu <- as.numeric(baseline_final_eu$ccei_2g_eu)
baseline_final_eu$ccei_lg_eu <- as.numeric(baseline_final_eu$ccei_lg_eu)

epsilon <- 1e-8

baseline_final_eu$match_lg_eu <- ifelse(
  !is.na(baseline_final_eu$ccei_lg_eu) &
    (abs(baseline_final_eu$ccei_lg_eu - baseline_final_eu$ccei_1g_eu) < epsilon |
       abs(baseline_final_eu$ccei_lg_eu - baseline_final_eu$ccei_2g_eu) < epsilon),
  1, 0
)

cat("match_lg_eu 결과:\n")
print(table(baseline_final_eu$match_lg_eu))

# 불필요한 변수 제거
baseline_final_eu$match_lg_eu <- NULL
baseline_final_eu$n_used_lg_eu <- NULL

save(baseline_final_eu, file = "../results/baseline_final_eu.RData")


################################################################

# 9. CCEI_hlg_eu 계산 및 병합

rm(list = ls())

# EU 기반
library(lpSolveAPI)
source("../programs/ccei_eu.R")
source("../programs/cons_eu.R")
source("../programs/eu.R")

# 변수 정의
data_name <- "baseline_raw"
result_name <- "garp_baseline_hlg_eu"

# 데이터 로드
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)

# 문자열 변환
data$id <- as.character(data$id)
data$partner_id <- as.character(data$partner_id)
data$game_type <- as.character(data$game_type)
data$mover <- as.character(data$mover)

# partner_id 보정: round 19 기준
library(dplyr)
data <- data %>%
  group_by(id) %>%
  mutate(partner_id = ifelse(round_number %in% 1:36,
                             partner_id[round_number == 19][1],
                             partner_id)) %>%
  ungroup()

# 라운드 필터
data <- data[data$round_number %in% c(1:18, 19:36), ]

# 유효 쌍만
filtered_data <- data[data$id != data$partner_id, ]

# group_id 생성
filtered_data$group_id <- paste0(
  pmax(filtered_data$id, filtered_data$partner_id),
  pmin(filtered_data$id, filtered_data$partner_id)
)

# 개인 결정
data_indiv <- filtered_data[filtered_data$game_type == "individual", ]

# 공동 결정 중 mover == "t"만
collective_data <- filtered_data[filtered_data$game_type == "collective" & filtered_data$mover == "t", ]

# 병합
data_combined <- rbind(data_indiv, collective_data)

# 계산
group_list <- sort(unique(data_combined$group_id))
result_matrix <- matrix(NA, nrow = length(group_list), ncol = 3)

for (i in seq_along(group_list)) {
  g <- group_list[i]
  sub <- data_combined[data_combined$group_id == g, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  pi <- matrix(1/2, 2, 1)
  
  result_matrix[i, 1] <- g
  result_matrix[i, 2] <- ccei_eu(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("group_id", "ccei_hlg_eu", "n_used_hlg_eu")

result_df$group_id <- as.character(result_df$group_id)
result_df$ccei_hlg_eu <- as.numeric(result_df$ccei_hlg_eu)
result_df$n_used_hlg_eu <- as.numeric(result_df$n_used_hlg_eu)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# 병합
load("../results/baseline_final_eu.RData")

baseline_final_eu$group_id <- as.character(baseline_final_eu$group_id)

baseline_final_eu <- merge(
  baseline_final_eu, result_df,
  by = "group_id", all.x = TRUE
)

# 진단 변수 제거
baseline_final_eu$n_used_hlg_eu <- NULL

save(baseline_final_eu, file = "../results/baseline_final_eu.RData")





#################################################################

# 10. index 만들기 (eu 버전)

rm(list = ls())

# 데이터 로드
load("../results/baseline_final_eu.RData")

# 숫자형 변환
baseline_final_eu$ccei_g_eu    <- as.numeric(baseline_final_eu$ccei_g_eu)
baseline_final_eu$ccei_hg_eu   <- as.numeric(baseline_final_eu$ccei_hg_eu)
baseline_final_eu$ccei_lg_eu   <- as.numeric(baseline_final_eu$ccei_lg_eu)
baseline_final_eu$ccei_hlg_eu  <- as.numeric(baseline_final_eu$ccei_hlg_eu)

# 지수 계산
baseline_final_eu$index_hg_eu <- with(
  baseline_final_eu,
  (ccei_g_eu - ccei_hg_eu) / (ccei_g_eu - ccei_hlg_eu)
)

baseline_final_eu$index_lg_eu <- with(
  baseline_final_eu,
  (ccei_g_eu - ccei_lg_eu) / (ccei_g_eu - ccei_hlg_eu)
)

# 변수 순서 정렬
desired_order <- c(
  "id", "partner_id", "group_id", "mover",
  "ccei_1_eu", "ccei_2_eu", "ccei_g_eu",
  "ccei_1g_eu", "ccei_2g_eu",
  "ccei_hg_eu", "ccei_lg_eu", "ccei_hlg_eu",
  "index_hg_eu", "index_lg_eu"
)

baseline_final_eu <- baseline_final_eu[, desired_order]

# NaN 개수 확인
cat("NaN 개수 확인:\n")
print(colSums(sapply(
  baseline_final_eu[, c("index_hg_eu", "index_lg_eu")],
  function(x) is.nan(x)
)))

# 저장
save(baseline_final_eu, file = "../results/baseline_final_eu.RData")



#################################################################

# 11. 분포, 평균, 중앙값

library(ggplot2)
library(dplyr)

rm(list = ls())
load("../results/baseline_final_eu.RData")

df <- baseline_final_eu
df <- df[!is.na(df$index_hg_eu), ]

# 세부 히스토그램 계산
hist_detail <- hist(df$index_hg_eu, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 0.2 단위 bin 그룹
df$bin_group <- cut(df$index_hg_eu, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
group_props <- df %>%
  group_by(bin_group) %>%
  summarise(n = n()) %>%
  mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.9, by = 0.2)
  )

# 평균 및 중앙값
x_mean <- mean(df$index_hg_eu)
x_median <- median(df$index_hg_eu)
y_max <- max(count_detail)

# 시각화
ggplot(df, aes(x = index_hg_eu)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "gray70", color = "black") +
  
  geom_text(
    data = data.frame(x = mid_detail, y = count_detail / 2,
                      label = ifelse(prop_detail >= 5, paste0(prop_detail, "%"), "")),
    aes(x = x, y = y, label = label),
    size = 4, fontface = "bold"
  ) +
  
  geom_text(
    data = group_props,
    aes(x = mid, y = y_max * 1.05, label = paste0(prop, "%")),
    size = 4, fontface = "bold"
  ) +
  
  geom_vline(xintercept = x_mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = x_median, color = "blue", linetype = "dashed") +
  
  annotate("text", x = x_mean, y = y_max * 0.5,
           label = paste0("mean = ", round(x_mean, 3)),
           color = "red", size = 4, hjust = 0.5) +
  annotate("text", x = x_median, y = y_max * 0.35,
           label = paste0("median = ", round(x_median, 3)),
           color = "blue", size = 4, hjust = 0.5) +
  
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    limits = c(0, 1),
    minor_breaks = NULL
  ) +
  
  labs(
    title = expression(italic("Distribution of  I"[h*",g"]^eu)),
    x = expression(I[h*",g"]^eu),
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )

# NaN 개수 출력
sum(is.nan(baseline_final_eu$index_hg_eu))



###############################################################

library(ggplot2)
library(dplyr)

# 데이터 로드
rm(list = ls())
load("../results/baseline_final_eu.RData")

df <- baseline_final_eu
df <- df[!is.na(df$index_lg_eu), ]

# 세부 bin (0.05 간격, 20개)
hist_detail <- hist(df$index_lg_eu, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 상단 구간 (0.2 간격, 5개)
df$bin_group <- cut(df$index_lg_eu, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
group_props <- df %>%
  group_by(bin_group) %>%
  summarise(n = n()) %>%
  mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.9, by = 0.2)
  )

# 평균 및 중앙값
x_mean <- mean(df$index_lg_eu)
x_median <- median(df$index_lg_eu)
y_max <- max(count_detail)

# 플롯
p <- ggplot(df, aes(x = index_lg_eu)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "gray70", color = "black") +
  
  geom_text(
    data = data.frame(x = mid_detail, y = count_detail / 2,
                      label = ifelse(prop_detail >= 5, paste0(prop_detail, "%"), "")),
    aes(x = x, y = y, label = label),
    size = 4, fontface = "bold"
  ) +
  
  geom_text(
    data = group_props,
    aes(x = mid, y = y_max * 1.05, label = paste0(prop, "%")),
    size = 4, fontface = "bold"
  ) +
  
  geom_vline(xintercept = x_mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = x_median, color = "blue", linetype = "dashed") +
  
  annotate("text", x = x_mean, y = y_max * 0.5,
           label = paste0("mean = ", round(x_mean, 3)),
           color = "red", size = 4, hjust = 0.5) +
  annotate("text", x = x_median, y = y_max * 0.35,
           label = paste0("median = ", round(x_median, 3)),
           color = "blue", size = 4, hjust = 0.5) +
  
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    limits = c(0, 1),
    minor_breaks = NULL
  ) +
  
  labs(
    title = expression(italic("Distribution of  I"[l*",g"]^eu)),
    x = expression(I[l*",g"]^eu),
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )

# 출력
print(p)


########################################################

library(ggplot2)

# 데이터 로드
rm(list = ls())
load("../results/baseline_final_eu.RData")

df <- baseline_final_eu

# 수치형 변환
cols <- c("ccei_1_eu", "ccei_2_eu", "ccei_g_eu", "ccei_hg_eu", "ccei_lg_eu", "ccei_hlg_eu")
df[cols] <- lapply(df[cols], function(x) as.numeric(as.character(x)))

# high / low 계산
df$ccei_h_eu <- pmax(df$ccei_1_eu, df$ccei_2_eu)
df$ccei_l_eu <- pmin(df$ccei_1_eu, df$ccei_2_eu)

# 평균값 계산
mean_df <- data.frame(
  type = factor(c("ccei_h_eu", "ccei_l_eu", "ccei_g_eu", "ccei_hg_eu", "ccei_lg_eu", "ccei_hlg_eu"),
                levels = c("ccei_h_eu", "ccei_l_eu", "ccei_g_eu", "ccei_hg_eu", "ccei_lg_eu", "ccei_hlg_eu")),
  value = c(
    mean(df$ccei_h_eu, na.rm = TRUE),
    mean(df$ccei_l_eu, na.rm = TRUE),
    mean(df$ccei_g_eu, na.rm = TRUE),
    mean(df$ccei_hg_eu, na.rm = TRUE),
    mean(df$ccei_lg_eu, na.rm = TRUE),
    mean(df$ccei_hlg_eu, na.rm = TRUE)
  )
)

ggplot(mean_df, aes(x = type, y = value, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  
  # 나머지 값: 점 위
  geom_text(
    data = subset(mean_df, type != "ccei_h_eu"),
    aes(label = round(value, 3)),
    vjust = -1,
    size = 4
  ) +
  
  # ccei_h_eu만 점 아래
  geom_text(
    data = subset(mean_df, type == "ccei_h_eu"),
    aes(label = round(value, 3)),
    vjust = 1.5,
    size = 4
  ) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Average CCEI by Type (EU-based)",
    x = NULL,
    y = "Average CCEI"
  ) +
  theme_minimal(base_size = 14)
