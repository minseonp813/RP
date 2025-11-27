# 0. F_baseline_final 데이터 구축

rm(list = ls())

# 함수 로드
source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/baseline_raw.RData")

# 문자열 처리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)

# 1. round_number == 2 기준, 유효한 쌍 필터링
data_filtered <- baseline_raw[
  baseline_raw$round_number == 2 &
    baseline_raw$partner_id != 0 &
    baseline_raw$id != baseline_raw$partner_id,
]

# 2. group_id 생성
data_filtered$group_id <- paste0(
  pmax(data_filtered$id, data_filtered$partner_id),
  pmin(data_filtered$id, data_filtered$partner_id)
)

# 3. group_id 기준 중복 제거
unique_groups <- data_filtered[!duplicated(data_filtered$group_id), ]

# 4. 초기 구성
F_baseline_final <- unique_groups[, c("id", "partner_id", "group_id", "mover")]

# 5. mover 정보 보정 (round_number == 19 기준)
raw_mover <- subset(baseline_raw, round_number == 19 & id != partner_id)
mover_info <- raw_mover[, c("id", "mover")]
mover_info <- mover_info[!duplicated(mover_info$id), ]

F_baseline_final <- merge(
  F_baseline_final, mover_info,
  by = "id", all.x = TRUE, suffixes = c("", "_from_raw")
)

# 6. 기존 mover 값이 비어있으면 보정값으로 채움
F_baseline_final$mover <- ifelse(
  is.na(F_baseline_final$mover) | F_baseline_final$mover == "",
  F_baseline_final$mover_from_raw,
  F_baseline_final$mover
)

# 7. 임시 열 제거
F_baseline_final$mover_from_raw <- NULL

# 저장
save(F_baseline_final, file = "../results/F_baseline_final.RData")



################################################################

# 1. F_CCEI_1, F_CCEI_2 계산 (Baseline)

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/baseline_raw.RData")
load("../results/F_baseline_final.RData")
load("../results/baseline_final.RData")

# 문자열 정리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
F_baseline_final$id <- as.character(F_baseline_final$id)
F_baseline_final$partner_id <- as.character(F_baseline_final$partner_id)

# 개인 게임만 추출 (1~18 라운드)
raw_filtered <- subset(
  baseline_raw,
  game_type == "individual" & round_number %in% 1:18 & id != partner_id
)

# F_CCEI 변수 초기화
F_baseline_final$f_ccei_1 <- NA
F_baseline_final$f_ccei_2 <- NA
F_baseline_final$n_1 <- NA
F_baseline_final$n_2 <- NA

# 상태 확률 (동일 확률: 1/2 each)
pi <- matrix(1/2, 2, 1)

# 루프 실행
for (i in seq_len(nrow(F_baseline_final))) {
  id1 <- F_baseline_final$id[i]
  id2 <- F_baseline_final$partner_id[i]
  
  # id1 처리
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    F_baseline_final$f_ccei_1[i] <- ccei_fgarp(p1, x1, pi)
    F_baseline_final$n_1[i] <- nrow(sub1)
  }
  
  # id2 처리
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    F_baseline_final$f_ccei_2[i] <- ccei_fgarp(p2, x2, pi)
    F_baseline_final$n_2[i] <- nrow(sub2)
  }
}

# 진단용 변수 제거
F_baseline_final$n_1 <- NULL
F_baseline_final$n_2 <- NULL

# 결과 저장
save(F_baseline_final, file = "../results/F_baseline_final.RData")


################################################################

# 2. F_CCEI_col 계산 (Baseline)

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/baseline_raw.RData")
load("../results/F_baseline_final.RData")

# 문자열 정리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
F_baseline_final$id <- as.character(F_baseline_final$id)

# 집단 결정 라운드 19–36 필터
raw_filtered <- subset(
  baseline_raw,
  round_number >= 19 & round_number <= 36 &
    id != partner_id &
    partner_id != "0"
)

id_list <- sort(unique(raw_filtered$id))
cat("F_CCEI_col 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

# 상태 확률: 두 상태가 1/2씩
pi <- matrix(1/2, 2, 1)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- raw_filtered[raw_filtered$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_fgarp(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "f_ccei_col", "n_col")
result_df$id <- as.character(result_df$id)
result_df$f_ccei_col <- as.numeric(result_df$f_ccei_col)
result_df$n_col <- as.numeric(result_df$n_col)

# 결과 병합
F_baseline_final$f_ccei_col <- NA
F_baseline_final$n_col <- NA

for (i in 1:nrow(result_df)) {
  id_i <- result_df$id[i]
  val  <- result_df$f_ccei_col[i]
  nval <- result_df$n_col[i]
  
  match_idx <- which(F_baseline_final$id == id_i)
  if (length(match_idx) > 0) {
    F_baseline_final$f_ccei_col[match_idx] <- val
    F_baseline_final$n_col[match_idx] <- nval
  }
}

# 진단용 변수 제거
F_baseline_final$n_col <- NULL

# 결과 저장
save(F_baseline_final, file = "../results/F_baseline_final.RData")


###################################################################

# F_CCEI_1g, F_CCEI_2g 계산 (Baseline)

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/baseline_raw.RData")
load("../results/F_baseline_final.RData")

# 문자열 변환
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
F_baseline_final$id <- as.character(F_baseline_final$id)
F_baseline_final$partner_id <- as.character(F_baseline_final$partner_id)

# 유효 라운드 필터: 개인 1–18 + 집단 19–36
raw_filtered <- subset(
  baseline_raw,
  (game_type == "individual" & round_number %in% 1:18) |
    (game_type == "collective" & round_number %in% 19:36)
)

# 쌍 중복 제거
raw_filtered <- raw_filtered[raw_filtered$id != raw_filtered$partner_id, ]

# 결과 칼럼 초기화
F_baseline_final$f_ccei_1g <- NA
F_baseline_final$f_ccei_2g <- NA
F_baseline_final$n_1g <- NA
F_baseline_final$n_2g <- NA

# 상태 확률 고정 (두 상태 모두 1/2)
pi <- matrix(1/2, 2, 1)

# 계산 루프
for (i in seq_len(nrow(F_baseline_final))) {
  id1 <- F_baseline_final$id[i]
  id2 <- F_baseline_final$partner_id[i]
  
  # id1
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    F_baseline_final$f_ccei_1g[i] <- ccei_fgarp(p1, x1, pi)
    F_baseline_final$n_1g[i] <- nrow(sub1)
  }
  
  # id2
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    F_baseline_final$f_ccei_2g[i] <- ccei_fgarp(p2, x2, pi)
    F_baseline_final$n_2g[i] <- nrow(sub2)
  }
}

# 진단용 변수 제거
F_baseline_final$n_1g <- NULL
F_baseline_final$n_2g <- NULL

# 저장
save(F_baseline_final, file = "../results/F_baseline_final.RData")


################################################################

# F-GARP 기준 high/low 구분 (Baseline)

# 초기화
rm(list = ls())

# 데이터 불러오기
load("../results/F_baseline_final.RData")
load("../data/baseline_raw.RData")

# 문자열 처리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
F_baseline_final$id <- as.character(F_baseline_final$id)
F_baseline_final$partner_id <- as.character(F_baseline_final$partner_id)

# 유효 그룹만 사용 (id ≠ partner_id)
baseline_raw <- baseline_raw[baseline_raw$id != baseline_raw$partner_id, ]

# f_high / f_low 초기화
baseline_raw$f_high <- 0
baseline_raw$f_low <- 0

# 루프: 각 그룹마다 high/low 결정
for (i in 1:nrow(F_baseline_final)) {
  id1 <- F_baseline_final$id[i]
  id2 <- F_baseline_final$partner_id[i]
  
  f1 <- as.numeric(F_baseline_final$f_ccei_1[i])
  f2 <- as.numeric(F_baseline_final$f_ccei_2[i])
  f1g <- as.numeric(F_baseline_final$f_ccei_1g[i])
  f2g <- as.numeric(F_baseline_final$f_ccei_2g[i])
  
  high <- id1
  low <- id2
  
  if (!is.na(f1) && !is.na(f2)) {
    if (f1 > f2) {
      high <- id1; low <- id2
    } else if (f2 > f1) {
      high <- id2; low <- id1
    } else if (!is.na(f1g) && !is.na(f2g)) {
      if (f1g > f2g) {
        high <- id1; low <- id2
      } else if (f2g > f1g) {
        high <- id2; low <- id1
      } else {
        high <- id1; low <- id2  # fallback
      }
    }
  }
  
  # 결과 저장
  baseline_raw$f_high[baseline_raw$id == high] <- 1
  baseline_raw$f_low[baseline_raw$id == low] <- 1
}

# 저장
save(baseline_raw, file = "../data/baseline_raw.RData")



#####################################################################

# baseline: f_ccei_hg 계산

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

# 변수 정의
data_name <- "baseline_raw"
result_name <- "fgarp_baseline_hg"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

# ▶️ F-GARP 기준 high: f_high == 1만 선택
data <- data[data$f_high == 1, ]

# 유효 라운드 필터: 개인 1~18 + 집단 19~36
data <- data[data$round_number %in% 1:36, ]

# ID 리스트
id_list <- sort(unique(data$id))
cat("F_CCEI_hg 계산 대상 ID 수:", length(id_list), "\n")

# 결과 초기화
result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)
pi <- matrix(1/2, 2, 1)  # 상태 확률

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- data[data$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_fgarp(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "f_ccei_hg", "n_hg")
result_df$id <- as.character(result_df$id)
result_df$f_ccei_hg <- as.numeric(result_df$f_ccei_hg)
result_df$n_hg <- as.numeric(result_df$n_hg)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/F_baseline_final.RData")

df_hg <- result_df
F_baseline_final$id <- as.character(F_baseline_final$id)
F_baseline_final$partner_id <- as.character(F_baseline_final$partner_id)
F_baseline_final$f_ccei_hg <- NA
F_baseline_final$n_hg <- NA

for (i in 1:nrow(df_hg)) {
  id_i <- df_hg$id[i]
  val  <- df_hg$f_ccei_hg[i]
  nval <- df_hg$n_hg[i]
  
  match_idx <- which(F_baseline_final$id == id_i)
  if (length(match_idx) > 0) {
    F_baseline_final$f_ccei_hg[match_idx] <- val
    F_baseline_final$n_hg[match_idx] <- nval
    next
  }
  
  match_idx <- which(F_baseline_final$partner_id == id_i)
  if (length(match_idx) > 0) {
    F_baseline_final$f_ccei_hg[match_idx] <- val
    F_baseline_final$n_hg[match_idx] <- nval
    next
  }
}

# 진단용 제거
F_baseline_final$n_hg <- NULL

# ------------ 검증 --------------------------

F_baseline_final$f_ccei_1g <- as.numeric(F_baseline_final$f_ccei_1g)
F_baseline_final$f_ccei_2g <- as.numeric(F_baseline_final$f_ccei_2g)
F_baseline_final$f_ccei_hg <- as.numeric(F_baseline_final$f_ccei_hg)

epsilon <- 1e-8

F_baseline_final$match_f_hg <- ifelse(
  !is.na(F_baseline_final$f_ccei_hg) &
    (abs(F_baseline_final$f_ccei_hg - F_baseline_final$f_ccei_1g) < epsilon |
       abs(F_baseline_final$f_ccei_hg - F_baseline_final$f_ccei_2g) < epsilon),
  1, 0
)

cat("match_f_hg 결과:\n")
print(table(F_baseline_final$match_f_hg))

# 검증용 제거
F_baseline_final$match_f_hg <- NULL

# 저장
save(F_baseline_final, file = "../results/F_baseline_final.RData")



#####################################################

# 8. CCEI_lg 계산 (F-GARP, baseline 기준)

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

data_name <- "baseline_raw"
result_name <- "fgarp_baseline_lg"

# 데이터 로드
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

# ▶️ F-GARP 기준: f_low == 1
data <- data[data$f_low == 1, ]

# 라운드 필터: 개인 1–18 + 집단 19–36
data <- data[data$round_number %in% c(1:36), ]

id_list <- sort(unique(data$id))
cat("F_CCEI_lg 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

# 상태 확률 (1/2, 1/2)
pi <- matrix(1/2, 2, 1)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- data[data$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_fgarp(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "f_ccei_lg", "n_lg")
result_df$id <- as.character(result_df$id)
result_df$f_ccei_lg <- as.numeric(result_df$f_ccei_lg)
result_df$n_lg <- as.numeric(result_df$n_lg)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/F_baseline_final.RData")

df_lg <- result_df
F_baseline_final$id <- as.character(F_baseline_final$id)
F_baseline_final$partner_id <- as.character(F_baseline_final$partner_id)

F_baseline_final$f_ccei_lg <- NA
F_baseline_final$n_lg <- NA

for (i in 1:nrow(df_lg)) {
  id_i <- df_lg$id[i]
  val  <- df_lg$f_ccei_lg[i]
  nval <- df_lg$n_lg[i]
  
  match_idx <- which(F_baseline_final$id == id_i)
  if (length(match_idx) > 0) {
    F_baseline_final$f_ccei_lg[match_idx] <- val
    F_baseline_final$n_lg[match_idx] <- nval
    next
  }
  
  match_idx <- which(F_baseline_final$partner_id == id_i)
  if (length(match_idx) > 0) {
    F_baseline_final$f_ccei_lg[match_idx] <- val
    F_baseline_final$n_lg[match_idx] <- nval
    next
  }
}

# ------------ 검증 --------------------------

F_baseline_final$f_ccei_1g <- as.numeric(F_baseline_final$f_ccei_1g)
F_baseline_final$f_ccei_2g <- as.numeric(F_baseline_final$f_ccei_2g)
F_baseline_final$f_ccei_lg <- as.numeric(F_baseline_final$f_ccei_lg)

epsilon <- 1e-8

F_baseline_final$match_f_lg <- ifelse(
  !is.na(F_baseline_final$f_ccei_lg) &
    (abs(F_baseline_final$f_ccei_lg - F_baseline_final$f_ccei_1g) < epsilon |
       abs(F_baseline_final$f_ccei_lg - F_baseline_final$f_ccei_2g) < epsilon),
  1, 0
)

cat("match_f_lg 결과:\n")
print(table(F_baseline_final$match_f_lg))

# 진단용 제거
F_baseline_final$n_lg <- NULL
F_baseline_final$match_f_lg <- NULL

# 저장
save(F_baseline_final, file = "../results/F_baseline_final.RData")


#####################################################

# 9. CCEI_hlg 계산 및 병합 (baseline, F-GARP 기준)

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

# 변수 이름 정의
data_name <- "baseline_raw"
result_name <- "fgarp_baseline_hlg"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)

# 문자형 변환
data$id <- as.character(data$id)
data$partner_id <- as.character(data$partner_id)
data$game_type <- as.character(data$game_type)
data$mover <- as.character(data$mover)

# mover 변환
data$mover[data$mover == "1"] <- "t"
data$mover[data$mover == "0"] <- "f"

# partner_id 보정 (round_number == 19 기준)
library(dplyr)
data <- data %>%
  group_by(id) %>%
  mutate(partner_id = ifelse(round_number >= 1 & round_number <= 36,
                             partner_id[round_number == 19][1],
                             partner_id)) %>%
  ungroup()

# 라운드 필터: 개인 1–18, 집단 19–36
data <- data[data$round_number %in% 1:36, ]

# 유효한 쌍만 선택
filtered_data <- data[data$id != data$partner_id, ]

# group_id 생성
filtered_data$group_id <- paste0(
  pmax(filtered_data$id, filtered_data$partner_id),
  pmin(filtered_data$id, filtered_data$partner_id)
)

# 개인 결정
data_indiv <- filtered_data[filtered_data$game_type == "individual", ]

# 집단 결정 (mover == "t"인 경우만)
collective_data <- filtered_data[filtered_data$game_type == "collective" & filtered_data$mover == "t", ]

# 병합
data_combined <- rbind(data_indiv, collective_data)

# 상태 확률 (1/2, 1/2)
pi <- matrix(1/2, 2, 1)

# CCEI 계산
group_list <- sort(unique(data_combined$group_id))
result_matrix <- matrix(NA, nrow = length(group_list), ncol = 3)

for (i in seq_along(group_list)) {
  g <- group_list[i]
  sub <- data_combined[data_combined$group_id == g, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- g
  result_matrix[i, 2] <- ccei_fgarp(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)
}

# 결과 정리
result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("group_id", "f_ccei_hlg", "n_used")
result_df$group_id <- as.character(result_df$group_id)
result_df$f_ccei_hlg <- as.numeric(result_df$f_ccei_hlg)
result_df$n_used <- as.numeric(result_df$n_used)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/F_baseline_final.RData")

F_baseline_final$group_id <- as.character(F_baseline_final$group_id)

F_baseline_final <- merge(
  F_baseline_final, result_df,
  by = "group_id", all.x = TRUE
)

# 진단용 제거
F_baseline_final$n_used <- NULL

# 저장
save(F_baseline_final, file = "../results/F_baseline_final.RData")






















################################################################
# 0. endline_final 데이터 구축

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/endline_raw.RData")

endline_raw$id <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)

# 1. round_number == 2 기준, 유효한 쌍만 필터링
data_filtered <- endline_raw[
  endline_raw$round_number == 2 &
    endline_raw$partner_id != 0 &
    endline_raw$id != endline_raw$partner_id,
]

# 2. group_id 생성 (id와 partner_id 정렬 후 붙임)
data_filtered$group_id <- paste0(
  pmax(data_filtered$id, data_filtered$partner_id),
  pmin(data_filtered$id, data_filtered$partner_id)
)

# 3. group_id 기준으로 중복 제거 (그룹당 하나만 남기기)
unique_groups <- data_filtered[!duplicated(data_filtered$group_id), ]

# 4. 초기 구성
F_endline_final <- unique_groups[, c("id", "partner_id", "group_id", "mover")]

# 5. mover 정보 보정 (round_number == 19 기준)
raw_mover <- subset(endline_raw, round_number == 19 & id != partner_id)
mover_info <- raw_mover[, c("id", "mover")]
mover_info <- mover_info[!duplicated(mover_info$id), ]

F_endline_final <- merge(
  F_endline_final, mover_info,
  by = "id", all.x = TRUE, suffixes = c("", "_from_raw")
)

# 6. 기존 mover 값이 비어있으면 보정값으로 채움
F_endline_final$mover <- ifelse(
  is.na(F_endline_final$mover) | F_endline_final$mover == "",
  F_endline_final$mover_from_raw,
  F_endline_final$mover
)

# 7. 보정용 임시 열 제거
F_endline_final$mover_from_raw <- NULL

# 저장
save(F_endline_final, file = "../results/F_endline_final.RData")

################################################################

# 1. CCEI_1, CCEI_2 계산

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/endline_raw.RData")
load("../results/F_endline_final.RData")

# 문자열 정리
endline_raw$id <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)
F_endline_final$id <- as.character(F_endline_final$id)
F_endline_final$partner_id <- as.character(F_endline_final$partner_id)

# 개인 게임만 추출 (1~18 라운드)
raw_filtered <- subset(
  endline_raw,
  game_type == "individual" & round_number %in% 1:18 & id != partner_id
)

# F_CCEI 변수 초기화
F_endline_final$f_ccei_1 <- NA
F_endline_final$f_ccei_2 <- NA
F_endline_final$n_1 <- NA
F_endline_final$n_2 <- NA

# 상태 확률 (동일 확률: 1/2 each)
pi <- matrix(1/2, 2, 1)

# 루프 실행
for (i in seq_len(nrow(F_endline_final))) {
  id1 <- F_endline_final$id[i]
  id2 <- F_endline_final$partner_id[i]
  
  # id1 처리
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    F_endline_final$f_ccei_1[i] <- ccei_fgarp(p1, x1, pi)
    F_endline_final$n_1[i] <- nrow(sub1)
  }
  
  # id2 처리
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    F_endline_final$f_ccei_2[i] <- ccei_fgarp(p2, x2, pi)
    F_endline_final$n_2[i] <- nrow(sub2)
  }
}

# 진단용 변수 제거
F_endline_final$n_1 <- NULL
F_endline_final$n_2 <- NULL

# 결과 저장
save(F_endline_final, file = "../results/F_endline_final.RData")

################################################################

# 2. CCEI_col 계산

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/endline_raw.RData")
load("../results/F_endline_final.RData")
load("../results/endline_final.RData")  # ⬅️ baseline CCEI 비교용

# 문자열 정리
endline_raw$id <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)
F_endline_final$id <- as.character(F_endline_final$id)
endline_final$id <- as.character(endline_final$id)

# 집단 결정 라운드 19–36 필터
raw_filtered <- subset(
  endline_raw,
  round_number >= 19 & round_number <= 36 &
    id != partner_id &
    partner_id != "0"
)

id_list <- sort(unique(raw_filtered$id))
cat("F_CCEI_col 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

# 상태 확률: 두 상태가 1/2씩
pi <- matrix(1/2, 2, 1)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- raw_filtered[raw_filtered$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_fgarp(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "f_ccei_col", "n_col")
result_df$id <- as.character(result_df$id)
result_df$f_ccei_col <- as.numeric(result_df$f_ccei_col)
result_df$n_col <- as.numeric(result_df$n_col)

# 결과 저장
F_endline_final$f_ccei_col <- NA
F_endline_final$n_col <- NA

for (i in 1:nrow(result_df)) {
  id_i <- result_df$id[i]
  val  <- result_df$f_ccei_col[i]
  nval <- result_df$n_col[i]
  
  match_idx <- which(F_endline_final$id == id_i)
  if (length(match_idx) > 0) {
    F_endline_final$f_ccei_col[match_idx] <- val
    F_endline_final$n_col[match_idx] <- nval
  }
}

# 진단용 제거
F_endline_final$n_col <- NULL

# 저장
save(F_endline_final, file = "../results/F_endline_final.RData")

###################################################################

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/endline_raw.RData")
load("../results/F_endline_final.RData")

# 문자열 변환
endline_raw$id <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)
F_endline_final$id <- as.character(F_endline_final$id)
F_endline_final$partner_id <- as.character(F_endline_final$partner_id)

# 유효 라운드 필터: 개인 1–18 + 집단 19–36
raw_filtered <- subset(
  endline_raw,
  (game_type == "individual" & round_number %in% 1:18) |
    (game_type == "collective" & round_number %in% 19:36)
)

# 쌍 중복 제거
raw_filtered <- raw_filtered[raw_filtered$id != raw_filtered$partner_id, ]

# 결과 칼럼 초기화
F_endline_final$f_ccei_1g <- NA
F_endline_final$f_ccei_2g <- NA
F_endline_final$n_1g <- NA
F_endline_final$n_2g <- NA

# 상태 확률 고정 (두 상태 모두 1/2)
pi <- matrix(1/2, 2, 1)

# 계산 루프
for (i in seq_len(nrow(F_endline_final))) {
  id1 <- F_endline_final$id[i]
  id2 <- F_endline_final$partner_id[i]
  
  # id1
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    F_endline_final$f_ccei_1g[i] <- ccei_fgarp(p1, x1, pi)
    F_endline_final$n_1g[i] <- nrow(sub1)
  }
  
  # id2
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    F_endline_final$f_ccei_2g[i] <- ccei_fgarp(p2, x2, pi)
    F_endline_final$n_2g[i] <- nrow(sub2)
  }
}

# 진단용 변수 제거
F_endline_final$n_1g <- NULL
F_endline_final$n_2g <- NULL

# 저장
save(F_endline_final, file = "../results/F_endline_final.RData")


################################################################

# 초기화
rm(list = ls())

# 데이터 불러오기
load("../results/F_endline_final.RData")
load("../data/endline_raw.RData")

# 문자열 처리
endline_raw$id <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)
F_endline_final$id <- as.character(F_endline_final$id)
F_endline_final$partner_id <- as.character(F_endline_final$partner_id)

# 유효 그룹만 사용 (id ≠ partner_id)
endline_raw <- endline_raw[endline_raw$id != endline_raw$partner_id, ]

# F-GARP 기준 high/low 초기화
endline_raw$f_high <- 0
endline_raw$f_low <- 0

# 루프: 각 그룹마다 f_high / f_low 결정
for (i in 1:nrow(F_endline_final)) {
  id1 <- F_endline_final$id[i]
  id2 <- F_endline_final$partner_id[i]
  
  f1 <- as.numeric(F_endline_final$f_ccei_1[i])
  f2 <- as.numeric(F_endline_final$f_ccei_2[i])
  f1g <- as.numeric(F_endline_final$f_ccei_1g[i])
  f2g <- as.numeric(F_endline_final$f_ccei_2g[i])
  
  high <- id1
  low <- id2
  
  if (!is.na(f1) && !is.na(f2)) {
    if (f1 > f2) {
      high <- id1; low <- id2
    } else if (f2 > f1) {
      high <- id2; low <- id1
    } else if (!is.na(f1g) && !is.na(f2g)) {
      if (f1g > f2g) {
        high <- id1; low <- id2
      } else if (f2g > f1g) {
        high <- id2; low <- id1
      } else {
        high <- id1; low <- id2  # 동률 fallback
      }
    }
  }
  
  # 결과 기록
  endline_raw$f_high[endline_raw$id == high] <- 1
  endline_raw$f_low[endline_raw$id == low] <- 1
}

# 저장
save(endline_raw, file = "../data/endline_raw.RData")


#####################################################################

# 7. CCEI_hg 계산

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

data_name <- "endline_raw"
result_name <- "fgarp_endline_hg"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

# ▶️ F-GARP 기준: f_high == 1만 선택
data <- data[data$f_high == 1, ]

# 라운드 필터: 개인 + 집단 모두
data <- data[
  (data$round_number >= 1 & data$round_number <= 36),
]

id_list <- sort(unique(data$id))
cat("F_CCEI 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

# 상태 확률 (1/2, 1/2)
pi <- matrix(1/2, 2, 1)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- data[data$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_fgarp(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "f_ccei_hg", "n_hg")
result_df$id <- as.character(result_df$id)
result_df$f_ccei_hg <- as.numeric(result_df$f_ccei_hg)
result_df$n_hg <- as.numeric(result_df$n_hg)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/F_endline_final.RData")

df_hg <- result_df
F_endline_final$id <- as.character(F_endline_final$id)
F_endline_final$partner_id <- as.character(F_endline_final$partner_id)
F_endline_final$f_ccei_hg <- NA
F_endline_final$n_hg <- NA

for (i in 1:nrow(df_hg)) {
  id_i <- df_hg$id[i]
  val  <- df_hg$f_ccei_hg[i]
  nval <- df_hg$n_hg[i]
  
  match_idx <- which(F_endline_final$id == id_i)
  if (length(match_idx) > 0) {
    F_endline_final$f_ccei_hg[match_idx] <- val
    F_endline_final$n_hg[match_idx] <- nval
    next
  }
  
  match_idx <- which(F_endline_final$partner_id == id_i)
  if (length(match_idx) > 0) {
    F_endline_final$f_ccei_hg[match_idx] <- val
    F_endline_final$n_hg[match_idx] <- nval
    next
  }
}

# 진단용 변수 제거
F_endline_final$n_hg <- NULL

# ------------ 검증 --------------------------

# 수치형 변환
F_endline_final$f_ccei_1g <- as.numeric(F_endline_final$f_ccei_1g)
F_endline_final$f_ccei_2g <- as.numeric(F_endline_final$f_ccei_2g)
F_endline_final$f_ccei_hg <- as.numeric(F_endline_final$f_ccei_hg)

# 허용 오차 (수치 오차 대응)
epsilon <- 1e-8

# 검증: f_ccei_hg가 f_ccei_1g 또는 f_ccei_2g 중 하나와 일치하는지
F_endline_final$match_f_hg <- ifelse(
  !is.na(F_endline_final$f_ccei_hg) &
    (abs(F_endline_final$f_ccei_hg - F_endline_final$f_ccei_1g) < epsilon |
       abs(F_endline_final$f_ccei_hg - F_endline_final$f_ccei_2g) < epsilon),
  1, 0
)

# 결과 출력
cat("match_f_hg 결과:\n")
print(table(F_endline_final$match_f_hg))

# 진단용 제거
F_endline_final$match_f_hg <- NULL

# 저장
save(F_endline_final, file = "../results/F_endline_final.RData")


#####################################################

# 8. CCEI_lg 계산

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

data_name <- "endline_raw"
result_name <- "fgarp_endline_lg"

# 데이터 로드
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

# ▶️ F-GARP 기준: f_low == 1
data <- data[data$f_low == 1, ]

# 라운드 필터: 개인 1–18 + 집단 19–36
data <- data[data$round_number %in% c(1:36), ]

id_list <- sort(unique(data$id))
cat("F_CCEI_lg 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

# 상태 확률 (1/2, 1/2)
pi <- matrix(1/2, 2, 1)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- data[data$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_fgarp(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "f_ccei_lg", "n_lg")
result_df$id <- as.character(result_df$id)
result_df$f_ccei_lg <- as.numeric(result_df$f_ccei_lg)
result_df$n_lg <- as.numeric(result_df$n_lg)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/F_endline_final.RData")

df_lg <- result_df
F_endline_final$id <- as.character(F_endline_final$id)
F_endline_final$partner_id <- as.character(F_endline_final$partner_id)

F_endline_final$f_ccei_lg <- NA
F_endline_final$n_lg <- NA

for (i in 1:nrow(df_lg)) {
  id_i <- df_lg$id[i]
  val  <- df_lg$f_ccei_lg[i]
  nval <- df_lg$n_lg[i]
  
  match_idx <- which(F_endline_final$id == id_i)
  if (length(match_idx) > 0) {
    F_endline_final$f_ccei_lg[match_idx] <- val
    F_endline_final$n_lg[match_idx] <- nval
    next
  }
  
  match_idx <- which(F_endline_final$partner_id == id_i)
  if (length(match_idx) > 0) {
    F_endline_final$f_ccei_lg[match_idx] <- val
    F_endline_final$n_lg[match_idx] <- nval
    next
  }
}

# ------------ 검증 --------------------------

F_endline_final$f_ccei_1g <- as.numeric(F_endline_final$f_ccei_1g)
F_endline_final$f_ccei_2g <- as.numeric(F_endline_final$f_ccei_2g)
F_endline_final$f_ccei_lg <- as.numeric(F_endline_final$f_ccei_lg)

epsilon <- 1e-8

F_endline_final$match_f_lg <- ifelse(
  !is.na(F_endline_final$f_ccei_lg) &
    (abs(F_endline_final$f_ccei_lg - F_endline_final$f_ccei_1g) < epsilon |
       abs(F_endline_final$f_ccei_lg - F_endline_final$f_ccei_2g) < epsilon),
  1, 0
)

cat("match_f_lg 결과:\n")
print(table(F_endline_final$match_f_lg))

# 진단용 제거
F_endline_final$n_lg <- NULL
F_endline_final$match_f_lg <- NULL

# 저장
save(F_endline_final, file = "../results/F_endline_final.RData")


#####################################################

# 9. CCEI_hlg 계산 및 병합

# 초기화
rm(list = ls())

# 함수 로드
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

# 변수 이름 정의
data_name <- "endline_raw"
result_name <- "fgarp_endline_hlg"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)

# 문자형 변환
data$id <- as.character(data$id)
data$partner_id <- as.character(data$partner_id)
data$game_type <- as.character(data$game_type)
data$mover <- as.character(data$mover)

# mover 변환
data$mover[data$mover == "1"] <- "t"
data$mover[data$mover == "0"] <- "f"

# partner_id 보정 (round 19 기준으로 정렬)
library(dplyr)
data <- data %>%
  group_by(id) %>%
  mutate(partner_id = ifelse(round_number >= 1 & round_number <= 36,
                             partner_id[round_number == 19][1],
                             partner_id)) %>%
  ungroup()

# 라운드 필터
data <- data[data$round_number %in% 1:36, ]

# 유효 페어만
filtered_data <- data[data$id != data$partner_id, ]

# group_id 구성
filtered_data$group_id <- paste0(
  pmax(filtered_data$id, filtered_data$partner_id),
  pmin(filtered_data$id, filtered_data$partner_id)
)

# 개인 선택
data_indiv <- filtered_data[filtered_data$game_type == "individual", ]

# 공동 선택 중 mover가 선택한 것만
collective_data <- filtered_data[filtered_data$game_type == "collective" & filtered_data$mover == "t", ]

# 합치기
data_combined <- rbind(data_indiv, collective_data)

# 상태 확률 (1/2, 1/2)
pi <- matrix(1/2, 2, 1)

# group_id 단위로 F-GARP CCEI 계산
group_list <- sort(unique(data_combined$group_id))
result_matrix <- matrix(NA, nrow = length(group_list), ncol = 3)

for (i in seq_along(group_list)) {
  g <- group_list[i]
  sub <- data_combined[data_combined$group_id == g, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- g
  result_matrix[i, 2] <- ccei_fgarp(p, x, pi)
  result_matrix[i, 3] <- nrow(sub)
}

# 결과 정리
result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("group_id", "f_ccei_hlg", "n_used")
result_df$group_id <- as.character(result_df$group_id)
result_df$f_ccei_hlg <- as.numeric(result_df$f_ccei_hlg)
result_df$n_used <- as.numeric(result_df$n_used)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/F_endline_final.RData")

F_endline_final$group_id <- as.character(F_endline_final$group_id)

F_endline_final <- merge(
  F_endline_final, result_df,
  by = "group_id", all.x = TRUE
)

# 진단용 제거
F_endline_final$n_used <- NULL

# 저장
save(F_endline_final, file = "../results/F_endline_final.RData")


################################################################




############################################################
# 0. 준비
############################################################
rm(list = ls())

# 이미 GARP 패널( id.x / id.y 가 들어 있음 )
load("../results/panel_final.RData")

# F-GARP 결과
load("../results/F_baseline_final.RData")
load("../results/F_endline_final.RData")

library(dplyr)

############################################################
# 1. endline 이름 뒤에 _end 붙이기 (id, partner_id, group_id 제외)
############################################################
cols_to_rename <- setdiff(names(F_endline_final), c("id", "partner_id", "group_id"))
names(F_endline_final)[names(F_endline_final) %in% cols_to_rename] <-
  paste0(cols_to_rename, "_end")

############################################################
# 2. 병합 준비: id, partner_id, mover 계열은 빼고 group_id 로만 붙임
############################################################
F_base_add <- F_baseline_final %>%
  select(-any_of(c("id", "partner_id", "mover")))          # baseline

F_end_add  <- F_endline_final %>%
  select(-any_of(c("id", "partner_id", "mover_end")))      # endline (_end 붙은 상태)

############################################################
# 3. panel_final 에 f-계열 다시 병합
############################################################
panel_final <- panel_final %>%
  left_join(F_base_add, by = "group_id") %>%               # baseline 먼저
  left_join(F_end_add,  by = "group_id")                   # endline 나중

############################################################
# 4. flip 여부 판단 후 swap (id.x ↔ partner_id.y 가 뒤집힌 경우)
############################################################
flip_idx <- which(panel_final$id.x == panel_final$partner_id.y)

# ── swap 대상 4개 ──────────────────────────────────────────
swap_pairs <- list(
  c("f_ccei_1_end",  "f_ccei_2_end"),
  c("f_ccei_1g_end", "f_ccei_2g_end")
)

for (p in swap_pairs) {
  tmp <- panel_final[[p[1]]][flip_idx]
  panel_final[[p[1]]][flip_idx] <- panel_final[[p[2]]][flip_idx]
  panel_final[[p[2]]][flip_idx] <- tmp
}


save(panel_final, file = "../results/panel_final.RData")


#################################################################





############################################################


rm(list = ls())
load("../results/panel_final.RData")

library(dplyr)

tol <- 1e-5   # 허용 오차 (소수점 5자리)

panel_chk <- panel_final %>% 
  mutate(
    cond_1 = ccei_1      + tol >= f_ccei_1,       # baseline 1번
    cond_2 = ccei_2      + tol >= f_ccei_2,       # baseline 2번
    cond_3 = ccei_1_end  + tol >= f_ccei_1_end,   # endline 1번
    cond_4 = ccei_2_end  + tol >= f_ccei_2_end,   # endline 2번
    all_ok = cond_1 & cond_2 & cond_3 & cond_4
  )

# 모든 조건 만족 여부
all(panel_chk$cond_1)   # TRUE면 ccei_1 ≥ f_ccei_1 (±1e-5) 전부 만족
all(panel_chk$cond_2)
all(panel_chk$cond_3)
all(panel_chk$cond_4)

# 어긋난 행만 보기
violations <- panel_chk %>% 
  filter(!all_ok) %>% 
  select(group_id, id.x, partner_id.x, id.y, partner_id.y,
         ccei_1, f_ccei_1, ccei_2, f_ccei_2,
         ccei_1_end, f_ccei_1_end, ccei_2_end, f_ccei_2_end)

# 조건별 위반 건수
violation_counts <- colSums(!panel_chk[, c("cond_1", "cond_2", "cond_3", "cond_4")])

print(violation_counts)
print(nrow(violations))   # 전부 맞으면 0





#########################################################

rm(list = ls())
load("../results/panel_final.RData")

# 수치형 변환 보장 (F-GARP 변수)
panel_final$f_ccei_1      <- as.numeric(panel_final$f_ccei_1)
panel_final$f_ccei_2      <- as.numeric(panel_final$f_ccei_2)
panel_final$f_ccei_1g     <- as.numeric(panel_final$f_ccei_1g)
panel_final$f_ccei_2g     <- as.numeric(panel_final$f_ccei_2g)
panel_final$f_ccei_1_end  <- as.numeric(panel_final$f_ccei_1_end)
panel_final$f_ccei_2_end  <- as.numeric(panel_final$f_ccei_2_end)
panel_final$f_ccei_1g_end <- as.numeric(panel_final$f_ccei_1g_end)
panel_final$f_ccei_2g_end <- as.numeric(panel_final$f_ccei_2g_end)

# baseline 기준 f_high 생성
panel_final$f_high <- ifelse(
  panel_final$f_ccei_1 > panel_final$f_ccei_2, 1,
  ifelse(
    panel_final$f_ccei_1 < panel_final$f_ccei_2, 0,
    ifelse(
      panel_final$f_ccei_1g > panel_final$f_ccei_2g, 1,
      ifelse(
        panel_final$f_ccei_1g < panel_final$f_ccei_2g, 0,
        1
      )
    )
  )
)

# endline 기준 f_high_end 생성
panel_final$f_high_end <- ifelse(
  panel_final$f_ccei_1_end > panel_final$f_ccei_2_end, 1,
  ifelse(
    panel_final$f_ccei_1_end < panel_final$f_ccei_2_end, 0,
    ifelse(
      panel_final$f_ccei_1g_end > panel_final$f_ccei_2g_end, 1,
      ifelse(
        panel_final$f_ccei_1g_end < panel_final$f_ccei_2g_end, 0,
        1
      )
    )
  )
)

# 확인
table(panel_final$f_high, useNA = "always")
table(panel_final$f_high_end, useNA = "always")

cor(as.numeric(panel_final$ccei_hlg), as.numeric(panel_final$f_ccei_hlg))

# 저장
save(panel_final, file = "../results/panel_final.RData")



###########################################################


################################################################################
# 0. 준비  ─ 기존 RA 변수 제거
################################################################################
rm(list = ls())
library(dplyr)

load("../results/panel_final.RData")          # id.x·id.y 등이 이미 있음
ra_vars <- c("RA_g","RA_g_end","RA_1","RA_2",
             "RA_1_end","RA_2_end")
panel_final <- panel_final %>% select(-any_of(ra_vars))

################################################################################
# 1. 보조 함수: 게임타입별 평균 RA 계산 테이블 만들기
################################################################################
#  - collective: intercept_x < intercept_y  → expensive 자산 = coord_y
#  - individual: intercept_x > intercept_y  → expensive 자산 = coord_x
get_RA_tbl <- function(raw, game_type, expensive_left) {
  raw %>% 
    filter(game_type == !!game_type) %>% 
    mutate(
      x_exp_asset = ifelse(
        (expensive_left == "x_lt_y" & intercept_x < intercept_y) |
          (expensive_left == "x_gt_y" & intercept_x > intercept_y),
        coord_y, coord_x        # 더 비싼 자산
      ),
      RA = x_exp_asset / (coord_x + coord_y)
    ) %>% 
    group_by(id) %>% 
    summarise(RA = mean(RA, na.rm = TRUE), .groups = "drop")
}

################################################################################
# 2. Baseline collective  →  RA_g  (swap 필요 X)
################################################################################
load("../data/baseline_raw.RData")

RA_g_tbl <- get_RA_tbl(baseline_raw, "collective", "x_lt_y")
panel_final$RA_g <- RA_g_tbl$RA[ match(panel_final$id.x, RA_g_tbl$id) ]

################################################################################
# 3. Endline collective  →  RA_g_end  (swap 필요 X)
################################################################################
load("../data/endline_raw.RData")

RA_g_end_tbl <- get_RA_tbl(endline_raw, "collective", "x_lt_y")
panel_final$RA_g_end <- RA_g_end_tbl$RA[ match(panel_final$id.x, RA_g_end_tbl$id) ]
# id.x 와 partner_id.x 모두 같은 값이어야 함
# (검증하려면 cor(panel_final$RA_g_end, RA_g_end_tbl$RA[match(panel_final$partner_id.x, RA_g_end_tbl$id)]))

################################################################################
# 4. Baseline individual  →  RA_1, RA_2  (swap 필요 X)
################################################################################
RA_ind_tbl <- get_RA_tbl(baseline_raw, "individual", "x_gt_y")

panel_final$RA_1 <- RA_ind_tbl$RA[ match(panel_final$id.x,         RA_ind_tbl$id) ]
panel_final$RA_2 <- RA_ind_tbl$RA[ match(panel_final$partner_id.x, RA_ind_tbl$id) ]

################################################################################
# 5. Endline individual  →  RA_1_end, RA_2_end  (flip 보정)
################################################################################
RA_ind_end_tbl <- get_RA_tbl(endline_raw, "individual", "x_gt_y")

panel_final$RA_1_end <- RA_ind_end_tbl$RA[ match(panel_final$id.y,        RA_ind_end_tbl$id) ]
panel_final$RA_2_end <- RA_ind_end_tbl$RA[ match(panel_final$partner_id.y, RA_ind_end_tbl$id) ]

# flip 행 교환
flip_idx <- which(panel_final$id.x == panel_final$partner_id.y)
tmp <- panel_final$RA_1_end[flip_idx]
panel_final$RA_1_end[flip_idx] <- panel_final$RA_2_end[flip_idx]
panel_final$RA_2_end[flip_idx] <- tmp

################################################################################
# 6. 간단 확인 (선택)
################################################################################
summary(panel_final$RA_1_end); summary(panel_final$RA_2_end)

library(dplyr)

tol <- 1e-5        # 소수점 5자리 이하 허용 오차

chk <- panel_final %>% 
  mutate(
    cond_1      = abs(RT_1      + RA_1      - 1) < tol,
    cond_2      = abs(RT_2      + RA_2      - 1) < tol,
    cond_1_end  = abs(RT_1_end  + RA_1_end  - 1) < tol,
    cond_2_end  = abs(RT_2_end  + RA_2_end  - 1) < tol,
    all_ok      =  cond_1 & cond_2 & cond_1_end & cond_2_end
  )

### 1) 각 조건이 전부 TRUE인가?
sapply(chk %>% select(starts_with("cond_")), all)
# TRUE 가 6개 전부 나오면 모든 조건 만족

### 2) 조건별 위반 건수
viol_cnt <- colSums(!chk %>% select(starts_with("cond_")))
print(viol_cnt)

### 3) 하나라도 어긴 행 살펴보기 (필요 시)
violations <- chk %>% filter(!all_ok)
# View(violations) 로 상세 확인 가능


################################################################################
# 7. 저장
################################################################################
save(panel_final, file = "../results/panel_final.RData")













##########################################################


#################################################################


###############################################################
library(ggplot2)
library(dplyr)

rm(list = ls())
load("../results/panel_final.RData")

# baseline 기준 index_lg 추출
df <- panel_final
df <- df[!is.na(df$index_lg), ]

# 세부 히스토그램 계산
hist_detail <- hist(df$index_lg, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 0.2 단위 bin 그룹
df$bin_group <- cut(df$index_lg, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
group_props <- df %>%
  group_by(bin_group) %>%
  summarise(n = n()) %>%
  mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.9, by = 0.2)
  )

# 평균 및 중앙값
x_mean <- mean(df$index_lg)
x_median <- median(df$index_lg)
y_max <- 350

# 시각화
ggplot(df, aes(x = index_lg)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "gray70", color = "black") +
  
  geom_text(
    data = data.frame(x = mid_detail, y = count_detail / 2,
                      label = ifelse(prop_detail >= 10, paste0(prop_detail, "%"), "")),
    aes(x = x, y = y, label = label),
    size = 4, fontface = "bold"
  ) +
  
  geom_text(
    data = group_props,
    aes(x = mid, y = y_max * 0.95, label = paste0(prop, "%")),
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
  
  scale_y_continuous(
    limits = c(0, 350),
    breaks = seq(0, 400, by = 50),
    expand = c(0, 0)
  ) +
  
  labs(
    title = expression(paste(italic("Distribution of "), I[l*","*g], " (baseline)")),
    x = expression(I[l*","*g]),
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )

###############################################################

library(ggplot2)
library(dplyr)

rm(list = ls())
load("../results/panel_final.RData")

# endline 기준 index_lg 추출
df <- panel_final
df <- df[!is.na(df$index_lg_end), ]

# 세부 히스토그램 계산
hist_detail <- hist(df$index_lg_end, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 0.2 단위 bin 그룹
df$bin_group <- cut(df$index_lg_end, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
group_props <- df %>%
  group_by(bin_group) %>%
  summarise(n = n()) %>%
  mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.9, by = 0.2)
  )

# 평균 및 중앙값
x_mean <- mean(df$index_lg_end)
x_median <- median(df$index_lg_end)
y_max <- 350

# 시각화
ggplot(df, aes(x = index_lg_end)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "gray70", color = "black") +
  
  geom_text(
    data = data.frame(x = mid_detail, y = count_detail / 2,
                      label = ifelse(prop_detail >= 10, paste0(prop_detail, "%"), "")),
    aes(x = x, y = y, label = label),
    size = 4, fontface = "bold"
  ) +
  
  geom_text(
    data = group_props,
    aes(x = mid, y = y_max * 0.95, label = paste0(prop, "%")),
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
  
  scale_y_continuous(
    limits = c(0, 350),
    breaks = seq(0, 400, by = 50),
    expand = c(0, 0)
  ) +
  
  labs(
    title = expression(paste(italic("Distribution of "), I[l*","*g], " (endline)")),
    x = expression(I[l*","*g]),
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )

###############################################################

library(ggplot2)

rm(list = ls())
load("../results/panel_final.RData")

# 수치형 변환
panel_final$ccei_1     <- as.numeric(panel_final$ccei_1)
panel_final$ccei_2     <- as.numeric(panel_final$ccei_2)
panel_final$ccei_1_end <- as.numeric(panel_final$ccei_1_end)
panel_final$ccei_2_end <- as.numeric(panel_final$ccei_2_end)

# 각각의 매트릭스 만들기
df1 <- data.frame(
  baseline = panel_final$ccei_1,
  endline  = panel_final$ccei_1_end
)

df2 <- data.frame(
  baseline = panel_final$ccei_2,
  endline  = panel_final$ccei_2_end
)

# 세로로 결합
df_all <- rbind(df1, df2)

# NA 제거
df_all <- df_all[complete.cases(df_all), ]

# 스캐터플롯
ggplot(df_all, aes(x = baseline, y = endline)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_fixed(ratio = 1) +  # 정사각형 비율
  labs(
    title = "Individual CCEI: Baseline vs Endline",
    x = "CCEI_i (Baseline)",
    y = "CCEI_i (Endline)"
  ) +
  theme_minimal(base_size = 14)

###################################################

# high type (h0)
df_high_h0 <- data.frame(
  baseline = ifelse(panel_final$high == 1, panel_final$ccei_1, panel_final$ccei_2),
  endline  = ifelse(panel_final$high == 1, panel_final$ccei_1_end, panel_final$ccei_2_end)
)
df_high_h0 <- df_high_h0[complete.cases(df_high_h0), ]

ggplot(df_high_h0, aes(x = baseline, y = endline)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dotted") +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Individual CCEI (high type, h0)",
    x = "CCEI_i (Baseline)",
    y = "CCEI_i (Endline)"
  ) +
  theme_minimal(base_size = 14)

# low type (h0)
df_low_h0 <- data.frame(
  baseline = ifelse(panel_final$high == 1, panel_final$ccei_2, panel_final$ccei_1),
  endline  = ifelse(panel_final$high == 1, panel_final$ccei_2_end, panel_final$ccei_1_end)
)
df_low_h0 <- df_low_h0[complete.cases(df_low_h0), ]

ggplot(df_low_h0, aes(x = baseline, y = endline)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dotted") +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Individual CCEI (low type, h0)",
    x = "CCEI_i (Baseline)",
    y = "CCEI_i (Endline)"
  ) +
  theme_minimal(base_size = 14)

### h1 기준 (endline 기준 high) ###

# high type (h1)
df_high_h1 <- data.frame(
  baseline = ifelse(panel_final$high_end == 1, panel_final$ccei_1, panel_final$ccei_2),
  endline  = ifelse(panel_final$high_end == 1, panel_final$ccei_1_end, panel_final$ccei_2_end)
)
df_high_h1 <- df_high_h1[complete.cases(df_high_h1), ]

ggplot(df_high_h1, aes(x = baseline, y = endline)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dotted") +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Individual CCEI (high type, h1)",
    x = "CCEI_i (Baseline)",
    y = "CCEI_i (Endline)"
  ) +
  theme_minimal(base_size = 14)

# low type (h1)
df_low_h1 <- data.frame(
  baseline = ifelse(panel_final$high_end == 1, panel_final$ccei_2, panel_final$ccei_1),
  endline  = ifelse(panel_final$high_end == 1, panel_final$ccei_2_end, panel_final$ccei_1_end)
)
df_low_h1 <- df_low_h1[complete.cases(df_low_h1), ]

ggplot(df_low_h1, aes(x = baseline, y = endline)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dotted") +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Individual CCEI (low type, h1)",
    x = "CCEI_i (Baseline)",
    y = "CCEI_i (Endline)"
  ) +
  theme_minimal(base_size = 14)

#########################################################################3

library(ggplot2)

rm(list = ls())
load("../results/panel_final.RData")

# 수치형 변환
panel_final$ccei_g     <- as.numeric(panel_final$ccei_g)
panel_final$ccei_g_end <- as.numeric(panel_final$ccei_g_end)

# 매트릭스 생성
df <- data.frame(
  baseline = panel_final$ccei_g,
  endline  = panel_final$ccei_g_end
)

# NA 제거
df <- df[complete.cases(df), ]

# 스캐터플롯
ggplot(df, aes(x = baseline, y = endline)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_fixed(ratio = 1) +
  labs(
    title = "Group CCEI: Baseline vs Endline",
    x = "CCEI_g (Baseline)",
    y = "CCEI_g (Endline)"
  ) +
  theme_minimal(base_size = 14)


##################################################################3

library(ggplot2)

rm(list = ls())
load("../results/panel_final.RData")

# 수치형 변환
cols <- c("ccei_g", "ccei_hg", "ccei_hlg",
          "ccei_g_end", "ccei_hlg_end",
          "ccei_1g_end", "ccei_2g_end")

panel_final[cols] <- lapply(panel_final[cols], function(x) as.numeric(as.character(x)))

# Step 1: index_h0g = 기존 index_hg
panel_final$index_h0g <- with(panel_final, (ccei_g - ccei_hg) / (ccei_g - ccei_hlg))

# Step 2: ccei_h0g_end 정의 (high == 1 → 1g, high == 0 → 2g)
panel_final$ccei_h0g_end <- ifelse(panel_final$high == 1,
                                   panel_final$ccei_1g_end,
                                   panel_final$ccei_2g_end)

# Step 3: index_h0g_end 계산
panel_final$index_h0g_end <- with(panel_final, 
                                  (ccei_g_end - ccei_h0g_end) / 
                                    (ccei_g_end - ccei_hlg_end))

# Step 4: 스캐터플롯용 데이터
df_h0g <- panel_final[, c("index_h0g", "index_h0g_end")]
df_h0g <- df_h0g[complete.cases(df_h0g), ]

# Step 5: 시각화
ggplot(df_h0g, aes(x = index_h0g, y = index_h0g_end)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dotted") +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = expression(I[h[0]*","*g]*": Baseline-Defined High Type"),
    x = expression(I[h[0]*","*g]*" (Baseline)"),
    y = expression(I[h[0]*","*g]*" (Endline)")
  ) +
  theme_minimal(base_size = 14)


# 기존 panel_final 계속 사용

# 수치형 변환 (혹시 빠졌을 경우 대비)
cols2 <- c("ccei_1g", "ccei_2g")
panel_final[cols2] <- lapply(panel_final[cols2], function(x) as.numeric(as.character(x)))

# Step 1: ccei_h1g 정의
panel_final$ccei_h1g <- ifelse(panel_final$high_end == 1,
                               panel_final$ccei_1g,
                               panel_final$ccei_2g)

# Step 2: index_h1g 계산
panel_final$index_h1g <- with(panel_final, 
                              (ccei_g - ccei_h1g) / 
                                (ccei_g - ccei_hlg))

# Step 3: ccei_h1g_end 정의
panel_final$ccei_h1g_end <- ifelse(panel_final$high_end == 1,
                                   panel_final$ccei_1g_end,
                                   panel_final$ccei_2g_end)

# Step 4: index_h1g_end 계산
panel_final$index_h1g_end <- with(panel_final, 
                                  (ccei_g_end - ccei_h1g_end) / 
                                    (ccei_g_end - ccei_hlg_end))

# Step 5: 스캐터플롯용 데이터 정리
df_h1g <- panel_final[, c("index_h1g", "index_h1g_end")]
df_h1g <- df_h1g[complete.cases(df_h1g), ]

# Step 6: 시각화
ggplot(df_h1g, aes(x = index_h1g, y = index_h1g_end)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dotted") +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = expression(I[h[1]*","*g]*": Endline-Defined High Type"),
    x = expression(I[h[1]*","*g]*" (Baseline)"),
    y = expression(I[h[1]*","*g]*" (Endline)")
  ) +
  theme_minimal(base_size = 14)

########################################################

# Step 1: ccei_l0g 정의
panel_final$ccei_l0g <- ifelse(panel_final$high == 1,
                               panel_final$ccei_2g,
                               panel_final$ccei_1g)

# Step 2: index_l0g 계산
panel_final$index_l0g <- with(panel_final,
                              (ccei_g - ccei_l0g) /
                                (ccei_g - ccei_hlg))

# Step 3: ccei_l0g_end 정의
panel_final$ccei_l0g_end <- ifelse(panel_final$high == 1,
                                   panel_final$ccei_2g_end,
                                   panel_final$ccei_1g_end)

# Step 4: index_l0g_end 계산
panel_final$index_l0g_end <- with(panel_final,
                                  (ccei_g_end - ccei_l0g_end) /
                                    (ccei_g_end - ccei_hlg_end))

# Step 5: 시각화용 데이터
df_l0g <- panel_final[, c("index_l0g", "index_l0g_end")]
df_l0g <- df_l0g[complete.cases(df_l0g), ]

# Step 6: 스캐터플롯
ggplot(df_l0g, aes(x = index_l0g, y = index_l0g_end)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dotted") +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = expression(I[l[0]*","*g]*": Baseline-Defined Low Type"),
    x = expression(I[l[0]*","*g]*" (Baseline)"),
    y = expression(I[l[0]*","*g]*" (Endline)")
  ) +
  theme_minimal(base_size = 14)



# Step 1: ccei_l1g 정의
panel_final$ccei_l1g <- ifelse(panel_final$high_end == 1,
                               panel_final$ccei_2g,
                               panel_final$ccei_1g)

# Step 2: index_l1g 계산
panel_final$index_l1g <- with(panel_final,
                              (ccei_g - ccei_l1g) /
                                (ccei_g - ccei_hlg))

# Step 3: ccei_l1g_end 정의
panel_final$ccei_l1g_end <- ifelse(panel_final$high_end == 1,
                                   panel_final$ccei_2g_end,
                                   panel_final$ccei_1g_end)

# Step 4: index_l1g_end 계산
panel_final$index_l1g_end <- with(panel_final,
                                  (ccei_g_end - ccei_l1g_end) /
                                    (ccei_g_end - ccei_hlg_end))

# Step 5: 시각화용 데이터
df_l1g <- panel_final[, c("index_l1g", "index_l1g_end")]
df_l1g <- df_l1g[complete.cases(df_l1g), ]

# Step 6: 스캐터플롯
ggplot(df_l1g, aes(x = index_l1g, y = index_l1g_end)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dotted") +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = expression(I[l[1]*","*g]*": Endline-Defined Low Type"),
    x = expression(I[l[1]*","*g]*" (Baseline)"),
    y = expression(I[l[1]*","*g]*" (Endline)")
  ) +
  theme_minimal(base_size = 14)

############################################################

library(ggplot2)
library(dplyr)

rm(list = ls())
load("../results/panel_final.RData")

# 데이터 정제
df_plot <- panel_final %>%
  mutate(
    RT_h = pmax(RT_1, RT_2, na.rm = TRUE),
    RT_l = pmin(RT_1, RT_2, na.rm = TRUE)
  ) %>%
  arrange(RT_g) %>%
  mutate(order = row_number())

# 시각화
ggplot(df_plot, aes(x = order)) +
  geom_segment(aes(y = RT_h, yend = RT_l, xend = order),
               color = "gray60", linetype = "dotted", linewidth = 0.3, alpha = 0.5) +
  
  geom_point(aes(y = RT_h, color = "RT_h"), size = 1) +
  geom_point(aes(y = RT_l, color = "RT_l"), size = 1) +
  geom_point(aes(y = RT_g, color = "RT_g"), size = 0.8) +  # RT_g 포인트 더 작게
  
  scale_color_manual(
    name = NULL,
    values = c("RT_g" = "black", "RT_h" = "#1f77b4", "RT_l" = "#ff7f0e"),
    breaks = c("RT_g", "RT_h", "RT_l"),
    labels = c("RT_g", "RT_h", "RT_l")
  ) +
  
  labs(
    title = "Baseline RT: RT_h, RT_l, and RT_g",
    x = "Group (Ordered by RT_g)",
    y = "Risk Tolerance"
  ) +
  coord_cartesian(ylim = c(0.3, 1.05)) +  # 줌아웃
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA)
  )



library(ggplot2)
library(dplyr)

rm(list = ls())
load("../results/panel_final.RData")

# 데이터 정제
df_plot_end <- panel_final %>%
  mutate(
    RT_h_end = pmax(RT_1_end, RT_2_end, na.rm = TRUE),
    RT_l_end = pmin(RT_1_end, RT_2_end, na.rm = TRUE)
  ) %>%
  arrange(RT_g_end) %>%
  mutate(order = row_number())  # RT_g_end 기준 정렬 인덱스

# 시각화
ggplot(df_plot_end, aes(x = order)) +
  geom_segment(aes(y = RT_h_end, yend = RT_l_end, xend = order),
               color = "gray60", linetype = "dotted", linewidth = 0.3, alpha = 0.5) +
  
  geom_point(aes(y = RT_h_end, color = "RT_h"), size = 1) +
  geom_point(aes(y = RT_l_end, color = "RT_l"), size = 1) +
  geom_point(aes(y = RT_g_end, color = "RT_g"), size = 0.8) +  # RT_g 포인트 작게
  
  scale_color_manual(
    name = NULL,
    values = c("RT_g" = "black", "RT_h" = "#1f77b4", "RT_l" = "#ff7f0e"),
    breaks = c("RT_g", "RT_h", "RT_l"),
    labels = c("RT_g", "RT_h", "RT_l")
  ) +
  
  labs(
    title = "Endline RT: RT_h, RT_l, and RT_g",
    x = "Group (Ordered by RT_g_end)",
    y = "Risk Tolerance"
  ) +
  coord_cartesian(ylim = c(0.3, 1.05)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA)
  )
