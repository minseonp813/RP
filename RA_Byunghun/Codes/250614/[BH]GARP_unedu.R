################################################################
# 0. 이건 SKIP.

# baseline_final_edu 초기 구성

# baseline_final_unedu 초기 구성

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/baseline_raw.RData")

baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)

# 1. round_number == 2 기준, 유효한 쌍만 필터링
data_filtered <- baseline_raw[
  baseline_raw$round_number == 2 &
    baseline_raw$partner_id != 0 &
    baseline_raw$id != baseline_raw$partner_id,
]

# 2. 중복 제거 (id 기준)
unique_rows <- data_filtered[!duplicated(data_filtered$id), ]

# 3. group_id 생성 (id와 partner_id의 조합 정렬 후 붙임)
unique_rows$group_id <- paste0(
  pmax(unique_rows$id, unique_rows$partner_id),
  pmin(unique_rows$id, unique_rows$partner_id)
)

# 4. 초기 구성
baseline_final_unedu <- unique_rows[, c("id", "partner_id", "group_id", "mover")]

# 5. mover 정보 보정 (round_number == 19 기준)
raw_mover <- subset(baseline_raw, round_number == 19 & id != partner_id)
mover_info <- raw_mover[, c("id", "mover")]
mover_info <- mover_info[!duplicated(mover_info$id), ]

baseline_final_unedu <- merge(
  baseline_final_unedu, mover_info,
  by = "id", all.x = TRUE, suffixes = c("", "_from_raw")
)

# 6. 기존 mover 값이 비어있으면 보정값으로 채움
baseline_final_unedu$mover <- ifelse(
  is.na(baseline_final_unedu$mover) | baseline_final_unedu$mover == "",
  baseline_final_unedu$mover_from_raw,
  baseline_final_unedu$mover
)

# 7. 보정용 임시 열 제거
baseline_final_unedu$mover_from_raw <- NULL

# 저장
save(baseline_final_unedu, file = "../results/baseline_final_unedu.RData")


################################################################
# 1. CCEI_1_unedu, CCEI_2_unedu 계산

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/baseline_raw.RData")
load("../results/baseline_final_unedu.RData")

# 문자열 정리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final_unedu$id <- as.character(baseline_final_unedu$id)
baseline_final_unedu$partner_id <- as.character(baseline_final_unedu$partner_id)

# 필터: 개인 round 1–9만 사용
raw_filtered <- subset(
  baseline_raw,
  game_type == "individual" & round_number %in% 1:9 & id != partner_id
)

# 초기화
baseline_final_unedu$ccei_1_unedu <- NA
baseline_final_unedu$ccei_2_unedu <- NA
baseline_final_unedu$n_1 <- NA
baseline_final_unedu$n_2 <- NA

# 계산 루프
for (i in seq_len(nrow(baseline_final_unedu))) {
  id1 <- baseline_final_unedu$id[i]
  id2 <- baseline_final_unedu$partner_id[i]
  
  # id1
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    baseline_final_unedu$ccei_1_unedu[i] <- ccei_garp(p1, x1)
    baseline_final_unedu$n_1[i] <- nrow(sub1)
  }
  
  # id2
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    baseline_final_unedu$ccei_2_unedu[i] <- ccei_garp(p2, x2)
    baseline_final_unedu$n_2[i] <- nrow(sub2)
  }
}

# 진단용 변수 제거
baseline_final_unedu$n_1 <- NULL
baseline_final_unedu$n_2 <- NULL

# 저장
save(baseline_final_unedu, file = "../results/baseline_final_unedu.RData")


###################################################################

# 2. CCEI_col_edu 계산

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/baseline_raw.RData")
load("../results/baseline_final_unedu.RData")

# 문자열 변환
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final_unedu$id <- as.character(baseline_final_unedu$id)

# collective unedu 라운드 필터
raw_filtered <- subset(
  baseline_raw,
  round_number >= 19 & round_number <= 27 &  
    id != partner_id &                     
    partner_id != "0"                      
)

id_list <- sort(unique(raw_filtered$id))
cat("CCEI_col_unedu 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- raw_filtered[raw_filtered$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_garp(p, x)
  result_matrix[i, 3] <- nrow(sub)  # 진단용 n
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "ccei_col_unedu", "n_col_unedu")
result_df$id <- as.character(result_df$id)
result_df$ccei_col_unedu <- as.numeric(result_df$ccei_col_unedu)
result_df$n_col_unedu <- as.numeric(result_df$n_col_unedu)

baseline_final_unedu$ccei_col_unedu <- NA
baseline_final_unedu$n_col_unedu <- NA

for (i in 1:nrow(result_df)) {
  id_i <- result_df$id[i]
  val  <- result_df$ccei_col_unedu[i]
  nval <- result_df$n_col_unedu[i]
  
  match_idx <- which(baseline_final_unedu$id == id_i)
  if (length(match_idx) > 0) {
    baseline_final_unedu$ccei_col_unedu[match_idx] <- val
    baseline_final_unedu$n_col_unedu[match_idx] <- nval
  }
}

# 진단용 변수 제거
baseline_final_unedu$n_col_unedu <- NULL

# 저장
save(baseline_final_unedu, file = "../results/baseline_final_unedu.RData")

################################################################

# 3. CCEI_1g, CCEI_2g 계산

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/baseline_raw.RData")
load("../results/baseline_final_unedu.RData")

# 문자열 변환
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final_unedu$id <- as.character(baseline_final_unedu$id)
baseline_final_unedu$partner_id <- as.character(baseline_final_unedu$partner_id)

# 유효 라운드 필터: 개인 1–9 + 그룹 19–27
raw_filtered <- subset(
  baseline_raw,
  (game_type == "individual" & round_number %in% 1:9) |
    (game_type == "collective" & round_number %in% 19:27)
)

# 쌍 중복 제거
raw_filtered <- raw_filtered[raw_filtered$id != raw_filtered$partner_id, ]

# 결과 칼럼 초기화
baseline_final_unedu$ccei_1g_unedu <- NA
baseline_final_unedu$ccei_2g_unedu <- NA
baseline_final_unedu$n_1g <- NA
baseline_final_unedu$n_2g <- NA

# 계산 루프
for (i in seq_len(nrow(baseline_final_unedu))) {
  id1 <- baseline_final_unedu$id[i]
  id2 <- baseline_final_unedu$partner_id[i]
  
  # id1
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    baseline_final_unedu$ccei_1g_unedu[i] <- ccei_garp(p1, x1)
    baseline_final_unedu$n_1g[i] <- nrow(sub1)
  }
  
  # id2
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    baseline_final_unedu$ccei_2g_unedu[i] <- ccei_garp(p2, x2)
    baseline_final_unedu$n_2g[i] <- nrow(sub2)
  }
}

# 진단용 변수 제거
baseline_final_unedu$n_1g <- NULL
baseline_final_unedu$n_2g <- NULL

# 저장
save(baseline_final_unedu, file = "../results/baseline_final_unedu.RData")



#####################################################################

# 4. high_edu / low_edu 구분

rm(list = ls())

# 데이터 불러오기
load("../results/baseline_final_unedu.RData")
load("../data/baseline_raw.RData")

# 문자열 처리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final_unedu$id <- as.character(baseline_final_unedu$id)
baseline_final_unedu$partner_id <- as.character(baseline_final_unedu$partner_id)

# 유효 그룹만 사용 (id ≠ partner_id)
baseline_raw <- baseline_raw[baseline_raw$id != baseline_raw$partner_id, ]

# 초기화
baseline_raw$high_unedu <- 0
baseline_raw$low_unedu <- 0

# 루프: 각 그룹마다 high/low 결정
for (i in 1:nrow(baseline_final_unedu)) {
  id1 <- baseline_final_unedu$id[i]
  id2 <- baseline_final_unedu$partner_id[i]
  
  c1 <- as.numeric(baseline_final_unedu$ccei_1_unedu[i])
  c2 <- as.numeric(baseline_final_unedu$ccei_2_unedu[i])
  c1g <- as.numeric(baseline_final_unedu$ccei_1g_unedu[i])
  c2g <- as.numeric(baseline_final_unedu$ccei_2g_unedu[i])
  
  # default
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
      } else {
        high <- id1; low <- id2  # tie fallback
      }
    }
  }
  
  baseline_raw$high_unedu[baseline_raw$id == high] <- 1
  baseline_raw$low_unedu[baseline_raw$id == low] <- 1
}

# 저장
save(baseline_raw, file = "../data/baseline_raw.RData")




#######################################################

# 1. 데이터 불러오기
load("../data/baseline_raw.RData")
load("../results/baseline_final_unedu.RData")

# 문자열 처리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_final_unedu$id <- as.character(baseline_final_unedu$id)
baseline_final_unedu$group_id <- as.character(baseline_final_unedu$group_id)

# 2. baseline_raw와 group_id 머지 (id 기준)
baseline_raw <- merge(
  baseline_raw, 
  baseline_final_unedu[, c("id", "group_id")],
  by = "id", all.x = TRUE
)

# 3. 필요한 행만 선택 (id ≠ partner_id & group_id 존재)
df <- baseline_raw[baseline_raw$partner_id != "0" & !is.na(baseline_raw$group_id), ]

# 4. 중복 제거 (group 단위로 1명만 유지)
df_unique <- df[!duplicated(df$group_id), ]

# 5. high vs high_unedu 테이블
tbl <- df_unique[, c("group_id", "high", "high_unedu")]
tbl <- tbl[!is.na(tbl$high) & !is.na(tbl$high_unedu), ]
xtab <- table(high = tbl$high, high_unedu = tbl$high_unedu)

# 6. 출력
cat("Confusion table of high vs high_unedu by group (corrected group_id):\n")
print(xtab)


# 시각화용 테이블
library(gt)

# 데이터 프레임으로 변환
xtab_df <- as.data.frame.matrix(xtab)

# 총합
total <- sum(xtab_df)

# 행 이름 붙이기
xtab_df <- tibble::rownames_to_column(xtab_df, var = "high")
xtab_df$high <- paste0("high: ", xtab_df$high)

# 열 이름 정리
colnames(xtab_df)[2:3] <- c("high_unedu: 0", "high_unedu: 1")

# 비율 계산 → 새 데이터프레임
xtab_pct <- xtab
xtab_pct[] <- round(100 * xtab / total, 1)

# 값과 비율 결합
xtab_df[["high_unedu: 0"]] <- paste0(xtab[ , "0"], " (", xtab_pct[ , "0"], "%)")
xtab_df[["high_unedu: 1"]] <- paste0(xtab[ , "1"], " (", xtab_pct[ , "1"], "%)")

# 합계 계산
row_sums <- rowSums(xtab)
col_sums <- colSums(xtab)

# gt 테이블 생성
gt_tbl <- xtab_df %>%
  gt() %>%
  tab_header(
    title = "Confusion Matrix: High vs High_Unedu",
    subtitle = "Grouped by unique group_id"
  ) %>%
  cols_label(high = "") %>%
  fmt_markdown(columns = everything())

# 합계 행 추가
gt_tbl <- gt_tbl %>%
  summary_rows(
    groups = NULL,
    columns = c("high_unedu: 0", "high_unedu: 1"),
    fns = list(Total = ~c(
      paste0(col_sums[1], " (", round(100 * col_sums[1]/total, 1), "%)"),
      paste0(col_sums[2], " (", round(100 * col_sums[2]/total, 1), "%)")
    )),
    missing_text = ""
  ) %>%
  tab_spanner(
    label = "Predicted: high_unedu",
    columns = c("high_unedu: 0", "high_unedu: 1")
  )

# 출력
gt_tbl



#####################################################

# 7. CCEI_hg_edu 결정

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

data_name <- "baseline_raw"
result_name <- "garp_baseline_hg_unedu"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

# high_unedu만 선택
data <- data[data$high_unedu == 1, ]

# 유효 라운드 필터링: 개인 1–9 + 집단 19–27
data <- data[
  (data$round_number >= 1 & data$round_number <= 9) |
    (data$round_number >= 19 & data$round_number <= 27),
]

id_list <- sort(unique(data$id))
cat("CCEI 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- data[data$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_garp(p, x)
  result_matrix[i, 3] <- nrow(sub)  # 사용된 선택 개수
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "ccei_hg_unedu", "n_hg_unedu")
result_df$id <- as.character(result_df$id)
result_df$ccei_hg_unedu <- as.numeric(result_df$ccei_hg_unedu)
result_df$n_hg_unedu <- as.numeric(result_df$n_hg_unedu)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/baseline_final_unedu.RData")

df_hg <- result_df
baseline_final_unedu$id <- as.character(baseline_final_unedu$id)
baseline_final_unedu$partner_id <- as.character(baseline_final_unedu$partner_id)
baseline_final_unedu$ccei_hg_unedu <- NA
baseline_final_unedu$n_hg_unedu <- NA

for (i in 1:nrow(df_hg)) {
  id_i <- df_hg$id[i]
  val  <- df_hg$ccei_hg_unedu[i]
  nval <- df_hg$n_hg_unedu[i]
  
  match_idx <- which(baseline_final_unedu$id == id_i)
  if (length(match_idx) > 0) {
    baseline_final_unedu$ccei_hg_unedu[match_idx] <- val
    baseline_final_unedu$n_hg_unedu[match_idx] <- nval
    next
  }
  
  match_idx <- which(baseline_final_unedu$partner_id == id_i)
  if (length(match_idx) > 0) {
    baseline_final_unedu$ccei_hg_unedu[match_idx] <- val
    baseline_final_unedu$n_hg_unedu[match_idx] <- nval
    next
  }
}

# ------------ 검증 --------------------------

baseline_final_unedu$ccei_1g_unedu <- as.numeric(baseline_final_unedu$ccei_1g_unedu)
baseline_final_unedu$ccei_2g_unedu <- as.numeric(baseline_final_unedu$ccei_2g_unedu)
baseline_final_unedu$ccei_hg_unedu <- as.numeric(baseline_final_unedu$ccei_hg_unedu)

epsilon <- 1e-8
baseline_final_unedu$match_hg_unedu <- ifelse(
  !is.na(baseline_final_unedu$ccei_hg_unedu) &
    (abs(baseline_final_unedu$ccei_hg_unedu - baseline_final_unedu$ccei_1g_unedu) < epsilon |
       abs(baseline_final_unedu$ccei_hg_unedu - baseline_final_unedu$ccei_2g_unedu) < epsilon),
  1, 0
)

cat("match_hg_unedu 결과:\n")
print(table(baseline_final_unedu$match_hg_unedu))

# 진단용 n 제거
baseline_final_unedu$n_hg_unedu <- NULL
baseline_final_unedu$match_hg_unedu <- NULL

save(baseline_final_unedu, file = "../results/baseline_final_unedu.RData")


#####################################################

# 8. CCEI_lg_edu 결정

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

data_name <- "baseline_raw"
result_name <- "garp_baseline_lg_unedu"

load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

# low_unedu 대상 필터
data <- data[data$low_unedu == 1, ]

# 라운드 필터: 개인 1–9, 집단 19–27
data <- data[data$round_number %in% c(1:9, 19:27), ]

id_list <- sort(unique(data$id))
cat("CCEI_lg_unedu 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- data[data$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_garp(p, x)
  result_matrix[i, 3] <- nrow(sub)  # 선택 수 기록
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "ccei_lg_unedu", "n_lg_unedu")
result_df$id <- as.character(result_df$id)
result_df$ccei_lg_unedu <- as.numeric(result_df$ccei_lg_unedu)
result_df$n_lg_unedu <- as.numeric(result_df$n_lg_unedu)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/baseline_final_unedu.RData")

df_lg <- result_df
baseline_final_unedu$id <- as.character(baseline_final_unedu$id)
baseline_final_unedu$partner_id <- as.character(baseline_final_unedu$partner_id)

baseline_final_unedu$ccei_lg_unedu <- NA
baseline_final_unedu$n_lg_unedu <- NA

for (i in 1:nrow(df_lg)) {
  id_i <- df_lg$id[i]
  val  <- df_lg$ccei_lg_unedu[i]
  nval <- df_lg$n_lg_unedu[i]
  
  match_idx <- which(baseline_final_unedu$id == id_i)
  if (length(match_idx) > 0) {
    baseline_final_unedu$ccei_lg_unedu[match_idx] <- val
    baseline_final_unedu$n_lg_unedu[match_idx] <- nval
    next
  }
  
  match_idx <- which(baseline_final_unedu$partner_id == id_i)
  if (length(match_idx) > 0) {
    baseline_final_unedu$ccei_lg_unedu[match_idx] <- val
    baseline_final_unedu$n_lg_unedu[match_idx] <- nval
    next
  }
}

# ------------ 검증 --------------------------

baseline_final_unedu$ccei_1g_unedu <- as.numeric(baseline_final_unedu$ccei_1g_unedu)
baseline_final_unedu$ccei_2g_unedu <- as.numeric(baseline_final_unedu$ccei_2g_unedu)
baseline_final_unedu$ccei_lg_unedu <- as.numeric(baseline_final_unedu$ccei_lg_unedu)

epsilon <- 1e-8

baseline_final_unedu$match_lg_unedu <- ifelse(
  !is.na(baseline_final_unedu$ccei_lg_unedu) &
    (abs(baseline_final_unedu$ccei_lg_unedu - baseline_final_unedu$ccei_1g_unedu) < epsilon |
       abs(baseline_final_unedu$ccei_lg_unedu - baseline_final_unedu$ccei_2g_unedu) < epsilon),
  1, 0
)

cat("match_lg_unedu 결과:\n")
print(table(baseline_final_unedu$match_lg_unedu))

# 진단용 변수 제거
baseline_final_unedu$n_lg_unedu <- NULL
baseline_final_unedu$match_lg_unedu <- NULL

save(baseline_final_unedu, file = "../results/baseline_final_unedu.RData")


################################################################

# 9. CCEI_hlg_edu 계산 및 병합

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 변수 이름 정의
data_name <- "baseline_raw"
result_name <- "garp_baseline_hlg_unedu"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)

# 문자형 변환
data$id <- as.character(data$id)
data$partner_id <- as.character(data$partner_id)
data$game_type <- as.character(data$game_type)
data$mover <- as.character(data$mover)

# partner_id 보정: round_number == 19 기준
library(dplyr)
data <- data %>%
  group_by(id) %>%
  mutate(partner_id = ifelse(round_number >= 1 & round_number <= 36,
                             partner_id[round_number == 19][1],
                             partner_id)) %>%
  ungroup()

# 라운드 필터: 1~9 + 19~27만 사용
data <- data[data$round_number %in% c(1:9, 19:27), ]

# id ≠ partner_id
filtered_data <- data[data$id != data$partner_id, ]

# group_id 생성
filtered_data$group_id <- paste0(
  pmax(filtered_data$id, filtered_data$partner_id),
  pmin(filtered_data$id, filtered_data$partner_id)
)

# 개인 결정 포함
data_indiv <- filtered_data[filtered_data$game_type == "individual", ]

# 공동 결정: mover == "t" 인 경우만
collective_data <- filtered_data[filtered_data$game_type == "collective" & filtered_data$mover == "t", ]

# 병합
data_combined <- rbind(data_indiv, collective_data)

# group_id 기준 CCEI 계산
group_list <- sort(unique(data_combined$group_id))
result_matrix <- matrix(NA, nrow = length(group_list), ncol = 3)

for (i in seq_along(group_list)) {
  g <- group_list[i]
  sub <- data_combined[data_combined$group_id == g, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- g
  result_matrix[i, 2] <- ccei_garp(p, x)
  result_matrix[i, 3] <- nrow(sub)  # 사용된 결정 수 기록
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("group_id", "ccei_hlg_unedu", "n_used_unedu")

result_df$group_id <- as.character(result_df$group_id)
result_df$ccei_hlg_unedu <- as.numeric(result_df$ccei_hlg_unedu)
result_df$n_used_unedu <- as.numeric(result_df$n_used_unedu)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# 병합
load("../results/baseline_final_unedu.RData")

baseline_final_unedu$group_id <- as.character(baseline_final_unedu$group_id)

baseline_final_unedu <- merge(
  baseline_final_unedu, result_df,
  by = "group_id", all.x = TRUE
)

# 진단용 n 제거
baseline_final_unedu$n_used_unedu <- NULL

save(baseline_final_unedu, file = "../results/baseline_final_unedu.RData")

#################################################################

# 10. index 만들기 (unedu 버전)

rm(list = ls())

load("../results/baseline_final_unedu.RData")

# 숫자형 변환
baseline_final_unedu$ccei_col_unedu <- as.numeric(as.character(baseline_final_unedu$ccei_col_unedu))
baseline_final_unedu$ccei_hg_unedu  <- as.numeric(as.character(baseline_final_unedu$ccei_hg_unedu))
baseline_final_unedu$ccei_lg_unedu  <- as.numeric(as.character(baseline_final_unedu$ccei_lg_unedu))
baseline_final_unedu$ccei_hlg_unedu <- as.numeric(as.character(baseline_final_unedu$ccei_hlg_unedu))

# 지수 계산
baseline_final_unedu$index_hg_unedu <- with(
  baseline_final_unedu,
  (ccei_col_unedu - ccei_hg_unedu) / (ccei_col_unedu - ccei_hlg_unedu)
)

baseline_final_unedu$index_lg_unedu <- with(
  baseline_final_unedu,
  (ccei_col_unedu - ccei_lg_unedu) / (ccei_col_unedu - ccei_hlg_unedu)
)

# 변수 이름 변경
names(baseline_final_unedu)[names(baseline_final_unedu) == "ccei_col_unedu"] <- "ccei_g_unedu"

# 변수 순서 재정렬
desired_order <- c(
  "id", "partner_id", "group_id", "mover",
  "ccei_1_unedu", "ccei_2_unedu", "ccei_g_unedu",
  "ccei_1g_unedu", "ccei_2g_unedu",
  "ccei_hg_unedu", "ccei_lg_unedu", "ccei_hlg_unedu",
  "index_hg_unedu", "index_lg_unedu"
)

baseline_final_unedu <- baseline_final_unedu[, desired_order]

# NaN 개수 확인
cat("NaN 개수 확인:\n")
print(colSums(sapply(baseline_final_unedu[, c("index_hg_unedu", "index_lg_unedu")], function(x) is.nan(x))))

# 저장
save(baseline_final_unedu, file = "../results/baseline_final_unedu.RData")



#################################################################

# 11. 분포, 평균, 중앙값

library(ggplot2)
library(dplyr)

rm(list = ls())
load("../results/baseline_final_unedu.RData")

df <- baseline_final_unedu
df <- df[!is.na(df$index_hg_unedu), ]

# 세부 히스토그램 계산
hist_detail <- hist(df$index_hg_unedu, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 0.2 단위 bin 그룹
df$bin_group <- cut(df$index_hg_unedu, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
group_props <- df %>%
  group_by(bin_group) %>%
  summarise(n = n()) %>%
  mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.9, by = 0.2)
  )

# 평균 및 중앙값
x_mean <- mean(df$index_hg_unedu)
x_median <- median(df$index_hg_unedu)
y_max <- max(count_detail)

# 시각화
ggplot(df, aes(x = index_hg_unedu)) +
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
    title = expression(italic("Distribution of  I"[h*",g"]^unedu)),
    x = expression(I[h*",g"]^unedu),
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )

# NaN 개수 확인
sum(is.nan(baseline_final_unedu$index_hg_unedu))



###############################################################

library(ggplot2)
library(dplyr)

# 데이터 로드
rm(list = ls())
load("../results/baseline_final_unedu.RData")

df <- baseline_final_unedu
df <- df[!is.na(df$index_lg_unedu), ]

# 세부 bin (0.05 간격)
hist_detail <- hist(df$index_lg_unedu, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 상단 구간 (0.2 단위 그룹화)
df$bin_group <- cut(df$index_lg_unedu, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
group_props <- df %>%
  group_by(bin_group) %>%
  summarise(n = n()) %>%
  mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.9, by = 0.2)
  )

# 평균 및 중앙값
x_mean <- mean(df$index_lg_unedu)
x_median <- median(df$index_lg_unedu)
y_max <- max(count_detail)

# 플롯
p <- ggplot(df, aes(x = index_lg_unedu)) +
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
    title = expression(italic("Distribution of  I"[l*",g"]^unedu)),
    x = expression(I[l*",g"]^unedu),
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
load("../results/baseline_final_unedu.RData")

df <- baseline_final_unedu

# 수치형 변환
cols <- c("ccei_1_unedu", "ccei_2_unedu", "ccei_g_unedu",
          "ccei_hg_unedu", "ccei_lg_unedu", "ccei_hlg_unedu")
df[cols] <- lapply(df[cols], function(x) as.numeric(as.character(x)))

# high / low 개인 CCEI
df$ccei_h_unedu <- pmax(df$ccei_1_unedu, df$ccei_2_unedu)
df$ccei_l_unedu <- pmin(df$ccei_1_unedu, df$ccei_2_unedu)

# 평균값 계산
mean_df <- data.frame(
  type = factor(c("ccei_h_unedu", "ccei_l_unedu", "ccei_g_unedu",
                  "ccei_hg_unedu", "ccei_lg_unedu", "ccei_hlg_unedu"),
                levels = c("ccei_h_unedu", "ccei_l_unedu", "ccei_g_unedu",
                           "ccei_hg_unedu", "ccei_lg_unedu", "ccei_hlg_unedu")),
  value = c(
    mean(df$ccei_h_unedu, na.rm = TRUE),
    mean(df$ccei_l_unedu, na.rm = TRUE),
    mean(df$ccei_g_unedu, na.rm = TRUE),
    mean(df$ccei_hg_unedu, na.rm = TRUE),
    mean(df$ccei_lg_unedu, na.rm = TRUE),
    mean(df$ccei_hlg_unedu, na.rm = TRUE)
  )
)

# 간결한 x축 라벨 정의
labels <- c(
  "ccei_h_unedu"   = "ccei_h",
  "ccei_l_unedu"   = "ccei_l",
  "ccei_g_unedu"   = "ccei_g",
  "ccei_hg_unedu"  = "ccei_hg",
  "ccei_lg_unedu"  = "ccei_lg",
  "ccei_hlg_unedu" = "ccei_hlg"
)

mean_df$type_label <- factor(mean_df$type,
                             levels = names(labels),
                             labels = labels)

# 그래프 그리기
ggplot(mean_df, aes(x = type_label, y = value, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  
  # 평균값 텍스트 표시
  geom_text(
    data = subset(mean_df, type != "ccei_h_unedu"),
    aes(label = round(value, 3)),
    vjust = -1,
    size = 4
  ) +
  geom_text(
    data = subset(mean_df, type == "ccei_h_unedu"),
    aes(label = round(value, 3)),
    vjust = 1.5,
    size = 4
  ) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Average CCEI by Type (Pre-Education)",
    x = NULL,
    y = "Average CCEI"
  ) +
  theme_minimal(base_size = 14)
