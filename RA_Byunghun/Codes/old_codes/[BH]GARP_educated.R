################################################################
# 0. 이건 SKIP.

# baseline_final_edu 초기 구성

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

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

baseline_final_edu <- unique_rows[, c("id", "partner_id", "group_id", "mover")]

#  mover 보정

raw_mover <- subset(baseline_raw, round_number == 19 & id != partner_id)
mover_info <- raw_mover[, c("id", "mover")]
mover_info <- mover_info[!duplicated(mover_info$id), ]

baseline_final_edu <- merge(baseline_final_edu, mover_info, by = "id", all.x = TRUE, suffixes = c("", "_from_raw"))

baseline_final_edu$mover <- ifelse(
  is.na(baseline_final_edu$mover) | baseline_final_edu$mover == "",
  baseline_final_edu$mover_from_raw,
  baseline_final_edu$mover
)

baseline_final_edu$mover_from_raw <- NULL

save(baseline_final_edu, file = "../results/baseline_final_edu.RData")


################################################################
# 1. CCEI_1_edu, CCEI_2_edu 계산 (round 1–18 기준)

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/baseline_raw.RData")
load("../results/baseline_final.RData")

# 문자열 정리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final$id <- as.character(baseline_final$id)
baseline_final$partner_id <- as.character(baseline_final$partner_id)

# 필터: 개인 round 10–18만 사용
raw_filtered <- subset(
  baseline_raw,
  game_type == "individual" & round_number %in% 10:18 & id != partner_id
)

# 초기화
baseline_final$ccei_1_edu <- NA
baseline_final$ccei_2_edu <- NA
baseline_final$n_1 <- NA
baseline_final$n_2 <- NA

# 계산 루프
for (i in seq_len(nrow(baseline_final))) {
  id1 <- baseline_final$id[i]
  id2 <- baseline_final$partner_id[i]
  
  # id1
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    baseline_final$ccei_1_edu[i] <- ccei_garp(p1, x1)
    baseline_final$n_1[i] <- nrow(sub1)
  }
  
  # id2
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    baseline_final$ccei_2_edu[i] <- ccei_garp(p2, x2)
    baseline_final$n_2[i] <- nrow(sub2)
  }
}

# 진단용 변수 제거
baseline_final$n_1 <- NULL
baseline_final$n_2 <- NULL

# 저장
save(baseline_final, file = "../results/baseline_final.RData")

###################################################################

# 2. CCEI_col_edu 계산

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/baseline_raw.RData")
load("../results/baseline_final.RData")

# 문자열 변환
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final$id <- as.character(baseline_final$id)

raw_filtered <- subset(
  baseline_raw,
  round_number >= 28 & round_number <= 36 &  
    id != partner_id &                     
    partner_id != "0"                      
)

id_list <- sort(unique(raw_filtered$id))
cat("CCEI_col_edu 계산 대상 ID 수:", length(id_list), "\n")

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
colnames(result_df) <- c("id", "ccei_col_edu", "n_col_edu")
result_df$id <- as.character(result_df$id)
result_df$ccei_col_edu <- as.numeric(result_df$ccei_col_edu)
result_df$n_col_edu <- as.numeric(result_df$n_col_edu)

baseline_final$ccei_col_edu <- NA
baseline_final$n_col_edu <- NA

for (i in 1:nrow(result_df)) {
  id_i <- result_df$id[i]
  val  <- result_df$ccei_col_edu[i]
  nval <- result_df$n_col_edu[i]
  
  match_idx <- which(baseline_final$id == id_i)
  if (length(match_idx) > 0) {
    baseline_final$ccei_col_edu[match_idx] <- val
    baseline_final$n_col_edu[match_idx] <- nval
  }
}

baseline_final$n_col_edu <- NULL

save(baseline_final, file = "../results/baseline_final.RData")


################################################################

# 3. CCEI_1g, CCEI_2g 계산 (1–9, 19–27 제거)


rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/baseline_raw.RData")
load("../results/baseline_final.RData")

# 문자열 변환
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final$id <- as.character(baseline_final$id)
baseline_final$partner_id <- as.character(baseline_final$partner_id)

# 유효 라운드 필터: 개인 10–18 + 그룹 28–36
raw_filtered <- subset(
  baseline_raw,
  (game_type == "individual" & round_number %in% 10:18) |
    (game_type == "collective" & round_number %in% 28:36)
)

# 쌍 중복 제거
raw_filtered <- raw_filtered[raw_filtered$id != raw_filtered$partner_id, ]

# 결과 칼럼 초기화
baseline_final$ccei_1g_edu <- NA
baseline_final$ccei_2g_edu <- NA
baseline_final$n_1g <- NA
baseline_final$n_2g <- NA

# 계산 루프
for (i in seq_len(nrow(baseline_final))) {
  id1 <- baseline_final$id[i]
  id2 <- baseline_final$partner_id[i]
  
  # id1
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    baseline_final$ccei_1g_edu[i] <- ccei_garp(p1, x1)
    baseline_final$n_1g[i] <- nrow(sub1)
  }
  
  # id2
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    baseline_final$ccei_2g_edu[i] <- ccei_garp(p2, x2)
    baseline_final$n_2g[i] <- nrow(sub2)
  }
}

baseline_final$n_1g <- NULL
baseline_final$n_2g <- NULL

save(baseline_final, file = "../results/baseline_final.RData")




#####################################################################

# 3. high_edu / low_edu 구분

rm(list = ls())

# 데이터 불러오기
load("../results/baseline_final.RData")
load("../data/baseline_raw.RData")

# 문자열 처리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final$id <- as.character(baseline_final$id)
baseline_final$partner_id <- as.character(baseline_final$partner_id)

# 유효 그룹만 사용 (id ≠ partner_id)
baseline_raw <- baseline_raw[baseline_raw$id != baseline_raw$partner_id, ]

# 초기화
baseline_raw$high_edu <- 0
baseline_raw$low_edu <- 0

# 루프: 각 그룹마다 high/low 결정
for (i in 1:nrow(baseline_final)) {
  id1 <- baseline_final$id[i]
  id2 <- baseline_final$partner_id[i]
  
  c1 <- as.numeric(baseline_final$ccei_1_edu[i])
  c2 <- as.numeric(baseline_final$ccei_2_edu[i])
  c1g <- as.numeric(baseline_final$ccei_1g_edu[i])
  c2g <- as.numeric(baseline_final$ccei_2g_edu[i])
  
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
        # tie: default로 두기
        high <- id1; low <- id2
      }
    }
  }
  
  baseline_raw$high_edu[baseline_raw$id == high] <- 1
  baseline_raw$low_edu[baseline_raw$id == low] <- 1
}

# 저장
save(baseline_raw, file = "../data/baseline_raw.RData")



#######################################################

# 1. 데이터 불러오기
load("../data/baseline_raw.RData")
load("../results/baseline_final.RData")

# 문자열 처리
baseline_raw$id <- as.character(baseline_raw$id)
baseline_final$id <- as.character(baseline_final$id)
baseline_final$group_id <- as.character(baseline_final$group_id)

# 2. baseline_raw와 group_id 머지 (id 기준)
baseline_raw <- merge(baseline_raw, 
                      baseline_final[, c("id", "group_id")],
                      by = "id", all.x = TRUE)

# 3. 필요한 행만 선택 (id ≠ partner_id & group_id 존재)
df <- baseline_raw[baseline_raw$partner_id != "0" & !is.na(baseline_raw$group_id), ]

# 4. 중복 제거 (group 단위로 1명만 유지)
df_unique <- df[!duplicated(df$group_id), ]

# 5. high vs high_edu 테이블
tbl <- df_unique[, c("group_id", "high", "high_edu")]
tbl <- tbl[!is.na(tbl$high) & !is.na(tbl$high_edu), ]
xtab <- table(high = tbl$high, high_edu = tbl$high_edu)

# 6. 출력
cat("Confusion table of high vs high_edu by group (corrected group_id):\n")
print(xtab)



library(gt)

# 데이터 프레임으로 변환
xtab_df <- as.data.frame.matrix(xtab)

# 총합
total <- sum(xtab_df)

# 행 이름 붙이기
xtab_df <- tibble::rownames_to_column(xtab_df, var = "high")
xtab_df$high <- paste0("high: ", xtab_df$high)

# 열 이름 정리
colnames(xtab_df)[2:3] <- c("high_edu: 0", "high_edu: 1")

# 비율 계산 → 새 데이터프레임
xtab_pct <- xtab
xtab_pct[] <- round(100 * xtab / total, 1)

# 값과 비율 결합
xtab_df[["high_edu: 0"]] <- paste0(xtab[ , "0"], " (", xtab_pct[ , "0"], "%)")
xtab_df[["high_edu: 1"]] <- paste0(xtab[ , "1"], " (", xtab_pct[ , "1"], "%)")

# 합계 계산
row_sums <- rowSums(xtab)
col_sums <- colSums(xtab)

# gt 테이블 생성
gt_tbl <- xtab_df %>%
  gt() %>%
  tab_header(
    title = "Confusion Matrix: High vs High_Edu",
    subtitle = "Grouped by unique group_id"
  ) %>%
  cols_label(high = "") %>%
  fmt_markdown(columns = everything())

# 합계 행 추가
gt_tbl <- gt_tbl %>%
  summary_rows(
    groups = NULL,
    columns = c("high_edu: 0", "high_edu: 1"),
    fns = list(Total = ~c(
      paste0(col_sums[1], " (", round(100 * col_sums[1]/total, 1), "%)"),
      paste0(col_sums[2], " (", round(100 * col_sums[2]/total, 1), "%)")
    )),
    missing_text = ""
  ) %>%
  tab_spanner(
    label = "Predicted: high_edu",
    columns = c("high_edu: 0", "high_edu: 1")
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
result_name <- "garp_baseline_hg_edu"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

data <- data[data$high_edu == 1, ]

data <- data[
  (data$round_number >= 10 & data$round_number <= 18) |
    (data$round_number >= 28 & data$round_number <= 36),
]

id_list <- sort(unique(data$id))
cat("CCEI 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 2)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- data[data$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_garp(p, x)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "ccei_hg_edu")
result_df$id <- as.character(result_df$id)
result_df$ccei_hg_edu <- as.numeric(result_df$ccei_hg_edu)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/baseline_final.RData")

df_hg <- result_df
baseline_final$id <- as.character(baseline_final$id)
baseline_final$partner_id <- as.character(baseline_final$partner_id)
baseline_final$ccei_hg_edu <- NA

for (i in 1:nrow(df_hg)) {
  id_i <- df_hg$id[i]
  val  <- df_hg$ccei_hg_edu[i]
  
  match_idx <- which(baseline_final$id == id_i)
  if (length(match_idx) > 0) {
    baseline_final$ccei_hg_edu[match_idx] <- val
    next
  }
  
  match_idx <- which(baseline_final$partner_id == id_i)
  if (length(match_idx) > 0) {
    baseline_final$ccei_hg_edu[match_idx] <- val
    next
  }
}

# ------------ 검증 --------------------------

baseline_final$ccei_1g_edu <- as.numeric(baseline_final$ccei_1g_edu)
baseline_final$ccei_2g_edu <- as.numeric(baseline_final$ccei_2g_edu)
baseline_final$ccei_hg_edu <- as.numeric(baseline_final$ccei_hg_edu)

epsilon <- 1e-8
baseline_final$match_hg_edu <- ifelse(
  !is.na(baseline_final$ccei_hg_edu) &
    (abs(baseline_final$ccei_hg_edu - baseline_final$ccei_1g_edu) < epsilon |
       abs(baseline_final$ccei_hg_edu - baseline_final$ccei_2g_edu) < epsilon),
  1, 0
)

cat("match_hg_edu 결과:\n")
print(table(baseline_final$match_hg_edu))

save(baseline_final, file = "../results/baseline_final.RData")





#####################################################

# 8. CCEI_lg_edu 결정

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

data_name <- "baseline_raw"
result_name <- "garp_baseline_lg_edu"

load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

data <- data[data$low_edu == 1, ]

data <- data[
  data$round_number %in% c(10:18, 28:36),
]

id_list <- sort(unique(data$id))
cat("CCEI_lg_edu 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 2)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- data[data$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_garp(p, x)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "ccei_lg_edu")
result_df$id <- as.character(result_df$id)
result_df$ccei_lg_edu <- as.numeric(result_df$ccei_lg_edu)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/baseline_final.RData")

df_lg <- result_df
baseline_final$id <- as.character(baseline_final$id)
baseline_final$partner_id <- as.character(baseline_final$partner_id)

baseline_final$ccei_lg_edu <- NA

for (i in 1:nrow(df_lg)) {
  id_i <- df_lg$id[i]
  val  <- df_lg$ccei_lg_edu[i]
  
  match_idx <- which(baseline_final$id == id_i)
  if (length(match_idx) > 0) {
    baseline_final$ccei_lg_edu[match_idx] <- val
    next
  }
  
  match_idx <- which(baseline_final$partner_id == id_i)
  if (length(match_idx) > 0) {
    baseline_final$ccei_lg_edu[match_idx] <- val
    next
  }
}

# ------------ 검증 --------------------------

baseline_final$ccei_1g_edu <- as.numeric(baseline_final$ccei_1g_edu)
baseline_final$ccei_2g_edu <- as.numeric(baseline_final$ccei_2g_edu)
baseline_final$ccei_lg_edu <- as.numeric(baseline_final$ccei_lg_edu)

epsilon <- 1e-8

baseline_final$match_lg_edu <- ifelse(
  !is.na(baseline_final$ccei_lg_edu) &
    (abs(baseline_final$ccei_lg_edu - baseline_final$ccei_1g_edu) < epsilon |
       abs(baseline_final$ccei_lg_edu - baseline_final$ccei_2g_edu) < epsilon),
  1, 0
)

cat("match_lg_edu 결과:\n")
print(table(baseline_final$match_lg_edu))

baseline_final$match_hg <- NULL
baseline_final$match_hg_edu <- NULL
baseline_final$match_lg_edu <- NULL
baseline_final$n_used <- NULL

save(baseline_final, file = "../results/baseline_final.RData")

################################################################

# 9. CCEI_hlg_edu 계산 및 병합

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 변수 이름 정의
data_name <- "baseline_raw"
result_name <- "garp_baseline_hlg_edu"

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

# 라운드 필터: 10~18 + 28~36만 사용
data <- data[data$round_number %in% c(10:18, 28:36), ]

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
colnames(result_df) <- c("group_id", "ccei_hlg_edu", "n_used_edu")

result_df$group_id <- as.character(result_df$group_id)
result_df$ccei_hlg_edu <- as.numeric(result_df$ccei_hlg_edu)
result_df$n_used_edu <- as.numeric(result_df$n_used_edu)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# 병합
load("../results/baseline_final.RData")

baseline_final$group_id <- as.character(baseline_final$group_id)

baseline_final <- merge(
  baseline_final, result_df,
  by = "group_id", all.x = TRUE
)

baseline_final$n_used_edu <- NULL

save(baseline_final, file = "../results/baseline_final.RData")




#################################################################

# 10. index 만들기 (edu 버전)

rm(list = ls())

load("../results/baseline_final.RData")

# 숫자형 변환
baseline_final$ccei_col_edu <- as.numeric(as.character(baseline_final$ccei_col_edu))
baseline_final$ccei_hg_edu  <- as.numeric(as.character(baseline_final$ccei_hg_edu))
baseline_final$ccei_lg_edu  <- as.numeric(as.character(baseline_final$ccei_lg_edu))
baseline_final$ccei_hlg_edu <- as.numeric(as.character(baseline_final$ccei_hlg_edu))

# 지수 계산
baseline_final$index_hg_edu <- with(
  baseline_final,
  (ccei_col_edu - ccei_hg_edu) / (ccei_col_edu - ccei_hlg_edu)
)

baseline_final$index_lg_edu <- with(
  baseline_final,
  (ccei_col_edu - ccei_lg_edu) / (ccei_col_edu - ccei_hlg_edu)
)

# 변수 이름 변경
names(baseline_final)[names(baseline_final) == "ccei_col_edu"] <- "ccei_g_edu"

# 변수 순서 재정렬
desired_order <- c(
  "id", "partner_id", "group_id", "mover",
  "ccei_1_edu", "ccei_2_edu", "ccei_g_edu",
  "ccei_1g_edu", "ccei_2g_edu",
  "ccei_hg_edu", "ccei_lg_edu", "ccei_hlg_edu",
  "index_hg_edu", "index_lg_edu"
)

baseline_final <- baseline_final[, desired_order]

# NaN 개수 확인
cat("NaN 개수 확인:\n")
print(colSums(sapply(baseline_final[, c("index_hg_edu", "index_lg_edu")], function(x) is.nan(x))))

# 저장
save(baseline_final, file = "../results/baseline_final.RData")


#################################################################

# 11. 분포, 평균, 중앙값

library(ggplot2)
library(dplyr)

rm(list = ls())
load("../results/baseline_final.RData")

df <- baseline_final
df <- df[!is.na(df$index_hg_edu), ]

# 세부 히스토그램 계산
hist_detail <- hist(df$index_hg_edu, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 0.2 단위 bin 그룹
df$bin_group <- cut(df$index_hg_edu, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
group_props <- df %>%
  group_by(bin_group) %>%
  summarise(n = n()) %>%
  mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.9, by = 0.2)
  )

# 평균 및 중앙값
x_mean <- mean(df$index_hg_edu)
x_median <- median(df$index_hg_edu)
y_max <- max(count_detail)

# 시각화
ggplot(df, aes(x = index_hg_edu)) +
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
    title = expression(italic("Distribution of  I"[h*",g"]^edu)),
    x = expression(I[h*",g"]^edu),
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )

sum(is.nan(baseline_final$index_hg_edu))


###############################################################

library(ggplot2)
library(dplyr)

# 데이터 로드
rm(list = ls())
load("../results/baseline_final.RData")

df <- baseline_final
df <- df[!is.na(df$index_lg_edu), ]

# 세부 bin (0.05 간격, 20개)
hist_detail <- hist(df$index_lg_edu, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 상단 구간 (0.2 간격, 5개)
df$bin_group <- cut(df$index_lg_edu, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
group_props <- df %>%
  group_by(bin_group) %>%
  summarise(n = n()) %>%
  mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.9, by = 0.2)
  )

# 평균 및 중앙값
x_mean <- mean(df$index_lg_edu)
x_median <- median(df$index_lg_edu)
y_max <- max(count_detail)

# 플롯
p <- ggplot(df, aes(x = index_lg_edu)) +
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
    title = expression(italic("Distribution of  I"[l*",g"]^edu)),
    x = expression(I[l*",g"]^edu),
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
load("../results/baseline_final.RData")


df <- baseline_final

# 수치형 변환
cols <- c("ccei_1_edu", "ccei_2_edu", "ccei_g_edu", "ccei_hg_edu", "ccei_lg_edu", "ccei_hlg_edu")
df[cols] <- lapply(df[cols], function(x) as.numeric(as.character(x)))

# high / low 구성
df$ccei_h_edu <- pmax(df$ccei_1_edu, df$ccei_2_edu)
df$ccei_l_edu <- pmin(df$ccei_1_edu, df$ccei_2_edu)

# 평균값 계산
mean_df <- data.frame(
  type = factor(c("ccei_h_edu", "ccei_l_edu", "ccei_g_edu", "ccei_hg_edu", "ccei_lg_edu", "ccei_hlg_edu"),
                levels = c("ccei_h_edu", "ccei_l_edu", "ccei_g_edu", "ccei_hg_edu", "ccei_lg_edu", "ccei_hlg_edu")),
  value = c(
    mean(df$ccei_h_edu, na.rm = TRUE),
    mean(df$ccei_l_edu, na.rm = TRUE),
    mean(df$ccei_g_edu, na.rm = TRUE),
    mean(df$ccei_hg_edu, na.rm = TRUE),
    mean(df$ccei_lg_edu, na.rm = TRUE),
    mean(df$ccei_hlg_edu, na.rm = TRUE)
  )
)

ggplot(mean_df, aes(x = type, y = value, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  
  # 나머지 값: 점 위
  geom_text(
    data = subset(mean_df, type != "ccei_h_edu"),
    aes(label = round(value, 3)),
    vjust = -1,
    size = 4
  ) +
  
  # ccei_h_edu만 점 아래
  geom_text(
    data = subset(mean_df, type == "ccei_h_edu"),
    aes(label = round(value, 3)),
    vjust = 1.5,
    size = 4
  ) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Average CCEI by Type (Post-Education)",
    x = NULL,
    y = "Average CCEI"
  ) +
  theme_minimal(base_size = 14)


