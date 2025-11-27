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
endline_final <- unique_groups[, c("id", "partner_id", "group_id", "mover")]

# 5. mover 정보 보정 (round_number == 19 기준)
raw_mover <- subset(endline_raw, round_number == 19 & id != partner_id)
mover_info <- raw_mover[, c("id", "mover")]
mover_info <- mover_info[!duplicated(mover_info$id), ]

endline_final <- merge(
  endline_final, mover_info,
  by = "id", all.x = TRUE, suffixes = c("", "_from_raw")
)

# 6. 기존 mover 값이 비어있으면 보정값으로 채움
endline_final$mover <- ifelse(
  is.na(endline_final$mover) | endline_final$mover == "",
  endline_final$mover_from_raw,
  endline_final$mover
)

# 7. 보정용 임시 열 제거
endline_final$mover_from_raw <- NULL

# 저장
save(endline_final, file = "../results/endline_final.RData")




################################################################

# 1. CCEI_1, CCEI_2 계산

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/endline_raw.RData")
load("../results/endline_final.RData")

# 문자열 정리
endline_raw$id <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)
endline_final$id <- as.character(endline_final$id)
endline_final$partner_id <- as.character(endline_final$partner_id)

# 개인 round 1–18 사용
raw_filtered <- subset(
  endline_raw,
  game_type == "individual" & round_number %in% 1:18 & id != partner_id
)

# 초기화
endline_final$ccei_1 <- NA
endline_final$ccei_2 <- NA
endline_final$n_1 <- NA
endline_final$n_2 <- NA

# 계산 루프
for (i in seq_len(nrow(endline_final))) {
  id1 <- endline_final$id[i]
  id2 <- endline_final$partner_id[i]
  
  # id1
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    endline_final$ccei_1[i] <- ccei_garp(p1, x1)
    endline_final$n_1[i] <- nrow(sub1)
  }
  
  # id2
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    endline_final$ccei_2[i] <- ccei_garp(p2, x2)
    endline_final$n_2[i] <- nrow(sub2)
  }
}

# 진단용 변수 제거
endline_final$n_1 <- NULL
endline_final$n_2 <- NULL

# 저장
save(endline_final, file = "../results/endline_final.RData")



###################################################################

# 2. CCEI_col 계산

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/endline_raw.RData")
load("../results/endline_final.RData")

# 문자열 정리
endline_raw$id <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)
endline_final$id <- as.character(endline_final$id)

# 집단 결정 라운드 19–36 필터
raw_filtered <- subset(
  endline_raw,
  round_number >= 19 & round_number <= 36 &
    id != partner_id &
    partner_id != "0"
)

id_list <- sort(unique(raw_filtered$id))
cat("CCEI_col 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- raw_filtered[raw_filtered$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_garp(p, x)
  result_matrix[i, 3] <- nrow(sub)  # 진단용
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "ccei_col", "n_col")
result_df$id <- as.character(result_df$id)
result_df$ccei_col <- as.numeric(result_df$ccei_col)
result_df$n_col <- as.numeric(result_df$n_col)

endline_final$ccei_col <- NA
endline_final$n_col <- NA

for (i in 1:nrow(result_df)) {
  id_i <- result_df$id[i]
  val  <- result_df$ccei_col[i]
  nval <- result_df$n_col[i]
  
  match_idx <- which(endline_final$id == id_i)
  if (length(match_idx) > 0) {
    endline_final$ccei_col[match_idx] <- val
    endline_final$n_col[match_idx] <- nval
  }
}

# 진단용 제거
endline_final$n_col <- NULL

# 저장
save(endline_final, file = "../results/endline_final.RData")


################################################################

# 3. CCEI_1g, CCEI_2g 계산

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 로드
load("../data/endline_raw.RData")
load("../results/endline_final.RData")

# 문자열 변환
endline_raw$id <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)
endline_final$id <- as.character(endline_final$id)
endline_final$partner_id <- as.character(endline_final$partner_id)

# 유효 라운드 필터: 개인 1–18 + 집단 19–36
raw_filtered <- subset(
  endline_raw,
  (game_type == "individual" & round_number %in% 1:18) |
    (game_type == "collective" & round_number %in% 19:36)
)

# 쌍 중복 제거
raw_filtered <- raw_filtered[raw_filtered$id != raw_filtered$partner_id, ]

# 결과 칼럼 초기화
endline_final$ccei_1g <- NA
endline_final$ccei_2g <- NA
endline_final$n_1g <- NA
endline_final$n_2g <- NA

# 계산 루프
for (i in seq_len(nrow(endline_final))) {
  id1 <- endline_final$id[i]
  id2 <- endline_final$partner_id[i]
  
  # id1
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    endline_final$ccei_1g[i] <- ccei_garp(p1, x1)
    endline_final$n_1g[i] <- nrow(sub1)
  }
  
  # id2
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    endline_final$ccei_2g[i] <- ccei_garp(p2, x2)
    endline_final$n_2g[i] <- nrow(sub2)
  }
}

# 진단용 변수 제거
endline_final$n_1g <- NULL
endline_final$n_2g <- NULL

# 저장
save(endline_final, file = "../results/endline_final.RData")




#####################################################################

# 4. high / low 구분

rm(list = ls())

# 데이터 불러오기
load("../results/endline_final.RData")
load("../data/endline_raw.RData")

# 문자열 처리
endline_raw$id <- as.character(endline_raw$id)
endline_raw$partner_id <- as.character(endline_raw$partner_id)
endline_final$id <- as.character(endline_final$id)
endline_final$partner_id <- as.character(endline_final$partner_id)

# 유효 그룹만 사용 (id ≠ partner_id)
endline_raw <- endline_raw[endline_raw$id != endline_raw$partner_id, ]

# 초기화
endline_raw$high <- 0
endline_raw$low <- 0

# 루프: 각 그룹마다 high/low 결정
for (i in 1:nrow(endline_final)) {
  id1 <- endline_final$id[i]
  id2 <- endline_final$partner_id[i]
  
  c1 <- as.numeric(endline_final$ccei_1[i])
  c2 <- as.numeric(endline_final$ccei_2[i])
  c1g <- as.numeric(endline_final$ccei_1g[i])
  c2g <- as.numeric(endline_final$ccei_2g[i])
  
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
      } else {
        high <- id1; low <- id2  # 동률 시 fallback
      }
    }
  }
  
  endline_raw$high[endline_raw$id == high] <- 1
  endline_raw$low[endline_raw$id == low] <- 1
}

# 저장
save(endline_raw, file = "../data/endline_raw.RData")


#####################################################

# 7. CCEI_hg 계산

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

data_name <- "endline_raw"
result_name <- "garp_endline_hg"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

# high만 선택
data <- data[data$high == 1, ]

# 유효 라운드 필터링: 개인 1–18 + 집단 19–36
data <- data[
  (data$round_number >= 1 & data$round_number <= 18) |
    (data$round_number >= 19 & data$round_number <= 36),
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
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "ccei_hg", "n_hg")
result_df$id <- as.character(result_df$id)
result_df$ccei_hg <- as.numeric(result_df$ccei_hg)
result_df$n_hg <- as.numeric(result_df$n_hg)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/endline_final.RData")

df_hg <- result_df
endline_final$id <- as.character(endline_final$id)
endline_final$partner_id <- as.character(endline_final$partner_id)
endline_final$ccei_hg <- NA
endline_final$n_hg <- NA

for (i in 1:nrow(df_hg)) {
  id_i <- df_hg$id[i]
  val  <- df_hg$ccei_hg[i]
  nval <- df_hg$n_hg[i]
  
  match_idx <- which(endline_final$id == id_i)
  if (length(match_idx) > 0) {
    endline_final$ccei_hg[match_idx] <- val
    endline_final$n_hg[match_idx] <- nval
    next
  }
  
  match_idx <- which(endline_final$partner_id == id_i)
  if (length(match_idx) > 0) {
    endline_final$ccei_hg[match_idx] <- val
    endline_final$n_hg[match_idx] <- nval
    next
  }
}

# ------------ 검증 --------------------------

endline_final$ccei_1g <- as.numeric(endline_final$ccei_1g)
endline_final$ccei_2g <- as.numeric(endline_final$ccei_2g)
endline_final$ccei_hg <- as.numeric(endline_final$ccei_hg)

epsilon <- 1e-8
endline_final$match_hg <- ifelse(
  !is.na(endline_final$ccei_hg) &
    (abs(endline_final$ccei_hg - endline_final$ccei_1g) < epsilon |
       abs(endline_final$ccei_hg - endline_final$ccei_2g) < epsilon),
  1, 0
)

cat("match_hg 결과:\n")
print(table(endline_final$match_hg))

# 진단용 제거
endline_final$n_hg <- NULL
endline_final$match_hg <- NULL

save(endline_final, file = "../results/endline_final.RData")




#####################################################

# 8. CCEI_lg 계산

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

data_name <- "endline_raw"
result_name <- "garp_endline_lg"

load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)
data$id <- as.character(data$id)

# low 대상 필터
data <- data[data$low == 1, ]

# 라운드 필터: 개인 1–18, 집단 19–36
data <- data[data$round_number %in% c(1:18, 19:36), ]

id_list <- sort(unique(data$id))
cat("CCEI_lg 계산 대상 ID 수:", length(id_list), "\n")

result_matrix <- matrix(NA, nrow = length(id_list), ncol = 3)

for (i in seq_along(id_list)) {
  id_i <- id_list[i]
  sub <- data[data$id == id_i, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  result_matrix[i, 1] <- id_i
  result_matrix[i, 2] <- ccei_garp(p, x)
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("id", "ccei_lg", "n_lg")
result_df$id <- as.character(result_df$id)
result_df$ccei_lg <- as.numeric(result_df$ccei_lg)
result_df$n_lg <- as.numeric(result_df$n_lg)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/endline_final.RData")

df_lg <- result_df
endline_final$id <- as.character(endline_final$id)
endline_final$partner_id <- as.character(endline_final$partner_id)

endline_final$ccei_lg <- NA
endline_final$n_lg <- NA

for (i in 1:nrow(df_lg)) {
  id_i <- df_lg$id[i]
  val  <- df_lg$ccei_lg[i]
  nval <- df_lg$n_lg[i]
  
  match_idx <- which(endline_final$id == id_i)
  if (length(match_idx) > 0) {
    endline_final$ccei_lg[match_idx] <- val
    endline_final$n_lg[match_idx] <- nval
    next
  }
  
  match_idx <- which(endline_final$partner_id == id_i)
  if (length(match_idx) > 0) {
    endline_final$ccei_lg[match_idx] <- val
    endline_final$n_lg[match_idx] <- nval
    next
  }
}

# ------------ 검증 --------------------------

endline_final$ccei_1g <- as.numeric(endline_final$ccei_1g)
endline_final$ccei_2g <- as.numeric(endline_final$ccei_2g)
endline_final$ccei_lg <- as.numeric(endline_final$ccei_lg)

epsilon <- 1e-8

endline_final$match_lg <- ifelse(
  !is.na(endline_final$ccei_lg) &
    (abs(endline_final$ccei_lg - endline_final$ccei_1g) < epsilon |
       abs(endline_final$ccei_lg - endline_final$ccei_2g) < epsilon),
  1, 0
)

cat("match_lg 결과:\n")
print(table(endline_final$match_lg))

# 진단용 변수 제거
endline_final$n_lg <- NULL
endline_final$match_lg <- NULL

save(endline_final, file = "../results/endline_final.RData")



################################################################

# 9. CCEI_hlg 계산 및 병합

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 변수 이름 정의
data_name <- "endline_raw"
result_name <- "garp_endline_hlg"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)

# 문자형 변환
data$id <- as.character(data$id)
data$partner_id <- as.character(data$partner_id)
data$game_type <- as.character(data$game_type)
data$mover <- as.character(data$mover)

# 숫자로 된 mover 값을 문자형 "t"/"f"로 변환
data$mover[data$mover == "1"] <- "t"
data$mover[data$mover == "0"] <- "f"

# partner_id 보정: round_number == 19 기준
library(dplyr)
data <- data %>%
  group_by(id) %>%
  mutate(partner_id = ifelse(round_number >= 1 & round_number <= 36,
                             partner_id[round_number == 19][1],
                             partner_id)) %>%
  ungroup()

# 라운드 필터: 개인 1–18, 집단 19–36
data <- data[data$round_number %in% c(1:18, 19:36), ]

# id ≠ partner_id
filtered_data <- data[data$id != data$partner_id, ]

# group_id 생성
filtered_data$group_id <- paste0(
  pmax(filtered_data$id, filtered_data$partner_id),
  pmin(filtered_data$id, filtered_data$partner_id)
)

# 개인 결정
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
  result_matrix[i, 3] <- nrow(sub)
}

result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("group_id", "ccei_hlg", "n_used")
result_df$group_id <- as.character(result_df$group_id)
result_df$ccei_hlg <- as.numeric(result_df$ccei_hlg)
result_df$n_used <- as.numeric(result_df$n_used)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ 병합 --------------------------

load("../results/endline_final.RData")

endline_final$group_id <- as.character(endline_final$group_id)

endline_final <- merge(
  endline_final, result_df,
  by = "group_id", all.x = TRUE
)


# 진단용 제거
endline_final$n_used <- NULL

save(endline_final, file = "../results/endline_final.RData")


#################################################################

# 10. index 만들기

rm(list = ls())

load("../results/endline_final.RData")

# 숫자형 변환
endline_final$ccei_col <- as.numeric(as.character(endline_final$ccei_col))
endline_final$ccei_hg  <- as.numeric(as.character(endline_final$ccei_hg))
endline_final$ccei_lg  <- as.numeric(as.character(endline_final$ccei_lg))
endline_final$ccei_hlg <- as.numeric(as.character(endline_final$ccei_hlg))

# 지수 계산
endline_final$index_hg <- with(
  endline_final,
  (ccei_col - ccei_hg) / (ccei_col - ccei_hlg)
)

endline_final$index_lg <- with(
  endline_final,
  (ccei_col - ccei_lg) / (ccei_col - ccei_hlg)
)

# 변수 이름 변경
names(endline_final)[names(endline_final) == "ccei_col"] <- "ccei_g"

# 변수 순서 재정렬
desired_order <- c(
  "id", "partner_id", "group_id", "mover",
  "ccei_1", "ccei_2", "ccei_g",
  "ccei_1g", "ccei_2g",
  "ccei_hg", "ccei_lg", "ccei_hlg",
  "index_hg", "index_lg"
)

endline_final <- endline_final[, desired_order]

# NaN 개수 확인
cat("NaN 개수 확인:\n")
print(colSums(sapply(endline_final[, c("index_hg", "index_lg")], function(x) is.nan(x))))

# 저장
save(endline_final, file = "../results/endline_final.RData")



#################################################################


# 패널 만들기

# ----- 패널 1: group_id 기준 병합 (wide panel: panel_final) -----

rm(list = ls())

load("../results/baseline_final.RData")
load("../results/endline_final.RData")

# endline 변수명 뒤에 _end 붙이기 (id, partner_id, group_id 제외)
cols_to_rename <- setdiff(names(endline_final), c("id", "partner_id", "group_id"))
names(endline_final)[names(endline_final) %in% cols_to_rename] <- 
  paste0(cols_to_rename, "_end")

# 병합: group_id 기준, 내부조인 (공통 group_id만)
panel_final <- merge(
  baseline_final,
  endline_final,
  by = "group_id",
  all = FALSE
)


id_flipped <- panel_final$id.x == panel_final$partner_id.y
# 순서 뒤바뀐 행 찾기
flip_idx <- which(panel_final$id.x == panel_final$partner_id.y)

# 값 스왑
for (i in flip_idx) {
  # swap ccei_1_end <-> ccei_2_end
  temp <- panel_final$ccei_1_end[i]
  panel_final$ccei_1_end[i] <- panel_final$ccei_2_end[i]
  panel_final$ccei_2_end[i] <- temp
  
  # swap ccei_1g_end <-> ccei_2g_end
  temp <- panel_final$ccei_1g_end[i]
  panel_final$ccei_1g_end[i] <- panel_final$ccei_2g_end[i]
  panel_final$ccei_2g_end[i] <- temp
}

# 저장
save(panel_final, file = "../results/panel_final.RData")


# ----- 패널 2: long panel 만들기 (panel_pbl: for DID etc.) -----

rm(list = ls())

load("../results/baseline_final.RData")
load("../results/endline_final.RData")

# 시점 변수 추가
baseline_final$t <- 0
endline_final$t  <- 1

# 공통 칼럼만 유지
common_cols <- intersect(names(baseline_final), names(endline_final))
panel_pbl <- rbind(
  baseline_final[, c(common_cols, "t")],
  endline_final[, c(common_cols, "t")]
)

panel_pbl$t.1 <- NULL

# 저장
save(panel_pbl, file = "../results/panel_pbl.RData")

#########################################################

rm(list = ls())
load("../results/panel_final.RData")

# 수치형 변환 보장
panel_final$ccei_1      <- as.numeric(panel_final$ccei_1)
panel_final$ccei_2      <- as.numeric(panel_final$ccei_2)
panel_final$ccei_1g     <- as.numeric(panel_final$ccei_1g)
panel_final$ccei_2g     <- as.numeric(panel_final$ccei_2g)
panel_final$ccei_1_end  <- as.numeric(panel_final$ccei_1_end)
panel_final$ccei_2_end  <- as.numeric(panel_final$ccei_2_end)
panel_final$ccei_1g_end <- as.numeric(panel_final$ccei_1g_end)
panel_final$ccei_2g_end <- as.numeric(panel_final$ccei_2g_end)

# baseline 기준 high 생성
panel_final$high <- ifelse(
  panel_final$ccei_1 > panel_final$ccei_2, 1,
  ifelse(
    panel_final$ccei_1 < panel_final$ccei_2, 0,
    ifelse(
      panel_final$ccei_1g > panel_final$ccei_2g, 1,
      ifelse(
        panel_final$ccei_1g < panel_final$ccei_2g, 0,
        1
      )
    )
  )
)

# endline 기준 high_end 생성
panel_final$high_end <- ifelse(
  panel_final$ccei_1_end > panel_final$ccei_2_end, 1,
  ifelse(
    panel_final$ccei_1_end < panel_final$ccei_2_end, 0,
    ifelse(
      panel_final$ccei_1g_end > panel_final$ccei_2g_end, 1,
      ifelse(
        panel_final$ccei_1g_end < panel_final$ccei_2g_end, 0,
        1
      )
    )
  )
)

# 확인
table(panel_final$high, useNA = "always")
table(panel_final$high_end, useNA = "always")


# 저장 (선택)
save(panel_final, file = "../results/panel_final.RData")


###########################################################
# ------------------------------
# Baseline: Group-level RT_g 계산
# ------------------------------

rm(list = ls())
load("../results/panel_final.RData")
load("../data/baseline_raw.RData")

library(dplyr)

# 1. collective 결정만 필터링
df <- baseline_raw %>%
  filter(game_type == "collective")

# 2. Risk Tolerance 계산
df <- df %>%
  mutate(
    x_cheaper_asset = ifelse(intercept_x > intercept_y, coord_x, coord_y),
    RT = x_cheaper_asset / (coord_x + coord_y)
  )

# 3. id 기준으로 평균 RT 및 라운드 수 계산
rt_id <- df %>%
  group_by(id) %>%
  summarise(
    RT = mean(RT, na.rm = TRUE),
    n = n()
  )

# 4. panel_final에 매칭
panel_final$RT_g       <- rt_id$RT[match(panel_final$id.x, rt_id$id)]
panel_final$RT_g_check <- rt_id$RT[match(panel_final$partner_id.x, rt_id$id)]

# 5. 검증
cor_check <- cor(panel_final$RT_g, panel_final$RT_g_check, use = "complete.obs")
cat("Correlation between RT_g and RT_g_check:", round(cor_check, 5), "\n")

# 6. 확인 후 제거
panel_final$RT_g_check <- NULL

# 7. 저장
save(panel_final, file = "../results/panel_final.RData")

#######################################################################

# ------------------------------
# Endline: Group-level RT_g 계산
# ------------------------------

rm(list = ls())
load("../results/panel_final.RData")
load("../data/endline_raw.RData")

library(dplyr)

# 1. collective 결정만 필터링
df <- endline_raw %>%
  filter(game_type == "collective")

# 2. Risk Tolerance 계산
df <- df %>%
  mutate(
    x_cheaper_asset = ifelse(intercept_x > intercept_y, coord_x, coord_y),
    RT = x_cheaper_asset / (coord_x + coord_y)
  )

# 3. id 기준으로 평균 RT 및 라운드 수 계산
rt_id <- df %>%
  group_by(id) %>%
  summarise(
    RT = mean(RT, na.rm = TRUE),
    n = n()
  )

# 4. panel_final에 매칭
panel_final$RT_g_end       <- rt_id$RT[match(panel_final$id.y, rt_id$id)]
panel_final$RT_g_end_check <- rt_id$RT[match(panel_final$partner_id.y, rt_id$id)]

# 5. 검증
cor_check <- cor(panel_final$RT_g_end, panel_final$RT_g_end_check, use = "complete.obs")
cat("Correlation between RT_g_end and RT_g_end_check:", round(cor_check, 5), "\n")

# 6. 확인 후 제거
panel_final$RT_g_end_check <- NULL

# 7. 저장
save(panel_final, file = "../results/panel_final.RData")


#############################################################

rm(list = ls())
load("../results/panel_final.RData")
load("../data/endline_raw.RData")
load("../data/baseline_raw.RData")

library(dplyr)

# 개인결정만 필터
df <- baseline_raw %>%
  filter(game_type == "individual")

# x가 더 싼 경우 x재 선택, 아니면 y재 선택
df <- df %>%
  mutate(
    x_cheaper_asset = ifelse(intercept_x > intercept_y, coord_x, coord_y),
    RT = x_cheaper_asset / (coord_x + coord_y)
  )

# 개인별 평균 RT 계산
rt_baseline <- df %>%
  group_by(id) %>%
  summarise(RT = mean(RT, na.rm = TRUE))  # id, RT

# panel_final 에 매칭
panel_final$RT_1     <- rt_baseline$RT[match(panel_final$id.x, rt_baseline$id)]
panel_final$RT_2     <- rt_baseline$RT[match(panel_final$partner_id.x, rt_baseline$id)]

save(panel_final, file = "../results/panel_final.RData")


#########################################################

rm(list = ls())
load("../results/panel_final.RData")
load("../data/endline_raw.RData")

library(dplyr)

# 1. 개인결정만 필터
df <- endline_raw %>%
  filter(game_type == "individual")

# 2. x가 더 싼 경우 x재 선택, 아니면 y재 선택
df <- df %>%
  mutate(
    x_cheaper_asset = ifelse(intercept_x > intercept_y, coord_x, coord_y),
    RT = x_cheaper_asset / (coord_x + coord_y)
  )

# 3. 개인별 평균 RT 계산
rt_endline <- df %>%
  group_by(id) %>%
  summarise(RT = mean(RT, na.rm = TRUE))  # id, RT

# 4. panel_final에 매칭
panel_final$RT_1_end <- rt_endline$RT[match(panel_final$id.y, rt_endline$id)]
panel_final$RT_2_end <- rt_endline$RT[match(panel_final$partner_id.y, rt_endline$id)]

# 순서 바뀐 행 인덱스
flip_idx <- which(panel_final$id.x == panel_final$partner_id.y)

# RT_1_end <-> RT_2_end 스왑
temp <- panel_final$RT_1_end[flip_idx]
panel_final$RT_1_end[flip_idx] <- panel_final$RT_2_end[flip_idx]
panel_final$RT_2_end[flip_idx] <- temp

summary(panel_final$RT_1_end)
summary(panel_final$RT_2_end)
table(is.na(panel_final$RT_1_end))
table(is.na(panel_final$RT_2_end))


# 5. 저장
save(panel_final, file = "../results/panel_final.RData")



##########################################################

# 11. 분포, 평균, 중앙값

library(ggplot2)
library(dplyr)

rm(list = ls())
load("../results/panel_final.RData")

# baseline 기준 index 추출
df <- panel_final
df <- df[!is.na(df$index_hg), ]  # index_hg는 baseline 기준

# 세부 히스토그램 계산
hist_detail <- hist(df$index_hg, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 0.2 단위 bin 그룹
df$bin_group <- cut(df$index_hg, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
group_props <- df %>%
  group_by(bin_group) %>%
  summarise(n = n()) %>%
  mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.9, by = 0.2)
  )

# 평균 및 중앙값
x_mean <- mean(df$index_hg)
x_median <- median(df$index_hg)
y_max <- 250

# 시각화
ggplot(df, aes(x = index_hg)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "gray70", color = "black") +
  
  geom_text(
    data = data.frame(x = mid_detail, y = count_detail / 2,
                      label = ifelse(prop_detail >= 10, paste0(prop_detail, "%"), "")),
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
  
  scale_y_continuous(
    limits = c(0, 280),
    breaks = seq(0, 300, by = 50),
    expand = c(0, 0)
  ) +
  
  labs(
    title = expression(paste(italic("Distribution of "), I[h*","*g], " (baseline)")),
    x = expression(I[h*","*g]),
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )

# NaN 개수 확인
table(is.nan(panel_final$index_hg))
table(is.nan(panel_final$index_hg_end))
table(is.nan(panel_final$index_hg) | is.nan(panel_final$index_hg_end) )

#################################################################

library(ggplot2)
library(dplyr)

rm(list = ls())
load("../results/panel_final.RData")

# endline 기준 index 추출
df <- panel_final
df <- df[!is.na(df$index_hg_end), ]  # index_hg_end 기준

# 세부 히스토그램 계산
hist_detail <- hist(df$index_hg_end, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 0.2 단위 bin 그룹
df$bin_group <- cut(df$index_hg_end, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
group_props <- df %>%
  group_by(bin_group) %>%
  summarise(n = n()) %>%
  mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.9, by = 0.2)
  )

# 평균 및 중앙값
x_mean <- mean(df$index_hg_end)
x_median <- median(df$index_hg_end)
y_max <- 250

# 시각화
ggplot(df, aes(x = index_hg_end)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "gray70", color = "black") +
  
  geom_text(
    data = data.frame(x = mid_detail, y = count_detail / 2,
                      label = ifelse(prop_detail >= 10, paste0(prop_detail, "%"), "")),
    aes(x = x, y = y, label = label),
    size = 4, fontface = "bold"
  ) +
  
  geom_text(
    data = group_props,
    aes(x = mid, y = y_max * 0.95, label = paste0(prop, "%")),  # y축 범위 안쪽으로 조정
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
    limits = c(0, 280),
    breaks = seq(0, 300, by = 50),
    expand = c(0, 0)
  ) +
  
  labs(
    title = expression(paste(italic("Distribution of "), I[h*","*g], " (endline)")),
    x = expression(I[h*","*g]),
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
