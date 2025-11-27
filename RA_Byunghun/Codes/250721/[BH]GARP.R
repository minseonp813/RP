#. 최종 인덱스 계산용 정리코드드

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

load("../results/garp_baseline_solo.RData")

df <- as.data.frame(garp_baseline_solo, stringsAsFactors = FALSE)

df$id <- as.character(df$id)
df$partner_id <- as.character(df$partner_id)

pairs <- subset(df, partner_id != "0")
pairs$group_id <- paste0(pmax(pairs$id, pairs$partner_id), pmin(pairs$id, pairs$partner_id))

pairs <- pairs[pairs$id != pairs$partner_id, ]
pairs$type <- NULL

names(pairs)[names(pairs) == "ccei"] <- "ccei_1"

# partner_id 기준으로 df$ccei 값 매칭 → ccei_2 생성
ccei_lookup <- df[, c("id", "ccei")]
colnames(ccei_lookup) <- c("partner_id", "ccei_2")  # 열 이름을 맞춰줌

# merge: pairs에 partner_id 기준으로 ccei_2 붙이기
pairs <- merge(pairs, ccei_lookup, by = "partner_id", all.x = TRUE)

pairs <- pairs[, c("id", "partner_id", "group_id", "ccei_1", "ccei_2", "mover")]

baseline_final <- pairs
save(baseline_final, file = "../results/baseline_final.RData")


######################################################

# 0. test baseline solo

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

load("../data/test_baseline_solo.RData")

# 유저 ID 추출
id_symmetric = sort(unique(test_baseline_solo[, 1]))

# 결과 저장용 행렬 초기화
test_garp_baseline_solo = NaN * matrix(1, length(id_symmetric), 2)

# 각 참가자에 대해 CCEI 계산
for (i in 1:length(id_symmetric)) {
  data = test_baseline_solo[test_baseline_solo[, 1] == id_symmetric[i], 3:6]
  p = rbind(1 / data[, 4], 1 / data[, 3])  # prices: px = 1/xmax, py = 1/ymax
  x = rbind(data[, 2], data[, 1])          # choices: x1, x2
  test_garp_baseline_solo[i, 1] = id_symmetric[i]
  test_garp_baseline_solo[i, 2] = ccei_garp(p, x)
}

save(test_garp_baseline_solo, file = "../results/test_garp_baseline_solo.RData")

########################################################

# 1. test baseline solo

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

data_name <- "baseline_solo"
result_name <- "garp_baseline_solo"

load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)

id_symmetric <- sort(unique(data[[1]]))
result_matrix <- matrix(NA, nrow = length(id_symmetric), ncol = 5)

for (i in 1:length(id_symmetric)) {
  id_i <- id_symmetric[i]
  subset_data <- data[data[[1]] == id_i, ]
  
  p <- rbind(1 / subset_data[, 6], 1 / subset_data[, 5])
  x <- rbind(subset_data[, 4], subset_data[, 3])
  ccei_val <- ccei_garp(p, x)
  
  partner_id <- as.character(subset_data[1, 7])
  type_val   <- as.character(subset_data[1, 8])
  mover_val  <- as.character(subset_data[1, 9])
  
  result_matrix[i, ] <- c(id_i, partner_id, ccei_val, type_val, mover_val)
}

colnames(result_matrix) <- c("id", "partner_id", "ccei", "type", "mover")

assign(result_name, result_matrix)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))




##########################################################

# 2. baseline pair

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

load("../data/baseline_pair.RData")

baseline_pair = baseline_pair[baseline_pair[[8]] != 0, ]
baseline_pair = baseline_pair[baseline_pair[[8]] != 0 & baseline_pair[[7]] != baseline_pair[[8]], ]


# 유저 ID 추출
id_symmetric = sort(unique(baseline_pair[, 1]))

# 결과 저장용 행렬 초기화
matrix_baseline_pair = NaN * matrix(1, length(id_symmetric), 2)

# 각 참가자에 대해 CCEI 계산
for (i in 1:length(id_symmetric)) {
  data = baseline_pair[baseline_pair[, 1] == id_symmetric[i], 3:6]
  p = rbind(1 / data[, 4], 1 / data[, 3])  # prices: px = 1/xmax, py = 1/ymax
  x = rbind(data[, 2], data[, 1])          # choices: x1, x2
  matrix_baseline_pair[i, 1] = id_symmetric[i]
  matrix_baseline_pair[i, 2] = ccei_garp(p, x)
}

save(matrix_baseline_pair, file = "../results/garp_baseline_pair.RData")

# 그룹 결정 CCEI 계산까지는 여기까지 완료됨 (이미 작성된 코드)

load("../results/baseline_final.RData")

group_ccei <- as.data.frame(matrix_baseline_pair, stringsAsFactors = FALSE)
colnames(group_ccei) <- c("group_id", "ccei_col")

baseline_final$group_id <- as.character(baseline_final$group_id)
group_ccei$group_id     <- as.character(group_ccei$group_id)

baseline_final <- merge(baseline_final, group_ccei, by = "group_id", all.x = TRUE)

save(baseline_final, file = "../results/baseline_final.RData")



################################################################

# 3. CCEI_1g 계산
# 4. CCEI_2g 계산

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

load("../data/baseline_raw.RData")
load("../results/baseline_final.RData")

baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final$id <- as.character(baseline_final$id)
baseline_final$partner_id <- as.character(baseline_final$partner_id)

# 필터링: id ≠ partner_id
baseline_raw <- baseline_raw[baseline_raw$id != baseline_raw$partner_id, ]

baseline_final$ccei_1g <- NA
baseline_final$ccei_2g <- NA

# 루프: baseline_final 기준으로 1g, 2g 계산
for (i in 1:nrow(baseline_final)) {
  id1 <- baseline_final$id[i]
  id2 <- baseline_final$partner_id[i]
  
  # id1의 데이터 (개인+그룹)
  sub1 <- baseline_raw[baseline_raw$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    baseline_final$ccei_1g[i] <- ccei_garp(p1, x1)
  }
  
  # id2의 데이터 (개인+그룹)
  sub2 <- baseline_raw[baseline_raw$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    baseline_final$ccei_2g[i] <- ccei_garp(p2, x2)
  }
}

#####################################################################

# 5. baseline_final 조정

rm(list = ls())

load("../results/baseline_final.RData")
load("../data/baseline_raw.RData")

baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final$id <- as.character(baseline_final$id)

raw_mover <- subset(baseline_raw, round_number == 19 & id != partner_id)

mover_info <- raw_mover[, c("id", "mover")]
mover_info <- mover_info[!duplicated(mover_info$id), ]

baseline_final <- merge(baseline_final, mover_info, by = "id", all.x = TRUE, suffixes = c("", "_from_raw"))

baseline_final$mover <- ifelse(is.na(baseline_final$mover) | baseline_final$mover == "",
                               baseline_final$mover_from_raw,
                               baseline_final$mover)

baseline_final$mover_from_raw <- NULL

save(baseline_final, file = "../results/baseline_final.RData")

#####################################################################

# 6. high low 구분

rm(list = ls())

load("../results/baseline_final.RData")
load("../data/baseline_raw.RData")

baseline_raw$id <- as.character(baseline_raw$id)
baseline_raw$partner_id <- as.character(baseline_raw$partner_id)
baseline_final$id <- as.character(baseline_final$id)
baseline_final$partner_id <- as.character(baseline_final$partner_id)

baseline_raw <- baseline_raw[baseline_raw$id != baseline_raw$partner_id, ]

baseline_raw$high <- 0
baseline_raw$low <- 0

# 루프: 각 쌍에 대해 high/low 판단 → raw에 반영
for (i in 1:nrow(baseline_final)) {
  id1 <- baseline_final$id[i]
  id2 <- baseline_final$partner_id[i]
  
  c1 <- as.numeric(baseline_final$ccei_1[i])
  c2 <- as.numeric(baseline_final$ccei_2[i])
  
  c1g <- as.numeric(baseline_final$ccei_1g[i])
  c2g <- as.numeric(baseline_final$ccei_2g[i])
  
  # 결정 순서
  if (!is.na(c1) && !is.na(c2) && c1 > c2) {
    high <- id1; low <- id2
  } else if (!is.na(c1) && !is.na(c2) && c2 > c1) {
    high <- id2; low <- id1
  } else if (!is.na(c1g) && !is.na(c2g) && c1g > c2g) {
    high <- id1; low <- id2
  } else if (!is.na(c1g) && !is.na(c2g) && c2g > c1g) {
    high <- id2; low <- id1
  } else {
    high <- id1; low <- id2  # 완전 tie일 경우 id 기준
  }
  
  baseline_raw$high[baseline_raw$id == high] <- 1
  baseline_raw$low[baseline_raw$id == low] <- 1
}

save(baseline_raw, file = "../data/baseline_raw.RData")

#####################################################

# 7. CCEI_hg 결정

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

data_name <- "baseline_raw"
result_name <- "garp_baseline_hg"

load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)

data$id <- as.character(data$id)

# high == 1 인 행만 필터
data <- data[data$high == 1, ]

id_list <- sort(unique(data$id))
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
colnames(result_df) <- c("id", "ccei_hg")

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))


# ------------ merge --------------------------

load("../results/baseline_final.RData")

df_hg <- as.data.frame(garp_baseline_hg, stringsAsFactors = FALSE)
colnames(df_hg) <- c("id", "ccei_hg")
df_hg$id <- as.character(df_hg$id)

baseline_final$id <- as.character(baseline_final$id)
baseline_final$partner_id <- as.character(baseline_final$partner_id)

baseline_final$ccei_hg <- NA

for (i in 1:nrow(df_hg)) {
  id_i <- df_hg$id[i]
  val  <- df_hg$ccei_hg[i]
  
  match_idx <- which(baseline_final$id == id_i)
  if (length(match_idx) > 0) {
    baseline_final$ccei_hg[match_idx] <- val
    next
  }
  
  match_idx <- which(baseline_final$partner_id == id_i)
  if (length(match_idx) > 0) {
    baseline_final$ccei_hg[match_idx] <- val
    next
  }
  
}

save(baseline_final, file = "../results/baseline_final.RData")

# check

epsilon <- 1e-8  # 충분히 작은 값 (필요시 1e-10까지도 가능)

baseline_final$ccei_1g <- as.numeric(baseline_final$ccei_1g)
baseline_final$ccei_2g <- as.numeric(baseline_final$ccei_2g)
baseline_final$ccei_hg <- as.numeric(baseline_final$ccei_hg)

baseline_final$match_hg <- ifelse(
  !is.na(baseline_final$ccei_hg) &
    (abs(baseline_final$ccei_hg - baseline_final$ccei_1g) < epsilon |
       abs(baseline_final$ccei_hg - baseline_final$ccei_2g) < epsilon),
  1, 0
)
table(baseline_final$match_hg)

save(baseline_final, file = "../results/baseline_final.RData")

#####################################################

# 8. CCEI_lg 결정

# ------------ ccei_lg 계산 및 병합 --------------------------

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

data_name <- "baseline_raw"
result_name <- "garp_baseline_lg"

load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)

data$id <- as.character(data$id)

# low == 1 인 행만 필터 (== high == 0)
data <- data[data$high == 0, ]

id_list <- sort(unique(data$id))
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
colnames(result_df) <- c("id", "ccei_lg")
result_df$id <- as.character(result_df$id)

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# ------------ merge to baseline_final --------------------------

load("../results/baseline_final.RData")

df_lg <- result_df
baseline_final$id <- as.character(baseline_final$id)
baseline_final$partner_id <- as.character(baseline_final$partner_id)

baseline_final$ccei_lg <- NA

for (i in 1:nrow(df_lg)) {
  id_i <- df_lg$id[i]
  val  <- df_lg$ccei_lg[i]
  
  match_idx <- which(baseline_final$id == id_i)
  if (length(match_idx) > 0) {
    baseline_final$ccei_lg[match_idx] <- val
    next
  }
  
  match_idx <- which(baseline_final$partner_id == id_i)
  if (length(match_idx) > 0) {
    baseline_final$ccei_lg[match_idx] <- val
    next
  }
}

save(baseline_final, file = "../results/baseline_final.RData")

# ------------ 확인용 매치 여부 --------------------------

epsilon <- 1e-8

baseline_final$ccei_1g <- as.numeric(as.character(baseline_final$ccei_1g))
baseline_final$ccei_lg <- as.numeric(as.character(baseline_final$ccei_lg))

baseline_final$match_lg <- ifelse(
  !is.na(baseline_final$ccei_lg) &
    (abs(baseline_final$ccei_lg - baseline_final$ccei_1g) < epsilon |
       abs(baseline_final$ccei_lg - baseline_final$ccei_2g) < epsilon),
  1, 0
)

table(baseline_final$match_lg)

#############################

# 9. CCEI_hlg 계산 및 병합

rm(list = ls())

source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 변수 이름 정의
data_name <- "baseline_raw"
result_name <- "garp_baseline_hlg"

# 데이터 불러오기
load(paste0("../data/", data_name, ".RData"))
data <- get(data_name)

# 문자형 변환
data$id <- as.character(data$id)
data$partner_id <- as.character(data$partner_id)
data$game_type <- as.character(data$game_type)
data$mover <- as.character(data$mover)

# partner_id 보정: 동일 id 내에서 round_number == 19의 값을 1~36 전체로 복사
library(dplyr)
data <- data %>%
  group_by(id) %>%
  mutate(partner_id = ifelse(round_number >= 1 & round_number <= 36,
                             partner_id[round_number == 19][1],
                             partner_id)) %>%
  ungroup()

# id != partner_id
filtered_data <- data[data$id != data$partner_id, ]

# group_id 생성
filtered_data$group_id <- paste0(pmax(filtered_data$id, filtered_data$partner_id),
                                 pmin(filtered_data$id, filtered_data$partner_id))

# 개인 결정 포함
data_indiv <- filtered_data[filtered_data$game_type == "individual", ]
# 공동 결정: mover == "t" 인 경우만
collective_data <- filtered_data[filtered_data$game_type == "collective" & filtered_data$mover == "t", ]

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
colnames(result_df) <- c("group_id", "ccei_hlg", "n_used")

assign(result_name, result_df)
save(list = result_name, file = paste0("../results/", result_name, ".RData"))

# baseline_final에 병합
load("../results/baseline_final.RData")

df_hlg <- as.data.frame(garp_baseline_hlg, stringsAsFactors = FALSE)
colnames(df_hlg) <- c("group_id", "ccei_hlg", "n_used")

df_hlg$group_id <- as.character(df_hlg$group_id)
baseline_final$group_id <- as.character(baseline_final$group_id)

baseline_final <- merge(baseline_final, df_hlg, by = "group_id", all.x = TRUE)

# 저장
save(baseline_final, file = "../results/baseline_final.RData")

#################################################################

# 10. index 만들기

rm(list = ls())

load("../results/baseline_final.RData")

baseline_final$ccei_g <- as.numeric(as.character(baseline_final$ccei_g))
baseline_final$ccei_hg  <- as.numeric(as.character(baseline_final$ccei_hg))
baseline_final$ccei_lg  <- as.numeric(as.character(baseline_final$ccei_lg))
baseline_final$ccei_hlg <- as.numeric(as.character(baseline_final$ccei_hlg))

baseline_final$index_hg <- with(baseline_final, (ccei_g - ccei_hg) / (ccei_g - ccei_hlg))
baseline_final$index_lg <- with(baseline_final, (ccei_g - ccei_lg) / (ccei_g - ccei_hlg))

# 불필요한 변수 제거
baseline_final$n_used <- NULL

# 변수 순서 재정렬
desired_order <- c(
  "id", "partner_id", "group_id", "mover",
  "ccei_1", "ccei_2", "ccei_g", "ccei_1g", "ccei_2g",
  "ccei_hg", "ccei_lg", "ccei_hlg", "n_used",
  "index_hg", "index_lg"
)

baseline_final <- baseline_final[, desired_order]

# NaN 개수 확인
colSums(sapply(baseline_final[, c("index_hg", "index_lg")], function(x) is.nan(x)))

save(baseline_final, file = "../results/baseline_final.RData")

#################################################################

# 11. 분포, 평균, 중앙값

library(ggplot2)
library(dplyr)

rm(list = ls())
load("../results/baseline_final.RData")

df <- baseline_final
df <- df[!is.na(df$index_hg), ]

hist_detail <- hist(df$index_hg, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

df$bin_group <- cut(df$index_hg, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
group_props <- df %>%
  group_by(bin_group) %>%
  summarise(n = n()) %>%
  mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.9, by = 0.2)
  )

x_mean <- mean(df$index_hg)
x_median <- median(df$index_hg)
y_max <- max(count_detail)

p <- ggplot(df, aes(x = index_hg)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "gray70", color = "black") +
  
  # 막대 중간 텍스트 (10% 이상만)
  geom_text(
    data = data.frame(x = mid_detail, y = count_detail / 2,
                      label = ifelse(prop_detail >= 10, paste0(prop_detail, "%"), "")),
    aes(x = x, y = y, label = label),
    size = 4, fontface = "bold"
  ) +
  
  # 상단 0.2 bin 비율
  geom_text(
    data = group_props,
    aes(x = mid, y = y_max * 1.05, label = paste0(prop, "%")),
    size = 4, fontface = "bold"
  ) +
  
  # 수직선
  geom_vline(xintercept = x_mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = x_median, color = "blue", linetype = "dashed") +
  
  # 수직선 중간에 텍스트 (mean 위쪽, median 아래쪽)
  annotate("text", x = x_mean, y = y_max * 0.5,
           label = paste0("mean = ", round(x_mean, 3)),
           color = "red", size = 4, hjust = 0.5) +
  annotate("text", x = x_median, y = y_max * 0.35,
           label = paste0("median = ", round(x_median, 3)),
           color = "blue", size = 4, hjust = 0.5) +
  
  # x축 눈금: 0.2 단위 표시, 0.1 그리드 제거
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    limits = c(0, 1),
    minor_breaks = NULL
  ) +
  
  labs(
    title = expression(italic("Distribution of  I"[h*",g"])),
    x = expression(I[h*",g"]),
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()  # 추가 보장용
  )

# 출력
print(p)

###############################################################

library(ggplot2)
library(dplyr)

# 데이터 로드
rm(list = ls())
load("../results/baseline_final.RData")

df <- baseline_final
df <- df[!is.na(df$index_lg), ]

# 세부 bin (0.05 간격, 20개)
hist_detail <- hist(df$index_lg, breaks = seq(0, 1, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 상단 구간 (0.2 간격, 5개)
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
y_max <- max(count_detail)

# 플롯
p <- ggplot(df, aes(x = index_lg)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "gray70", color = "black") +
  
  # 막대 중간 텍스트 (5% 이상만)
  geom_text(
    data = data.frame(x = mid_detail, y = count_detail / 2,
                      label = ifelse(prop_detail >= 5, paste0(prop_detail, "%"), "")),
    aes(x = x, y = y, label = label),
    size = 4, fontface = "bold"
  ) +
  
  # 상단 0.2 bin 비율
  geom_text(
    data = group_props,
    aes(x = mid, y = y_max * 1.05, label = paste0(prop, "%")),
    size = 4, fontface = "bold"
  ) +
  
  # 수직선
  geom_vline(xintercept = x_mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = x_median, color = "blue", linetype = "dashed") +
  
  # 수직선 중간 텍스트
  annotate("text", x = x_mean, y = y_max * 0.5,
           label = paste0("mean = ", round(x_mean, 3)),
           color = "red", size = 4, hjust = 0.5) +
  annotate("text", x = x_median, y = y_max * 0.35,
           label = paste0("median = ", round(x_median, 3)),
           color = "blue", size = 4, hjust = 0.5) +
  
  # x축 설정
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.2),
    limits = c(0, 1),
    minor_breaks = NULL
  ) +
  
  labs(
    title = expression(italic("Distribution of  I"[l*",g"])),
    x = expression(I[l*",g"]),
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

######################################################

library(ggplot2)
library(gridExtra)

# 데이터 로드
rm(list = ls())
load("../results/baseline_final.RData")

df <- baseline_final

# 공통 플롯 함수 (coord_fixed 포함)
plot_ccei <- function(xvar, yvar, xlabel, ylabel, title) {
  ggplot(df, aes_string(x = xvar, y = yvar)) +
    geom_point(alpha = 0.5, size = 2, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
    xlim(0, 1) + ylim(0, 1) +
    labs(title = title, x = xlabel, y = ylabel) +
    coord_fixed(ratio = 1) +   # 정사각형 비율 유지
    theme_minimal(base_size = 14)
}

# 4개 산점도 생성
p1 <- plot_ccei("ccei_g",   "ccei_hg",  expression(CCEI[g]),  expression(CCEI[hg]),  "CCEI_g vs CCEI_hg")
p2 <- plot_ccei("ccei_g",   "ccei_lg",  expression(CCEI[g]),  expression(CCEI[lg]),  "CCEI_g vs CCEI_lg")
p3 <- plot_ccei("ccei_hg",  "ccei_hlg", expression(CCEI[hg]), expression(CCEI[hlg]), "CCEI_hg vs CCEI_hlg")
p4 <- plot_ccei("ccei_lg",  "ccei_hlg", expression(CCEI[lg]), expression(CCEI[hlg]), "CCEI_lg vs CCEI_hlg")

print(p1)
print(p2)
print(p3)
print(p4)

library(ggplot2)

##########################################################
# 데이터 로드
rm(list = ls())

# 데이터 다시 불러오기
load("../results/baseline_final.RData")

# df를 안전하게 복사
df <- baseline_final

# 수치형 변환
df$ccei_1 <- as.numeric(as.character(df$ccei_1))
df$ccei_2 <- as.numeric(as.character(df$ccei_2))

# 큰/작은 값 정의
df$ccei_h <- pmax(df$ccei_1, df$ccei_2)
df$ccei_l <- pmin(df$ccei_1, df$ccei_2)
df$delta <- df$ccei_h - df$ccei_l



# 히스토그램 데이터 계산
hist_detail <- hist(df$delta, breaks = seq(0, 0.8, by = 0.05), plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 상단 비율 (0.2 간격 그룹)
df$bin_group <- cut(df$delta, breaks = seq(0, 0.8, by = 0.2), include.lowest = TRUE)
group_props <- df |>
  dplyr::group_by(bin_group) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::mutate(
    prop = round(n / sum(n) * 100, 1),
    mid = seq(0.1, 0.7, by = 0.2)
  )

# 평균/중앙값
x_mean <- mean(df$delta, na.rm = TRUE)
x_median <- median(df$delta, na.rm = TRUE)
y_max <- max(count_detail)

# 플롯
ggplot(df, aes(x = delta)) +
  geom_histogram(breaks = seq(0, 0.8, by = 0.05), fill = "gray70", color = "black") +
  
  # 막대 중간 텍스트 (5% 이상)
  geom_text(
    data = data.frame(x = mid_detail, y = count_detail / 2,
                      label = ifelse(prop_detail >= 5, paste0(prop_detail, "%"), "")),
    aes(x = x, y = y, label = label),
    size = 4, fontface = "bold"
  ) +
  
  # 상단 0.2 그룹 비율
  geom_text(
    data = group_props,
    aes(x = mid, y = y_max * 1.05, label = paste0(prop, "%")),
    size = 4, fontface = "bold"
  ) +
  
  # 수직선
  geom_vline(xintercept = x_mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = x_median, color = "blue", linetype = "dashed") +
  
  # 평균선 주석 (수직선 위, 너무 위는 피함)
  annotate("text", x = x_mean, y = y_max * 0.9,
           label = paste0("mean = ", round(x_mean, 3)),
           color = "red", size = 4, hjust = 0.5) +
  
  # 중앙값선 주석 (수직선 위, mean보다 살짝 아래)
  annotate("text", x = x_median, y = y_max * 0.8,
           label = paste0("median = ", round(x_median, 3)),
           color = "blue", size = 4, hjust = 0.5) +
  
  
  # x축 설정
  scale_x_continuous(
    breaks = seq(0, 0.8, by = 0.2),
    limits = c(0, 0.8),
    minor_breaks = NULL
  ) +
  
  labs(
    title = expression(italic("Distribution of  CCEI"[h] - CCEI[l])),
    x = expression(CCEI[h] - CCEI[l]),
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )

########################################################

library(ggplot2)

# 데이터 로드
rm(list = ls())
load("../results/panel_final.RData")

df <- panel_final

# 수치형 변환 (필요한 칼럼만)
cols <- c("ccei_1", "ccei_2", "ccei_g", "ccei_hg", "ccei_lg", "ccei_hlg")
df[cols] <- lapply(df[cols], function(x) as.numeric(as.character(x)))

# high / low 개인 CCEI 계산
df$ccei_h <- pmax(df$ccei_1, df$ccei_2, na.rm = TRUE)
df$ccei_l <- pmin(df$ccei_1, df$ccei_2, na.rm = TRUE)

# 평균값 계산
mean_df <- data.frame(
  type = factor(c("ccei_h", "ccei_l", "ccei_g", "ccei_hg", "ccei_lg", "ccei_hlg"),
                levels = c("ccei_h", "ccei_l", "ccei_g", "ccei_hg", "ccei_lg", "ccei_hlg")),
  value = c(
    mean(df$ccei_h, na.rm = TRUE),
    mean(df$ccei_l, na.rm = TRUE),
    mean(df$ccei_g, na.rm = TRUE),
    mean(df$ccei_hg, na.rm = TRUE),
    mean(df$ccei_lg, na.rm = TRUE),
    mean(df$ccei_hlg, na.rm = TRUE)
  )
)

# x축 라벨 정리
labels <- c(
  "ccei_h"       = "ccei_h",
  "ccei_l"       = "ccei_l",
  "ccei_g"       = "ccei_g",
  "ccei_hg"      = "ccei_hg",
  "ccei_lg"      = "ccei_lg",
  "ccei_hlg"     = "ccei_hlg"
)

mean_df$type_label <- factor(mean_df$type, levels = names(labels), labels = labels)

# 시각화
ggplot(mean_df, aes(x = type_label, y = value, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  geom_text(aes(label = round(value, 3)), vjust = -1, size = 4) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Average CCEI by Type (Baseline)",
    x = NULL,
    y = "Average CCEI"
  ) +
  theme_minimal(base_size = 14)


library(ggplot2)

# 데이터 로드
rm(list = ls())
load("../results/panel_final.RData")

df <- panel_final

# 수치형 변환 (필요한 endline 변수들)
cols_end <- c("ccei_1_end", "ccei_2_end", "ccei_g_end", "ccei_hg_end", "ccei_lg_end", "ccei_hlg_end")
df[cols_end] <- lapply(df[cols_end], function(x) as.numeric(as.character(x)))

# high / low 개인 CCEI 계산
df$ccei_h_end <- pmax(df$ccei_1_end, df$ccei_2_end, na.rm = TRUE)
df$ccei_l_end <- pmin(df$ccei_1_end, df$ccei_2_end, na.rm = TRUE)

# 평균값 계산
mean_df_end <- data.frame(
  type = factor(c("ccei_h_end", "ccei_l_end", "ccei_g_end", "ccei_hg_end", "ccei_lg_end", "ccei_hlg_end"),
                levels = c("ccei_h_end", "ccei_l_end", "ccei_g_end", "ccei_hg_end", "ccei_lg_end", "ccei_hlg_end")),
  value = c(
    mean(df$ccei_h_end, na.rm = TRUE),
    mean(df$ccei_l_end, na.rm = TRUE),
    mean(df$ccei_g_end, na.rm = TRUE),
    mean(df$ccei_hg_end, na.rm = TRUE),
    mean(df$ccei_lg_end, na.rm = TRUE),
    mean(df$ccei_hlg_end, na.rm = TRUE)
  )
)

# x축 라벨 정리
labels_end <- c(
  "ccei_h_end"   = "ccei_h",
  "ccei_l_end"   = "ccei_l",
  "ccei_g_end"   = "ccei_g",
  "ccei_hg_end"  = "ccei_hg",
  "ccei_lg_end"  = "ccei_lg",
  "ccei_hlg_end" = "ccei_hlg"
)

mean_df_end$type_label <- factor(mean_df_end$type, levels = names(labels_end), labels = labels_end)

# 시각화
ggplot(mean_df_end, aes(x = type_label, y = value, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  geom_text(aes(label = round(value, 3)), vjust = -1, size = 4) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Average CCEI by Type (Endline)",
    x = NULL,
    y = "Average CCEI"
  ) +
  theme_minimal(base_size = 14)

