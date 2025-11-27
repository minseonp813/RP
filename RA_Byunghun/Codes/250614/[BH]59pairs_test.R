# 모든 객체 초기화
rm(list = ls())

# 외부 R 스크립트 로드
source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

# 데이터 불러오기
load("../data/baseline_raw.RData")
load("../results/baseline_final.RData")

# high_id, ccei_h, ccei_l 초기화
baseline_final$high_id <- NA
baseline_final$ccei_h <- NA
baseline_final$ccei_l <- NA

# 루프: 각 그룹마다 high_id, ccei_h, ccei_l 결정
for (i in 1:nrow(baseline_final)) {
  id1 <- baseline_final$id[i]
  id2 <- baseline_final$partner_id[i]
  
  c1  <- as.numeric(baseline_final$ccei_1[i])
  c2  <- as.numeric(baseline_final$ccei_2[i])
  c1g <- as.numeric(baseline_final$ccei_1g[i])
  c2g <- as.numeric(baseline_final$ccei_2g[i])
  
  if (!is.na(c1) && !is.na(c2)) {
    if (c1 > c2) {
      baseline_final$high_id[i] <- id1
      baseline_final$ccei_h[i] <- c1
      baseline_final$ccei_l[i] <- c2
    } else if (c2 > c1) {
      baseline_final$high_id[i] <- id2
      baseline_final$ccei_h[i] <- c2
      baseline_final$ccei_l[i] <- c1
    } else if (!is.na(c1g) && !is.na(c2g)) {
      if (c1g > c2g) {
        baseline_final$high_id[i] <- id1
        baseline_final$ccei_h[i] <- c1
        baseline_final$ccei_l[i] <- c2
      } else if (c2g > c1g) {
        baseline_final$high_id[i] <- id2
        baseline_final$ccei_h[i] <- c2
        baseline_final$ccei_l[i] <- c1
      } else {
        baseline_final$high_id[i] <- id1  # tie → 기본값
        baseline_final$ccei_h[i] <- c1
        baseline_final$ccei_l[i] <- c2
      }
    } else {
      baseline_final$high_id[i] <- id1  # 기본값
      baseline_final$ccei_h[i] <- c1
      baseline_final$ccei_l[i] <- c2
    }
  }
  # else: high_id, ccei_h/l는 그대로 NA
}

# index_hg가 NaN이면 59쌍
baseline_final$pairs59 <- ifelse(is.nan(baseline_final$index_hg), 1, 0)

# 필요한 변수만 선택
output <- baseline_final[, c("id", "partner_id", "ccei_g", "ccei_h", "ccei_l", "high_id", "pairs59")]

# 저장
write.csv(output, file = "59pairs.csv", row.names = FALSE)

########################################

# ccei_1과 ccei_2를 하나의 벡터로 결합
all_ccei <- as.numeric(c(baseline_final$ccei_1, baseline_final$ccei_2))
all_ccei <- na.omit(all_ccei)


summary(all_ccei)  # Min, 1st Qu., Median, Mean, 3rd Qu., Max

# 또는 직접 사분위수만
quantile(all_ccei, probs = c(0.25, 0.5, 0.75))





library(ggplot2)

# 59쌍 필터링
df <- subset(baseline_final, pairs59 == 1)

# 스캐터플롯
ggplot(df, aes(x = ccei_l, y = ccei_g)) +
  geom_point(color = "darkorange", size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  coord_fixed() +
  xlim(0, 1) +
  ylim(0, 1) +
  labs(
    title = "Scatterplot of CCEI_l vs. CCEI_g (59 pairs)",
    x = "Low individual's CCEI (ccei_l)",
    y = "Group CCEI (ccei_g)"
  ) +
  theme_minimal(base_size = 14)



ggplot(df, aes(x = ccei_l, y = ccei_h)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  coord_fixed() +
  xlim(0, 1) +
  ylim(0, 1) +
  labs(
    title = "Scatterplot of CCEI_l vs. CCEI_h (59 matched pairs)",
    x = "Low individual's CCEI (ccei_l)",
    y = "High individual's CCEI (ccei_h)"
  ) +
  theme_minimal(base_size = 14)

ggplot(df, aes(x = ccei_l, y = ccei_h)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  coord_fixed() +  # <- 이것이 핵심입니다
  labs(
    title = "Scatterplot of CCEI_l vs. CCEI_h (59 matched pairs)",
    x = "Low individual's CCEI (ccei_l)",
    y = "High individual's CCEI (ccei_h)"
  ) +
  theme_minimal(base_size = 14)











# 59쌍만 필터링
df <- subset(baseline_final, pairs59 == 1)

# CCEI 차이 변수 추가
df$diff_hl <- df$ccei_h - df$ccei_l

# 히스토그램 그리기
ggplot(df, aes(x = diff_hl)) +
  geom_histogram(binwidth = 0.02, fill = "tomato", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  labs(
    title = "Histogram of CCEI Difference (High - Low)",
    x = "CCEI_h - CCEI_l",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)





library(ggplot2)

# 59쌍만 필터링
df <- subset(baseline_final, pairs59 == 1)

# 시각화
ggplot(df, aes(x = ccei_h, y = ccei_g)) +
  geom_point(color = "steelblue", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "High individual's CCEI",
    y = "Group CCEI",
    title = "Group vs. High CCEI (59 matched pairs)"
  ) +
  theme_minimal()






library(ggplot2)

# 59쌍만 필터링
df <- subset(baseline_final, pairs59 == 1)

# 밀도 함수 시각화
ggplot(df, aes(x = ccei_g)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgreen") +
  labs(
    title = "Distribution of Group CCEI (ccei_g) in 59 matched pairs",
    x = "Group CCEI (ccei_g)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)





library(ggplot2)
library(dplyr)

# 0 ~ 1 사이 0.05 단위로 자르기
df_binned <- df %>%
  mutate(bin = cut(ccei_g, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE))

# 구간별 개수로 히스토그램
ggplot(df_binned, aes(x = bin)) +
  geom_bar(fill = "skyblue", color = "white") +
  geom_vline(xintercept = which(levels(df_binned$bin) == "[0.95,1]"), 
             linetype = "dashed", color = "darkgreen") +
  labs(
    title = "Histogram of Group CCEI (ccei_g)",
    x = "Group CCEI (binned in 0.05 intervals)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##################################################################
rm(list = ls())

# 필요한 함수 로드
source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

load("../data/baseline_000.RData")
load("../results/baseline_final.RData")

id_list <- sort(unique(baseline_000$id))
results <- NaN * matrix(1, length(id_list), 2)

for (i in 1:length(id_list)) {
  person_data <- subset(baseline_000, id == id_list[i])
  
  # 소비벡터 x: coord_x, coord_y
  x <- t(cbind(person_data$coord_x, person_data$coord_y))
  
  # 가격벡터 p: 1 / intercept_x, 1 / intercept_y
  # 단, 0이 있는 경우는 NA로 처리 (0으로 나눌 수 없음)
  px <- 1 / person_data$intercept_x
  py <- 1 / person_data$intercept_y
  px[is.infinite(px)] <- NA
  py[is.infinite(py)] <- NA
  p <- t(cbind(px, py))
  
  results[i, 1] <- id_list[i]
  results[i, 2] <- ccei_garp(p, x)
}

# 저장
colnames(results) <- c("id", "ccei")






############################################################

