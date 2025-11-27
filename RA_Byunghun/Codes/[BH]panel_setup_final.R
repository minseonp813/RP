library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("../results/panel_final.RData")
load("../results/panel_pbl.RData")

# 교차표 생성
table(panel_final$high, panel_final$high_end)


# 1. long 형식으로 변환
ccei_long <- panel_final %>%
  select(group_id, ccei_1, ccei_2) %>%
  pivot_longer(
    cols = starts_with("ccei_"),
    names_to = "member",
    values_to = "ccei"
  ) %>%
  arrange(group_id)

# 2. 중간값 계산
ccei_median <- median(ccei_long$ccei, na.rm = TRUE)

# 3. 중간값과 비교해서 그룹별 합산
panel_final <- panel_final %>%
  mutate(
    ccei_both_median = 
      as.integer(ccei_1 >= ccei_median) + as.integer(ccei_2 >= ccei_median)
  )

panel_final %>%
  count(ccei_both_median)

#################################

# 1. long 형식으로 변환
ccei_long_end <- panel_final %>%
  select(group_id, ccei_1_end, ccei_2_end) %>%
  pivot_longer(
    cols = starts_with("ccei_"),
    names_to = "member",
    values_to = "ccei"
  ) %>%
  arrange(group_id)

summary(ccei_long_end$ccei)

# 더 세부적인 사분위수 (0%, 25%, 50%, 75%, 100%)
quantile(ccei_long$ccei, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# 2. 중간값 계산
ccei_median_end <- median(ccei_long_end$ccei, na.rm = TRUE)

# 3. 중간값과 비교해서 그룹별 합산
panel_final <- panel_final %>%
  mutate(
    ccei_both_median_end = 
      as.integer(ccei_1_end >= ccei_median_end) + as.integer(ccei_2_end >= ccei_median_end)
  )

# 4. 빈도 확인
panel_final %>%
  count(ccei_both_median_end)

save(panel_final, file = "../results/panel_final.RData")
#######################################################

rm(list = ls())
load("../results/panel_final.RData")

library(ggplot2)
library(dplyr)

# 그룹 라벨: 0 → "Low, Low", 1 → "Low, High", 2 → "High, High"
panel_final <- panel_final %>%
  mutate(group_label = factor(
    ccei_both_median,
    levels = c(0, 1, 2),
    labels = c("(Low, Low)", "(Low, High)", "(High, High)")
  ))

# CDF plot (baseline, Stata 스타일 범례 포함)
cdf_baseline_plot <- ggplot(panel_final, aes(x = ccei_g, color = group_label, linetype = group_label)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective Rationality",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0.1, 1.0), breaks = seq(0.2, 1.0, by = 0.2)) +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.text = element_text(size = 12),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = "cdf_baseline_median.png",
  path = "C:/Users/hahn0/Desktop/RP Slides",
  width = 7, height = 5, dpi = 300
)

################################


# 그룹 라벨 생성 (endline 기준)
panel_final <- panel_final %>%
  mutate(group_label_end = factor(
    ccei_both_median_end,
    levels = c(0, 1, 2),
    labels = c("(Low, Low)", "(Low, High)", "(High, High)")
  ))

# CDF plot (endline)
cdf_endline_plot <- ggplot(panel_final, aes(x = ccei_g_end, color = group_label_end, linetype = group_label_end)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective Rationality",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0.1, 1.0), breaks = seq(0.2, 1.0, by = 0.2)) +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1.5, "lines"),         # 항목 높이 조절
    legend.text = element_text(size = 12),        # 텍스트 크기 증가
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA)
  )

# 저장
ggsave(
  filename = "cdf_endline_median.png",
  plot = cdf_endline_plot,
  path = "C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/figures",
  width = 7, height = 5, dpi = 300
)

################################################33

library(haven)
library(dplyr)
library(stringr)

rm(list = ls())
load("../results/panel_final.RData")
load("../results/panel_pbl.RData")

# 두 사람의 개인 합리성 지수 중 최대값
panel_final <- panel_final %>%
  mutate(
    ccei_ind_max = pmax(ccei_1, ccei_2, na.rm = TRUE),
    ccei_ind_dist = round(abs(ccei_1 - ccei_2), 6)
  )

# 2. Stata 파일 불러오기
stata_data <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/Risk_ByPair_for_merge.dta")

# 3. group_id 생성
stata_data <- stata_data %>%
  mutate(
    high_id = pmax(id, partner),
    low_id = pmin(id, partner),
    group_id = str_c(high_id, low_id, sep = "")  # group_id는 문자열
  )

# 4. 필요한 변수만 추출
stata_selected <- stata_data %>%
  select(group_id,
         class,
         mathscore_max, mathscore_dist,
         height_gr_max, height_gr_dist,
         mathscore_max_missing, mathscore_dist_missing,
         height_gr_max_missing, height_gr_dist_missing,
         malepair_co, friendship)

# 5. R의 panel_final과 병합
panel_final <- panel_final %>%
  left_join(stata_selected, by = "group_id")

panel_final <- panel_final %>%
  select(-height_gr_max_missing, -height_gr_dist_missing)

# 두 사람의 개인 합리성 지수 중 최대값
panel_final <- panel_final %>%
  mutate(
    ccei_ind_max_end = pmax(ccei_1_end, ccei_2_end, na.rm = TRUE),
    ccei_ind_dist_end = round(abs(ccei_1_end - ccei_2_end), 6)
  )

save(panel_final, file = "../results/panel_final.RData")

###############################################################


library(haven)
library(dplyr)
library(stringr)

rm(list = ls())
load("../results/panel_final.RData")
load("../results/panel_pbl.RData")

# 2. Stata 파일 불러오기
stata_data <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/Risk_ByPair_for_merge.dta")

# 3. group_id 생성
stata_data <- stata_data %>%
  mutate(
    high_id = pmax(id, partner),
    low_id = pmin(id, partner),
    group_id = str_c(high_id, low_id, sep = "")  # group_id는 문자열
  )

# 4. 필요한 변수만 추출
stata_selected <- stata_data %>%
  select(group_id,
         mathscore, mathscore2,
         )

# 5. R의 panel_final과 병합
panel_final <- panel_final %>%
  left_join(stata_selected, by = "group_id")

save(panel_final, file = "../results/panel_final.RData")

##############################################################



library(haven)
library(dplyr)
library(stringr)

# 1. 기존 분석 데이터 로드
rm(list = ls())
load("../results/panel_final.RData")

# 2. 기존 group_id 목록 저장
valid_group_ids <- unique(panel_final$group_id)

# 3. 친구 지목 데이터 불러오기 (t == 0만 남기기)
network_raw <- read_dta("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/network_survey.dta")
network_t0 <- network_raw %>% filter(t == 0)
network_t1 <- network_raw %>% filter(t == 1)


# 함수 정의: 특정 사람이 특정인을 친구로 지목했는지 확인
check_friendship <- function(data, id, target) {
  any(data$id == id & data$obj_id == target)
}

# 새 변수 생성
panel_final <- panel_final %>%
  rowwise() %>%
  mutate(
    friendship_1 = as.integer(check_friendship(network_t0, id.x, partner_id.x)),
    friendship_2 = as.integer(check_friendship(network_t0, partner_id.x, id.x)),
    friendship_1_end = as.integer(check_friendship(network_t1, id.x, partner_id.x)),
    friendship_2_end = as.integer(check_friendship(network_t1, partner_id.x, id.x))
  ) %>%
  ungroup()

save(panel_final, file = "../results/panel_final.RData")


##############################################

library(haven)
library(dplyr)
library(stringr)

# 1. 기존 분석 데이터 로드
rm(list = ls())
load("../results/panel_final.RData")

# 2. 기존 group_id 목록 저장
valid_group_ids <- unique(panel_final$group_id)

# 3. 친구 지목 데이터 불러오기 (t == 1만 남기기)
network_raw <- read_dta("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/network_survey.dta") %>%
  filter(t == 1)

# 4. group_id처럼 조합 만들기: id, obj_id 중 큰 것 + "" + 작은 것
network_raw <- network_raw %>%
  mutate(
    big_id = pmax(id, obj_id),
    small_id = pmin(id, obj_id),
    group_id = str_c(big_id, small_id, sep = "")
  )

# 5. 실제 분석에 사용된 그룹만 남기기
network_filtered <- network_raw %>%
  filter(group_id %in% valid_group_ids)

# 1. group_id별 친구 지목 횟수 계산 (1번 or 2번)
friendship_end_df <- network_filtered %>%
  count(group_id, name = "friendship_end")

# 2. 기존 panel_final에 머지 (group_id 기준)
panel_final <- panel_final %>%
  left_join(friendship_end_df, by = "group_id") %>%
  mutate(friendship_end= ifelse(is.na(friendship_end), 0, friendship_end))

save(panel_final, file = "../results/panel_final.RData")

# 1. 교차표 생성
tbl <- table(panel_final$friendship, panel_final$friendship_end)

# 2. 행 기준(row-wise) 비율 계산
prop <- prop.table(tbl, margin = 1)

# 3. count + 퍼센트 같이 출력
result <- paste0(tbl, " (", round(100 * prop, 1), "%)")
dim(result) <- dim(tbl)  # reshape to match table

# 4. 행렬 형태로 보기 좋게 데이터프레임으로 변환
rownames(result) <- rownames(tbl)
colnames(result) <- colnames(tbl)
as.data.frame.matrix(result)

##############################################

# Table 2. Regression

rm(list = ls())
load("../results/panel_final.RData")

library(haven)

panel_final <- panel_final %>%
  rename_with(~ gsub("\\.", "_", .x))

write_dta(panel_final, "panel_final.dta")






######################################

# panel_pbl

rm(list = ls())
load("../results/panel_final.RData")

names(panel_final)

# 1️⃣ 변수 목록 (최종 칼럼 이름 기준)
vars <- c(
  "group_id", "ccei_1", "ccei_2", "ccei_g", "index_hg", "index_lg",
  "ccei_ind_max", "ccei_ind_dist", "height_gr_max", "height_gr_dist",
  "mathscore_max", "mathscore_dist", "mathscore_max_missing",
  "mathscore_dist_missing", "malepair_co", "friendship", "class", "RT_1", "RT_2"
)

# 2️⃣ matrix1: baseline 데이터
matrix1 <- panel_final[vars]
matrix1$time <- 0

# 3️⃣ matrix2: endline 데이터 템플릿 (NA로 초기화)
matrix2 <- matrix(NA, nrow = nrow(panel_final), ncol = length(vars),
                  dimnames = list(NULL, vars))
matrix2 <- as.data.frame(matrix2)

# group_id는 panel_final에서 그대로 가져오기
matrix2$group_id <- panel_final$group_id

# endline에서 값을 가져와야 할 변수들 (_end 제거 후 붙이기)
endline_map <- c(
  "ccei_1_end" = "ccei_1",
  "ccei_2_end" = "ccei_2",
  "ccei_g_end" = "ccei_g",
  "index_hg_end" = "index_hg",
  "index_lg_end" = "index_lg",
  "ccei_ind_max_end" = "ccei_ind_max",
  "ccei_ind_dist_end" = "ccei_ind_dist",
  "friendship_end" = "friendship",
  "RT_1_end" = "RT_1", 
  "RT_2_end" = "RT_2"
)

# 채워넣기
for (orig in names(endline_map)) {
  target <- endline_map[[orig]]
  matrix2[[target]] <- panel_final[[orig]]
}

# 나머지 공통 변수들: 그대로 복사
shared_vars <- setdiff(vars, c("ccei_1", "ccei_2", "ccei_g", "index_hg", "index_lg",
                               "ccei_ind_max", "ccei_ind_dist", "friendship", "RT_1", "RT_2"))
for (var in shared_vars) {
  matrix2[[var]] <- panel_final[[var]]
}

# time 부여
matrix2$time <- 1

# 4️⃣ 두 매트릭스 합치기
panel_pbl <- rbind(matrix1, matrix2)

# 정렬
panel_pbl <- panel_pbl[order(panel_pbl$group_id, panel_pbl$time), ]


# risk_aversion_max: 두 사람 중 큰 값
panel_pbl$risk_aversion_max <- pmax(panel_pbl$RT_1, panel_pbl$RT_2, na.rm = TRUE)

# risk_aversion_distance: 절댓값 차이
panel_pbl$risk_aversion_distance <- round(abs(panel_pbl$RT_1 - panel_pbl$RT_2), 7)




# 5️⃣ 저장
save(panel_pbl, file = "../results/panel_pbl.RData")

library(haven)
write_dta(panel_pbl, "../results/panel_pbl.dta")


######################################

# panel_pbl

rm(list = ls())
load("../results/panel_final.RData")
names(panel_final)

summarize_real_data <- function(df, suffix = "") {
  hg_col <- paste0("ccei_hg", suffix)
  lg_col <- paste0("ccei_lg", suffix)
  ihg_col <- paste0("index_hg", suffix)
  ilg_col <- paste0("index_lg", suffix)
  
  # (1) Frequencies of CCEI_hg vs CCEI_lg
  diff <- df[[hg_col]] - df[[lg_col]]
  relation <- ifelse(diff > 1e-6, ">", ifelse(diff < -1e-6, "<", "="))
  freq_table <- table(relation)
  freq_prop <- prop.table(freq_table)
  
  cat("\n==========\n")
  cat(paste("CCEI_hg", suffix, "vs CCEI_lg", suffix, "\n"))
  print(data.frame(Frequency = freq_table, Proportion = round(freq_prop, 4)))
  
  # (2) Distributions of I_hg and I_lg
  par(mfrow = c(1, 2))
  hist(df[[ihg_col]], breaks = seq(0, 1, by = 0.05), col = "lightblue",
       main = paste0("Distribution of ", ihg_col), xlab = ihg_col)
  abline(v = mean(df[[ihg_col]], na.rm = TRUE), col = "red", lty = 2)
  abline(v = median(df[[ihg_col]], na.rm = TRUE), col = "blue", lty = 2)
  
  hist(df[[ilg_col]], breaks = seq(0, 1, by = 0.05), col = "lightgreen",
       main = paste0("Distribution of ", ilg_col), xlab = ilg_col)
  abline(v = mean(df[[ilg_col]], na.rm = TRUE), col = "red", lty = 2)
  abline(v = median(df[[ilg_col]], na.rm = TRUE), col = "blue", lty = 2)
  par(mfrow = c(1,1))
  
  # (3) Frequency of index_hg > index_lg
  comparison <- df[[ihg_col]] > df[[ilg_col]]
  bar_data <- table(comparison)
  bar_prop <- prop.table(bar_data)
  
  barplot(bar_data,
          main = paste0("index_hg", suffix, " > index_lg", suffix),
          col = "gray",
          ylim = c(0, max(bar_data) * 1.2))
  text(x = c(0.7, 1.9), y = bar_data + 10,
       labels = paste0(round(bar_prop * 100, 1), "%"), cex = 1.2)
}

# Baseline
summarize_real_data(panel_final, suffix = "")

# Endline
summarize_real_data(panel_final, suffix = "_end")





library(ggplot2)

# 차이 변수 생성
panel_final$diff_index_lg_hg <- panel_final$index_lg - panel_final$index_hg
df <- panel_final[!is.na(panel_final$diff_index_lg_hg), ]

# 히스토그램용 범위
breaks_seq <- seq(-1, 1, by = 0.05)
hist_detail <- hist(df$diff_index_lg_hg, breaks = breaks_seq, plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 평균과 중앙값
x_mean <- mean(df$diff_index_lg_hg)
x_median <- median(df$diff_index_lg_hg)
y_max <- max(count_detail)

# ggplot 시각화
ggplot(df, aes(x = diff_index_lg_hg)) +
  geom_histogram(breaks = breaks_seq, fill = "gray70", color = "black") +
  geom_text(
    data = data.frame(x = mid_detail, y = count_detail,
                      label = ifelse(prop_detail >= 10, paste0(prop_detail, "%"), "")),
    aes(x = x, y = y * 0.6, label = label),
    size = 4, fontface = "bold"
  ) +
  geom_vline(xintercept = x_mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = x_median, color = "blue", linetype = "dashed") +
  annotate("text", x = x_mean, y = y_max * 1,
           label = paste0("mean = ", round(x_mean, 3)),
           color = "red", size = 4, hjust = 0.5) +
  annotate("text", x = x_median, y = y_max * 0.9,
           label = paste0("median = ", round(x_median, 3)),
           color = "blue", size = 4, hjust = 0.5) +
  scale_x_continuous(
    breaks = seq(-1, 1, by = 0.2),
    limits = c(-0.5, 1),
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    limits = c(0, y_max * 1.2),
    expand = c(0, 0)
  ) +
  labs(
    title = "Distribution of I_lg - I_hg (Baseline)",
    x = "index_lg - index_hg",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )

#######################################################

######## old. 일단 이건 works fine ##########

# 준비
rm(list = ls())
load("../results/panel_final.RData")

names(panel_final)

# 결과 저장용 리스트
individual_rows <- list()

# 반복 (baseline과 endline 모두 포함)
for (t in c(0, 1)) {
  # 시점별 변수 선택
  is_end <- t == 1
  ccei_1g <- if (is_end) panel_final$ccei_1g_end else panel_final$ccei_1g
  ccei_2g <- if (is_end) panel_final$ccei_2g_end else panel_final$ccei_2g
  index_hg <- if (is_end) panel_final$index_hg_end else panel_final$index_hg
  index_lg <- if (is_end) panel_final$index_lg_end else panel_final$index_lg
  high <- if (is_end) panel_final$high_end else panel_final$high
  
  # 반복문
  for (i in 1:nrow(panel_final)) {
    g_id <- panel_final$group_id[i]
    
    # 첫 번째 개인 (id.x는 항상 ccei_1g)
    row1 <- data.frame(
      id = panel_final$id.x[i],
      group_id = g_id,
      time = t,
      CCEI_ig = ccei_1g[i],
      I_ig = if (high[i] == 1) index_hg[i] else index_lg[i],
      high_dummy = high[i],
      ccei_ind_max = if (is_end) panel_final$ccei_ind_max_end[i] else panel_final$ccei_ind_max[i],
      ccei_ind_dist = if (is_end) panel_final$ccei_ind_dist_end[i] else panel_final$ccei_ind_dist[i],
      height_gr_max = panel_final$height_gr_max[i],
      height_gr_dist = panel_final$height_gr_dist[i],
      mathscore_max = panel_final$mathscore_max[i],
      mathscore_dist = panel_final$mathscore_dist[i],
      mathscore_max_missing = panel_final$mathscore_max_missing[i],
      mathscore_dist_missing = panel_final$mathscore_dist_missing[i],
      malepair_co = panel_final$malepair_co[i],
      friendship = if (is_end) panel_final$friendship_end[i] else panel_final$friendship[i],
      class = panel_final$class[i],
      stringsAsFactors = FALSE
    )
    
    # 두 번째 개인 (partner_id.x는 항상 ccei_2g)
    row2 <- data.frame(
      id = panel_final$partner_id.x[i],
      group_id = g_id,
      time = t,
      CCEI_ig = ccei_2g[i],
      I_ig = if (high[i] == 1) index_lg[i] else index_hg[i],
      high_dummy = 1 - high[i],
      ccei_ind_max = if (is_end) panel_final$ccei_ind_max_end[i] else panel_final$ccei_ind_max[i],
      ccei_ind_dist = if (is_end) panel_final$ccei_ind_dist_end[i] else panel_final$ccei_ind_dist[i],
      height_gr_max = panel_final$height_gr_max[i],
      height_gr_dist = panel_final$height_gr_dist[i],
      mathscore_max = panel_final$mathscore_max[i],
      mathscore_dist = panel_final$mathscore_dist[i],
      mathscore_max_missing = panel_final$mathscore_max_missing[i],
      mathscore_dist_missing = panel_final$mathscore_dist_missing[i],
      malepair_co = panel_final$malepair_co[i],
      friendship = if (is_end) panel_final$friendship_end[i] else panel_final$friendship[i],
      class = panel_final$class[i],
      stringsAsFactors = FALSE
    )
    
    individual_rows[[length(individual_rows) + 1]] <- row1
    individual_rows[[length(individual_rows) + 1]] <- row2
  }
}

# 데이터프레임으로 결합
panel_individual <- do.call(rbind, individual_rows)

# 확인
dim(panel_individual)  # 2628행 예상
table(panel_individual$time)
length(unique(panel_individual$group_id))  # 657
length(unique(panel_individual$id))        # 1314

# 저장
save(panel_individual, file = "../results/panel_individual.RData")
library(haven)
write_dta(panel_individual, "../results/panel_individual.dta")

#######################################################

# 준비
rm(list = ls())
load("../results/panel_final.RData")

names(panel_final)

# 결과 저장용 리스트
individual_rows <- list()

# 반복 (baseline과 endline 모두 포함)
for (t in c(0, 1)) {
  # 시점별 변수 선택
  is_end <- t == 1
  ccei_1g <- if (is_end) panel_final$ccei_1g_end else panel_final$ccei_1g
  ccei_2g <- if (is_end) panel_final$ccei_2g_end else panel_final$ccei_2g
  index_hg <- if (is_end) panel_final$index_hg_end else panel_final$index_hg
  index_lg <- if (is_end) panel_final$index_lg_end else panel_final$index_lg
  high <- if (is_end) panel_final$high_end else panel_final$high
  RT_1 <- if (is_end) panel_final$RT_1_end else panel_final$RT_1
  RT_2 <- if (is_end) panel_final$RT_2_end else panel_final$RT_2
  
  # 반복문
  for (i in 1:nrow(panel_final)) {
    g_id <- panel_final$group_id[i]
    
    # 첫 번째 개인 (id.x는 항상 ccei_1g, RT_1)
    row1 <- data.frame(
      id = panel_final$id.x[i],
      group_id = g_id,
      time = t,
      CCEI_ig = ccei_1g[i],
      I_ig = if (high[i] == 1) index_hg[i] else index_lg[i],
      high_dummy = high[i],
      RT = RT_1[i],
      ccei_ind_max = if (is_end) panel_final$ccei_ind_max_end[i] else panel_final$ccei_ind_max[i],
      ccei_ind_dist = if (is_end) panel_final$ccei_ind_dist_end[i] else panel_final$ccei_ind_dist[i],
      height_gr_max = panel_final$height_gr_max[i],
      height_gr_dist = panel_final$height_gr_dist[i],
      mathscore_max = panel_final$mathscore_max[i],
      mathscore_dist = panel_final$mathscore_dist[i],
      mathscore_max_missing = panel_final$mathscore_max_missing[i],
      mathscore_dist_missing = panel_final$mathscore_dist_missing[i],
      malepair_co = panel_final$malepair_co[i],
      friendship = if (is_end) panel_final$friendship_end[i] else panel_final$friendship[i],
      class = panel_final$class[i],
      stringsAsFactors = FALSE
    )
    
    # 두 번째 개인 (partner_id.x는 ccei_2g, RT_2)
    row2 <- data.frame(
      id = panel_final$partner_id.x[i],
      group_id = g_id,
      time = t,
      CCEI_ig = ccei_2g[i],
      I_ig = if (high[i] == 1) index_lg[i] else index_hg[i],
      high_dummy = 1 - high[i],
      RT = RT_2[i],
      ccei_ind_max = if (is_end) panel_final$ccei_ind_max_end[i] else panel_final$ccei_ind_max[i],
      ccei_ind_dist = if (is_end) panel_final$ccei_ind_dist_end[i] else panel_final$ccei_ind_dist[i],
      height_gr_max = panel_final$height_gr_max[i],
      height_gr_dist = panel_final$height_gr_dist[i],
      mathscore_max = panel_final$mathscore_max[i],
      mathscore_dist = panel_final$mathscore_dist[i],
      mathscore_max_missing = panel_final$mathscore_max_missing[i],
      mathscore_dist_missing = panel_final$mathscore_dist_missing[i],
      malepair_co = panel_final$malepair_co[i],
      friendship = if (is_end) panel_final$friendship_end[i] else panel_final$friendship[i],
      class = panel_final$class[i],
      stringsAsFactors = FALSE
    )
    
    # 리스트에 추가
    individual_rows[[length(individual_rows) + 1]] <- row1
    individual_rows[[length(individual_rows) + 1]] <- row2
  }
}

# 데이터프레임으로 결합
panel_individual <- do.call(rbind, individual_rows)

panel_individual <- panel_individual %>%
  group_by(group_id, time) %>%
  mutate(
    RT_partner = RT[match(id, rev(id))],  # 두 명이므로 뒤집기
    RT_max = pmax(RT, RT_partner, na.rm = TRUE),
    RT_dist = round(abs(RT - RT_partner), 6)
  ) %>%
  ungroup()

# 필요한 열만 추출
ccei_info <- panel_final %>%
  select(group_id, id.x, partner_id.x, ccei_1, ccei_2, ccei_1_end, ccei_2_end)

# 개인 데이터에 ccei 정보 매칭
panel_individual <- panel_individual %>%
  left_join(ccei_info, by = "group_id") %>%
  mutate(
    ccei_i = case_when(
      time == 0 & id == id.x ~ ccei_1,
      time == 0 & id == partner_id.x ~ ccei_2,
      time == 1 & id == id.x ~ ccei_1_end,
      time == 1 & id == partner_id.x ~ ccei_2_end,
      TRUE ~ NA_real_
    ),
    ccei_j = case_when(
      time == 0 & id == id.x ~ ccei_2,
      time == 0 & id == partner_id.x ~ ccei_1,
      time == 1 & id == id.x ~ ccei_2_end,
      time == 1 & id == partner_id.x ~ ccei_1_end,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-ccei_1, -ccei_2, -ccei_1_end, -ccei_2_end, -id.x, -partner_id.x)  # 정리


# 확인
dim(panel_individual)  # 예상: 2628 행
table(panel_individual$time)
length(unique(panel_individual$group_id))  # 예상: 657
length(unique(panel_individual$id))        # 예상: 1314

# 저장
save(panel_individual, file = "../results/panel_individual.RData")
library(haven)
write_dta(panel_individual, "../results/panel_individual.dta")

#########################################

rm(list = ls())
load("../results/panel_final.RData")
load("../results/panel_individual.RData")

library(dplyr)
library(haven)

# mover 변수 생성
panel_individual <- panel_individual %>%
  left_join(panel_final %>% select(group_id, id.x, partner_id.x), by = "group_id") %>%
  mutate(
    mover = case_when(
      id == id.x ~ 1,
      id == partner_id.x ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-id.x, -partner_id.x)  # 불필요한 열 정리

# 저장
save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "../results/panel_individual.dta")

##############################################################

# 초기화
rm(list = ls())
load("../results/panel_final.RData")
load("../results/panel_individual.RData")

library(dplyr)
library(tidyr)
library(haven)

# 1. time에 따라 ccei_g 데이터를 준비
ccei_data <- panel_final %>%
  select(group_id, ccei_g, ccei_g_end) %>%
  distinct() %>%
  pivot_longer(cols = c(ccei_g, ccei_g_end),
               names_to = "time_label", values_to = "ccei_g") %>%
  mutate(time = ifelse(time_label == "ccei_g", 0, 1)) %>%
  select(group_id, time, ccei_g)

# 2. panel_individual에 병합
panel_individual <- panel_individual %>%
  left_join(ccei_data, by = c("group_id", "time"))

# 3. 저장
save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "../results/panel_individual.dta")


##############################################################

# 초기화
rm(list = ls())
load("../results/panel_final.RData")
load("../results/panel_individual.RData")

library(dplyr)
library(tidyr)
library(haven)

# 1. panel_final → 개인 단위 4행 데이터 생성
ind_rows <- list()

for (i in 1:nrow(panel_final)) {
  g <- panel_final[i, ]
  
  # baseline 시점
  ind_rows[[length(ind_rows) + 1]] <- data.frame(
    id = g$id.x,
    group_id = g$group_id,
    time = 0,
    high_dummy = g$high,
    new1_I_ig = if (g$high == 1) g$index_hg else g$new1_I_lg,
    new2_I_ig = if (g$high == 1) g$new2_I_hg else g$new2_I_lg
  )
  ind_rows[[length(ind_rows) + 1]] <- data.frame(
    id = g$partner_id.x,
    group_id = g$group_id,
    time = 0,
    high_dummy = 1 - g$high,
    new1_I_ig = if (g$high == 1) g$new1_I_lg else g$index_hg,
    new2_I_ig = if (g$high == 1) g$new2_I_lg else g$new2_I_hg
  )
  
  # endline 시점
  ind_rows[[length(ind_rows) + 1]] <- data.frame(
    id = g$id.x,
    group_id = g$group_id,
    time = 1,
    high_dummy = g$high_end,
    new1_I_ig = if (g$high_end == 1) g$index_hg_end else g$new1_I_lg_end,
    new2_I_ig = if (g$high_end == 1) g$new2_I_hg_end else g$new2_I_lg_end
  )
  ind_rows[[length(ind_rows) + 1]] <- data.frame(
    id = g$partner_id.x,
    group_id = g$group_id,
    time = 1,
    high_dummy = 1 - g$high_end,
    new1_I_ig = if (g$high_end == 1) g$new1_I_lg_end else g$index_hg_end,
    new2_I_ig = if (g$high_end == 1) g$new2_I_lg_end else g$new2_I_hg_end
  )
}

# 2. 개인 단위 데이터 결합
index_individual <- bind_rows(ind_rows)

# 3. panel_individual에 머지
panel_individual <- panel_individual %>%
  left_join(index_individual, by = c("id", "group_id", "time", "high_dummy"))

# 4. 저장
save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "../results/panel_individual.dta")


##############################################################
# 준비
rm(list = ls())
load("../results/panel_final.RData")
load("../results/panel_individual.RData")

# 평균 계산
mean_ccei_hg <- mean(panel_final$ccei_hg, na.rm = TRUE)
mean_ccei_lg <- mean(panel_final$ccei_lg, na.rm = TRUE)

# 출력
cat("CCEI_hg 평균:", mean_ccei_hg, "\n")
cat("CCEI_lg 평균:", mean_ccei_lg, "\n")

# 준비
rm(list = ls())
load("../results/panel_final.RData")

# 개인별 CCEI_ig 생성: high==1이면 ccei_1g, 아니면 ccei_2g
panel_final$ccei_ig <- ifelse(panel_final$high == 1, 
                              panel_final$ccei_1g, 
                              panel_final$ccei_2g)

# 평균 계산
mean_ccei_ig <- mean(panel_final$ccei_ig, na.rm = TRUE)

# 출력
cat("개인별 CCEI_ig 평균:", mean_ccei_ig, "\n")


# 조건: time == 0 (baseline 시점)
# 각 개인이 가진 값 중 high인 경우만 ccei_ig를 추출
mean_ccei_ig_group_based <- panel_individual %>%
  filter(time == 0) %>%
  group_by(group_id) %>%
  summarise(CCEI_ig = CCEI_ig[1]) %>%  # 어차피 group당 하나씩만 추출된 상태
  summarise(mean_ccei = mean(CCEI_ig, na.rm = TRUE)) %>%
  pull(mean_ccei)

# 결과 출력
cat("개인별 CCEI_ig 평균 (group-wise):", mean_ccei_ig_group_based, "\n")

##############################################

rm(list = ls())

# 패키지 불러오기
library(dplyr)
library(haven)

# 데이터 불러오기
load("../results/panel_final.RData")
load("../results/panel_individual.RData")
stata_data <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/Risk_merged.dta")

names(panel_final)

# 1. group_id 생성 (id, partner의 최대/최소 조합으로 문자열 생성)
stata_data <- stata_data %>%
  mutate(
    bigid = pmax(id, partner),
    smallid = pmin(id, partner),
    group_id = paste0(bigid, smallid)
  )

# 2. I_hg, I_lg 병합 (그룹 단위, 시간 무관)
group_level_vars <- panel_final %>%
  select(group_id, index_hg, index_lg) %>%
  rename(I_hg = index_hg, I_lg = index_lg)

stata_data <- stata_data %>%
  left_join(group_level_vars, by = "group_id")

# 3. I_ig 병합 (개인 단위, 시간은 0인 경우만)
individual_level_vars <- panel_individual %>%
  filter(time == 0) %>%
  select(id, I_ig) %>%
  mutate(id = as.numeric(id)) %>%
  distinct(id, .keep_all = TRUE)

stata_data <- stata_data %>%
  left_join(individual_level_vars, by = "id")

stata_data <- stata_data %>%
  relocate(group_id, I_ig, I_hg, I_lg, .after = id)

# 4. 저장
write_dta(stata_data, "C:/Users/hahn0/Dropbox/RP/Data/Risk_merged_with_I.dta")

################################################################

rm(list = ls())
load("../results/panel_final.RData")
load("../results/panel_individual.RData")
load("../data/endline_raw.RData")

sub_data <- endline_raw[endline_raw$id == "1110108", ]

library(dplyr)
library(haven)

ind_rows <- list()

for (i in 1:nrow(panel_final)) {
  g <- panel_final[i, ]
  
  # baseline (time = 0)
  ind_rows[[length(ind_rows) + 1]] <- data.frame(
    id         = g$id.x,
    group_id   = g$group_id,
    time       = 0,
    f_high     = g$f_high,
    
    f_ccei_i   = g$f_ccei_1,
    f_ccei_ig  = g$f_ccei_1g,
    f_ccei_j   = g$f_ccei_2,
    
    f_ccei_g   = g$f_ccei_col,
    f_ccei_hg  = g$f_ccei_hg,
    f_ccei_lg  = g$f_ccei_lg
  )
  
  ind_rows[[length(ind_rows) + 1]] <- data.frame(
    id         = g$partner_id.x,
    group_id   = g$group_id,
    time       = 0,
    f_high     = 1 - g$f_high,
    
    f_ccei_i   = g$f_ccei_2,
    f_ccei_ig  = g$f_ccei_2g,
    f_ccei_j   = g$f_ccei_1,
    
    f_ccei_g   = g$f_ccei_col,
    f_ccei_hg  = g$f_ccei_hg,
    f_ccei_lg  = g$f_ccei_lg
  )
  
  # endline (time = 1)
  ind_rows[[length(ind_rows) + 1]] <- data.frame(
    id         = g$id.x,
    group_id   = g$group_id,
    time       = 1,
    f_high     = g$f_high_end,
    
    f_ccei_i   = g$f_ccei_1_end,
    f_ccei_ig  = g$f_ccei_1g_end,
    f_ccei_j   = g$f_ccei_2_end,
    
    f_ccei_g   = g$f_ccei_col_end,
    f_ccei_hg  = g$f_ccei_hg_end,
    f_ccei_lg  = g$f_ccei_lg_end
  )
  
  ind_rows[[length(ind_rows) + 1]] <- data.frame(
    id         = g$partner_id.x,
    group_id   = g$group_id,
    time       = 1,
    f_high     = 1 - g$f_high_end,
    
    f_ccei_i   = g$f_ccei_2_end,
    f_ccei_ig  = g$f_ccei_2g_end,
    f_ccei_j   = g$f_ccei_1_end,
    
    f_ccei_g   = g$f_ccei_col_end,
    f_ccei_hg  = g$f_ccei_hg_end,
    f_ccei_lg  = g$f_ccei_lg_end
  )
}

f_ccei_individual <- bind_rows(ind_rows)

# 병합: id, group_id, time 기준으로만
panel_individual <- panel_individual %>%
  left_join(f_ccei_individual, by = c("id", "group_id", "time"))

save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "../results/panel_individual.dta")



# 허용 오차 설정
epsilon <- 1e-5

# 이론 위배 (단, 미세한 오차는 제외)
ccei_violation <- panel_individual %>%
  filter(
    !is.na(ccei_i), !is.na(f_ccei_i),
    ccei_i + epsilon < f_ccei_i  # ccei_i < f_ccei_i - epsilon 인 경우만 keep
  )

# 결과 확인
print(ccei_violation)

# 개수 확인
cat("이론 위배 사례 수 (오차 제외):", nrow(ccei_violation), "\n")

#############################################################
rm(list = ls())

# 데이터 필터링
load("../data/endline_raw.RData")
load("../data/baseline_raw.RData")

sub_data <- subset(endline_raw, id == "1110108" & game_type == "individual")
sub_data <- subset(baseline_raw, id == "1110108" & game_type == "individual")

# 상태별 가격 행렬 (2행 N열), 선택 행렬 (2행 N열)
p <- rbind(1 / sub_data$intercept_x, 1 / sub_data$intercept_y)
x <- rbind(sub_data$coord_x, sub_data$coord_y)

# 상태 확률
pi <- matrix(1/2, 2, 1)

# F-GARP 기반 CCEI 계산
source("../programs/ccei_fgarp.R")
source("../programs/fgarp.R")
source("../programs/warshall.R")

f_ccei_i <- ccei_fgarp(p, x, pi)

# 결과 출력
cat("F-GARP CCEI for ID 1110108:", f_ccei_i, "\n")
cat("Number of observations used:", nrow(sub_data), "\n")


source("../programs/ccei_garp.R")
source("../programs/garp.R")
source("../programs/warshall.R")

ccei_i <- ccei_garp(p, x)

# 결과 출력
cat("GARP CCEI for ID 1110108:", ccei_i, "\n")
cat("Number of observations used:", nrow(sub_data), "\n")


#############################################################


rm(list = ls())

# 패키지 불러오기
library(dplyr)

# 데이터 불러오기
load("../data/baseline_raw.RData")
load("../data/endline_raw.RData")


# baseline에서 고유한 id 수
n_distinct(baseline_raw$id)

# endline에서 고유한 id 수
n_distinct(endline_raw$id)

#########################################################

rm(list = ls())
load("../results/panel_individual.RData")

library(dplyr)
library(haven)

# 1. panel_final 불러오고 성별조합 더미 생성 --------------------------
panel_final <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta") %>%
  mutate(
    pairtype = case_when(
      male  == 1 & male2 == 1 ~ "MM",
      male  == 1 & male2 == 0 ~ "MF",
      male  == 0 & male2 == 1 ~ "FM",
      TRUE                    ~ "FF"
    ),
    d_MF = as.integer(pairtype == "MF"),
    d_FM = as.integer(pairtype == "FM"),
    d_FF = as.integer(pairtype == "FF")
  )

# 2. 필요한 열 추출 ----------------------------------------------------
demo <- panel_final %>%
  select(group_id, id_x, partner_id_x,    # ★ 변수명 수정
         male, male2,
         mathscore, mathscore2,
         height, height2,
         height_gr, height_gr2,
         pairtype, d_MF, d_FM, d_FF)

panel_individual <- panel_individual %>%
  left_join(demo, by = "group_id")

# 3. 개인(i) 변수 생성 -------------------------------------------------
panel_individual <- panel_individual %>%
  mutate(
    male_i       = if_else(id == id_x, male,       male2),
    mathscore_i  = if_else(id == id_x, mathscore,  mathscore2),
    height_i     = if_else(id == id_x, height,     height2),
    height_gr_i  = if_else(id == id_x, height_gr,  height_gr2)
  ) %>%
  select(-male, -male2, -mathscore, -mathscore2,  # 중간 열 제거
         -height, -height2, -height_gr, -height_gr2,
         -id_x, -partner_id_x)

# 4. 저장 --------------------------------------------------------------
save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "../results/panel_individual.dta")

##############################################################

library(dplyr)
library(haven)

load("../results/panel_individual.RData")
panel_final <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")

panel_individual <- panel_individual %>%
  arrange(group_id, time) %>%              # 같은 group 안에서 짝이 연속으로 오도록
  group_by(group_id, time) %>%             # (짝은 2명뿐)
  mutate(
    ## 1) 상대방 값(j) = 두 명이므로 뒤집기
    mathscore_j = mathscore_i[match(id, rev(id))],
    height_j    = height_i[match(id, rev(id))],
    
    ## 2) 본인이 더 높으면 1, 아니면 0
    high_math   = as.integer(mathscore_i > mathscore_j),
    high_height = as.integer(height_i    > height_j)
  ) %>%
  ungroup()

sum(is.na(panel_individual$high_math))
sum(is.na(panel_individual$high_height))

save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "../results/panel_individual.dta")

########################################################################


library(haven)
library(dplyr)
library(stringr)

# 1. 기존 분석 데이터 로드
rm(list = ls())
load("../results/panel_individual.RData")

# 3. 친구 지목 데이터 불러오기 (t == 0만 남기기)
network_raw <- read_dta("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/network_survey.dta")
network_t0 <- network_raw %>% filter(t == 0)
network_t1 <- network_raw %>% filter(t == 1)


# 함수 정의: 특정 사람이 특정인을 친구로 지목했는지 확인
check_friendship <- function(data, id, target) {
  any(data$id == id & data$obj_id == target)
}

# 새 변수 생성
panel_final <- panel_final %>%
  rowwise() %>%
  mutate(
    friendship_1 = as.integer(check_friendship(network_t0, id.x, partner_id.x)),
    friendship_2 = as.integer(check_friendship(network_t0, partner_id.x, id.x)),
    friendship_1_end = as.integer(check_friendship(network_t1, id.x, partner_id.x)),
    friendship_2_end = as.integer(check_friendship(network_t1, partner_id.x, id.x))
  ) %>%
  ungroup()

save(panel_final, file = "../results/panel_final.RData")

######################################################################


# 데이터 로드
rm(list = ls())
   
library(ggplot2)
library(dplyr)
library(tidyr)

load("../results/panel_final.RData")

f_ccei_long <- panel_final %>%
  select(group_id, f_ccei_1, f_ccei_2) %>%
  pivot_longer(
    cols = starts_with("f_ccei_"),
    names_to = "member",
    values_to = "f_ccei"
  ) %>%
  arrange(group_id)

f_ccei_median <- median(f_ccei_long$f_ccei, na.rm = TRUE)

panel_final <- panel_final %>%
  mutate(
    f_ccei_both_median = 
      as.integer(f_ccei_1 >= f_ccei_median) + as.integer(f_ccei_2 >= f_ccei_median)
  )

f_ccei_long_end <- panel_final %>%
  select(group_id, f_ccei_1_end, f_ccei_2_end) %>%
  pivot_longer(
    cols = starts_with("f_ccei_"),
    names_to = "member",
    values_to = "f_ccei"
  ) %>%
  arrange(group_id)

f_ccei_median_end <- median(f_ccei_long_end$f_ccei, na.rm = TRUE)

panel_final <- panel_final %>%
  mutate(
    f_ccei_both_median_end = 
      as.integer(f_ccei_1_end >= f_ccei_median_end) + as.integer(f_ccei_2_end >= f_ccei_median_end)
  )

panel_final <- panel_final %>%
  mutate(f_group_label = factor(
    f_ccei_both_median,
    levels = c(0, 1, 2),
    labels = c("(Low, Low)", "(Low, High)", "(High, High)")
  ))

panel_final <- panel_final %>%
  mutate(f_group_label_end = factor(
    f_ccei_both_median_end,
    levels = c(0, 1, 2),
    labels = c("(Low, Low)", "(Low, High)", "(High, High)")
  ))

ggplot(panel_final, aes(x = f_ccei_g, color = f_group_label, linetype = f_group_label)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Individual Rationality",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0.1, 1.0), breaks = seq(0.2, 1.0, by = 0.2)) +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.text = element_text(size = 12),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggplot(panel_final, aes(x = f_ccei_g_end, color = f_group_label_end, linetype = f_group_label_end)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Individual Rationality",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0.1, 1.0), breaks = seq(0.2, 1.0, by = 0.2)) +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1.5, "lines"),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA)
  )
