rm(list = ls())

load("../results/panel_final.RData")

names(panel_final)


library(dplyr)

# Step 1: 개별 id 수준 데이터프레임 생성 (2명씩 분해)
long_df <- panel_final %>%
  select(id_1 = id.x, id_2 = partner_id.x,
         ccei_1, ccei_2, ccei_1_end, ccei_2_end) %>%
  rowwise() %>%
  mutate(
    # Baseline 판단
    high_baseline_1 = case_when(
      ccei_1 > ccei_2 ~ 1,
      ccei_1 < ccei_2 ~ 0,
      ccei_1 == ccei_2 & ccei_1_end > ccei_2_end ~ 1,
      ccei_1 == ccei_2 & ccei_1_end < ccei_2_end ~ 0,
      TRUE ~ 1
    ),
    high_baseline_2 = 1 - high_baseline_1,  # 상대방은 반대로
    
    # Endline 판단
    high_endline_1 = case_when(
      ccei_1_end > ccei_2_end ~ 1,
      ccei_1_end < ccei_2_end ~ 0,
      ccei_1_end == ccei_2_end & ccei_1 > ccei_2 ~ 1,
      ccei_1_end == ccei_2_end & ccei_1 < ccei_2 ~ 0,
      TRUE ~ 1
    ),
    high_endline_2 = 1 - high_endline_1
  ) %>%
  ungroup() %>%
  # 긴 포맷으로 풀기
  select(id_1, high_baseline_1, high_endline_1,
         id_2, high_baseline_2, high_endline_2) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c(".value", "person"),
    names_pattern = "(.*)_(1|2)"
  ) %>%
  select(id, high_baseline, high_endline)

# 빈도표
freq_table <- table(long_df$high_baseline, long_df$high_endline)

# 비율표 (전체 기준)
prop_table <- round(prop.table(freq_table), 3) * 100

# 출력
cat("▶ 빈도표 (Freq):\n")
print(freq_table)

cat("\n▶ 비율표 (% of total):\n")
print(prop_table)



##########################################

panel_final <- panel_final %>%
  mutate(
    high_v2_baseline = if_else(index_hg >= index_lg, 1, 0),
    high_v2_endline  = if_else(index_hg_end >= index_lg_end, 1, 0)
  )
table(panel_final$high_v2_baseline, panel_final$high_v2_endline)

total_n <- 562
freq_table <- table(panel_final$high_v2_baseline, panel_final$high_v2_endline)
prop_table <- round(freq_table / total_n, 3)  # 소수점 3자리까지
prop_table


panel_final <- panel_final %>%
  mutate(
    test_base = case_when(
      index_hg > index_lg ~ 2,
      index_hg == index_lg ~ 1,
      index_hg < index_lg ~ 0
    ),
    test_end = case_when(
      index_hg_end > index_lg_end ~ 2,
      index_hg_end == index_lg_end ~ 1,
      index_hg_end < index_lg_end ~ 0
    )
  )
table(panel_final$test_base, panel_final$test_end)

#######################################################

# 환경 초기화
rm(list = ls())

# 데이터 불러오기
load("../results/panel_final.RData")

library(dplyr)

long_df <- panel_final %>%
  select(id_1 = id.x, id_2 = partner_id.x,
         RT_1, RT_2, RT_g, RT_1_end, RT_2_end, RT_g_end) %>%
  rowwise() %>%
  mutate(
    # baseline 거리 비교 (절댓값 기준)
    bigdist_base_1 = case_when(
      abs(RT_1 - RT_g) > abs(RT_2 - RT_g) ~ 1,
      abs(RT_1 - RT_g) < abs(RT_2 - RT_g) ~ 0,
      TRUE ~ 1  # tie이면 둘 다 1
    ),
    bigdist_base_2 = case_when(
      abs(RT_2 - RT_g) > abs(RT_1 - RT_g) ~ 1,
      abs(RT_2 - RT_g) < abs(RT_1 - RT_g) ~ 0,
      TRUE ~ 1
    ),
    
    # endline 거리 비교
    bigdist_end_1 = case_when(
      abs(RT_1_end - RT_g_end) > abs(RT_2_end - RT_g_end) ~ 1,
      abs(RT_1_end - RT_g_end) < abs(RT_2_end - RT_g_end) ~ 0,
      TRUE ~ 1
    ),
    bigdist_end_2 = case_when(
      abs(RT_2_end - RT_g_end) > abs(RT_1_end - RT_g_end) ~ 1,
      abs(RT_2_end - RT_g_end) < abs(RT_1_end - RT_g_end) ~ 0,
      TRUE ~ 1
    )
  ) %>%
  ungroup() %>%
  select(id_1, bigdist_base_1, bigdist_end_1,
         id_2, bigdist_base_2, bigdist_end_2) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c(".value", "person"),
    names_pattern = "(.*)_(1|2)"
  ) %>%
  select(id, bigdist_baseline = bigdist_base, bigdist_endline = bigdist_end)


freq_table <- table(long_df$bigdist_baseline, long_df$bigdist_endline)
prop_table <- round(prop.table(freq_table), 3) * 100

cat("▶ 빈도표 (Freq):\n")
print(freq_table)

cat("\n▶ 비율표 (% of total):\n")
print(prop_table)




####################################################

# 4-1 번 (초기 시도)

rm(list = ls())

# 데이터 불러오기
load("../results/panel_final.RData")

library(dplyr)
library(tidyr)

# Step 1: 개인 수준 long_df 생성
long_df <- panel_final %>%
  select(group_id, id_1 = id.x, id_2 = partner_id.x,
         ccei_1, ccei_2, ccei_1_end, ccei_2_end,
         index_hg, index_lg, index_hg_end, index_lg_end) %>%
  rowwise() %>%
  mutate(
    # Baseline 기준 고합리자 판단
    high_base_1 = case_when(
      ccei_1 > ccei_2 ~ 1,
      ccei_1 < ccei_2 ~ 0,
      ccei_1 == ccei_2 & ccei_1_end > ccei_2_end ~ 1,
      ccei_1 == ccei_2 & ccei_1_end < ccei_2_end ~ 0,
      TRUE ~ 1
    ),
    high_base_2 = 1 - high_base_1,
    
    # 그룹 지표 복사해서 각 개인에게 붙여주기
    index_hg_1 = index_hg,
    index_lg_1 = index_lg,
    index_hg_end_1 = index_hg_end,
    index_lg_end_1 = index_lg_end,
    
    index_hg_2 = index_hg,
    index_lg_2 = index_lg,
    index_hg_end_2 = index_hg_end,
    index_lg_end_2 = index_lg_end
  ) %>%
  ungroup() %>%
  # 긴 포맷으로 변환
  select(group_id,
         id_1, high_base_1, index_hg_1, index_lg_1, index_hg_end_1, index_lg_end_1,
         id_2, high_base_2, index_hg_2, index_lg_2, index_hg_end_2, index_lg_end_2) %>%
  pivot_longer(
    cols = -group_id,
    names_to = c(".value", "person"),
    names_pattern = "(.*)_(1|2)"
  ) %>%
  rename(id = id, high_baseline = high_base) %>%
  mutate(
    index_less_hg_baseline = if_else(index_hg <= index_lg, 1, 0),
    index_less_hg_endline  = if_else(index_hg_end <= index_lg_end, 1, 0)
  )


# baseline 기준 관여도 비교
cat("▶ table(high_baseline, index_less_hg_baseline)\n")
tbl1 <- table(long_df$high_baseline, long_df$index_less_hg_baseline)
print(tbl1)
print(round(prop.table(tbl1), 3) * 100)




# 4-2 번

rm(list = ls())

load("../results/panel_final.RData")

cols <- c("ccei_g","ccei_lg" , "ccei_1g", "ccei_2g" ,"ccei_hg", "ccei_hlg",
          "ccei_g_end", "ccei_hlg_end",
          "ccei_1g_end", "ccei_2g_end")

panel_final[cols] <- lapply(panel_final[cols], function(x) as.numeric(as.character(x)))

# Step 1: index_h0g (baseline)
panel_final$index_h0g <- with(panel_final, 
                              (ccei_g - ccei_hg) / (ccei_g - ccei_hlg)
)

panel_final$index_l0g <- with(panel_final,
                              (ccei_g - ccei_lg) / (ccei_g - ccei_hlg)
)

# Step 2
panel_final$ccei_h0g_end <- ifelse(panel_final$high == 1,
                                   panel_final$ccei_1g_end,
                                   panel_final$ccei_2g_end)

panel_final$index_h0g_end <- with(panel_final, 
                                  (ccei_g_end - ccei_h0g_end) / (ccei_g_end - ccei_hlg_end)
)

panel_final$ccei_l0g_end <- ifelse(panel_final$high == 1,
                                   panel_final$ccei_2g_end,
                                   panel_final$ccei_1g_end)

panel_final$index_l0g_end <- with(panel_final, 
                                  (ccei_g_end - ccei_l0g_end) / (ccei_g_end - ccei_hlg_end)
)


library(dplyr)

# Step 1: 개인 단위 long 데이터 프레임 구성
long_df <- panel_final %>%
  transmute(
    id_1 = id.x,
    id_2 = partner_id.x,
    high = high,
    group_id = group_id,
    index_cond = if_else(index_h0g_end >= index_l0g_end, 1, 0)
  ) %>%
  rowwise() %>%
  mutate(
    high_1 = high,
    high_2 = 1 - high
  ) %>%
  ungroup() %>%
  select(group_id, id_1, high_1, id_2, high_2, index_cond) %>%
  tidyr::pivot_longer(
    cols = c(id_1, id_2, high_1, high_2),
    names_to = c(".value", "person"),
    names_pattern = "(.*)_(1|2)"
  ) %>%
  rename(id = id, high_baseline = high) %>%
  mutate(index_cond = index_cond)  # 그룹단위 값 복사

# Step 2: 테이블 출력
tbl <- table(long_df$high_baseline, long_df$index_cond)
print(tbl)
print(round(prop.table(tbl), 3) * 100)

##############################################################################
# (1) 히스토그램 그리는 재사용 함수  ─ var: 열 이름 문자열
##############################################################################
plot_index_dist <- function(data, var, main_lab) {
  df <- data %>% dplyr::filter(!is.na(.data[[var]]))
  
  # ── 0.05 bin 히스토그램(세부라벨) ──────────────────────────
  h <- hist(df[[var]], breaks = seq(0, 1, by = 0.05), plot = FALSE)
  mid_detail   <- h$mids
  count_detail <- h$counts
  prop_detail  <- round(count_detail / sum(count_detail) * 100, 1)
  
  # ── 0.2 bin 라벨(상단) ────────────────────────────────────
  df$bin_group <- cut(df[[var]], breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
  group_props <- df %>%
    dplyr::group_by(bin_group) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    dplyr::mutate(
      prop = round(n / sum(n) * 100, 1),
      mid  = (as.numeric(bin_group) - 0.5) * 0.2
    )
  
  # ── 요약 통계 & y축 상한 ──────────────────────────────────
  x_mean   <- mean(df[[var]])
  x_median <- median(df[[var]])
  y_max    <- ceiling(max(count_detail) / 50) * 50   # 50 단위 올림
  
  # ── 시각화 ────────────────────────────────────────────────
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]])) +
    ggplot2::geom_histogram(breaks = seq(0, 1, by = 0.05),
                            fill = "gray70", color = "black") +
    
    ggplot2::geom_text(
      data = data.frame(x = mid_detail,
                        y = count_detail / 2,
                        label = ifelse(prop_detail >= 10,
                                       paste0(prop_detail, "%"), "")),
      ggplot2::aes(x = x, y = y, label = label),
      size = 4, fontface = "bold"
    ) +
    
    ggplot2::geom_text(
      data = group_props,
      ggplot2::aes(x = mid, y = y_max * 0.95,
                   label = paste0(prop, "%")),
      size = 4, fontface = "bold"
    ) +
    
    ggplot2::geom_vline(xintercept = x_mean,   color = "red",  linetype = "dashed") +
    ggplot2::geom_vline(xintercept = x_median, color = "blue", linetype = "dashed") +
    
    ggplot2::annotate("text", x = x_mean,   y = y_max * 0.50,
                      label = paste0("mean = ", round(x_mean, 3)),
                      color = "red",  size = 4, hjust = 0.5) +
    ggplot2::annotate("text", x = x_median, y = y_max * 0.35,
                      label = paste0("median = ", round(x_median, 3)),
                      color = "blue", size = 4, hjust = 0.5) +
    
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.2),
                                limits = c(0, 1), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, y_max),
                                breaks = seq(0, y_max, by = 50),
                                expand = c(0, 0)) +
    ggplot2::labs(title = main_lab, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      plot.margin      = ggplot2::margin(10, 20, 10, 10),
      panel.grid.minor = ggplot2::element_blank()
    )
}


# ① I_h0g_end (high  기준, endline)
plot_index_dist(
  panel_final,
  "index_hg_end",
  expression(paste(italic("Distribution of "), I[h[0]*","*g], " (endline)"))
)

# ② I_l0g_end (low 기준, endline)
plot_index_dist(
  panel_final,
  "index_lg_end",
  expression(paste(italic("Distribution of "), I[l[0]*","*g], " (endline)"))
)



####################################################

rm(list = ls())
load("../results/panel_final.RData")

library(dplyr)
library(tidyr)

##############################################################################
# 1. RT 기반 거리 비교 변수(high_dist) 생성 ― panel_final 자체에 추가
##############################################################################
panel_final <- panel_final %>% 
  mutate(
    # RT_1이 그룹 RT_g에서 더 멀면 1, 그렇지 않으면 0 (동점도 1)
    high_dist = case_when(
      abs(RT_1 - RT_g) > abs(RT_2 - RT_g) ~ 1,
      abs(RT_1 - RT_g) < abs(RT_2 - RT_g) ~ 0,
      TRUE                                ~ 1
    )
  )

##############################################################################
# 2. RT-기반(high_dist) High/Low에 따라 CCEI와 Index 재계산
#    └ baseline · endline 모두 계산, 변수명에 _RTbased 명시
##############################################################################
panel_final <- panel_final %>% 
  mutate(
    ## ─────────── baseline ───────────
    ccei_h0g_RTbased  = if_else(high_dist == 1, ccei_1g,    ccei_2g),
    ccei_l0g_RTbased  = if_else(high_dist == 1, ccei_2g,    ccei_1g),
    
    index_h0g_RTbased = (ccei_g     - ccei_h0g_RTbased) / (ccei_g     - ccei_hlg),
    index_l0g_RTbased = (ccei_g     - ccei_l0g_RTbased) / (ccei_g     - ccei_hlg),
    
    ## ─────────── endline ───────────
    ccei_h0g_RTbased_endline  = if_else(high_dist == 1, ccei_1g_end, ccei_2g_end),
    ccei_l0g_RTbased_endline  = if_else(high_dist == 1, ccei_2g_end, ccei_1g_end),
    
    index_h0g_RTbased_endline = (ccei_g_end - ccei_h0g_RTbased_endline) / 
      (ccei_g_end - ccei_hlg_end),
    index_l0g_RTbased_endline = (ccei_g_end - ccei_l0g_RTbased_endline) / 
      (ccei_g_end - ccei_hlg_end)
  )

##############################################################################
# long_df 재작성
##############################################################################

##############################################################################
# ① 두 사람 거리 차이로 개인별 high 플래그 만든 뒤               (같음=둘 다 1)
# ② id_1 / id_2 를 long 형으로 펼치면서 high_p1 / high_p2 매칭
# ③ 최종 person-level long_df 확정
##############################################################################

long_df <- panel_final %>% 
  # ── ① 거리·high 플래그 ──
  mutate(
    dist_1 = abs(RT_1 - RT_g),
    dist_2 = abs(RT_2 - RT_g),
    
    high_p1 = case_when(dist_1 > dist_2 ~ 1,
                        dist_1 < dist_2 ~ 0,
                        TRUE            ~ 1),
    high_p2 = case_when(dist_2 > dist_1 ~ 1,
                        dist_2 < dist_1 ~ 0,
                        TRUE            ~ 1)
  ) %>% 
  
  # ── id 열 이름을 먼저 바꾼 뒤 ──
  rename(id_1 = id.x, id_2 = partner_id.x) %>% 
  
  # ── ② long 형 변환 ──
  pivot_longer(
    cols      = c(id_1, id_2),
    names_to  = "which_person",
    values_to = "id"
  ) %>% 
  
  mutate(
    high_dist = if_else(which_person == "id_1", high_p1, high_p2)
  ) %>% 
  
  # ── ③ 필요한 열만 남기고 정렬 ──
  select(
    id, group_id,
    high_dist,
    index_h0g_RTbased,         index_l0g_RTbased,
    index_h0g_RTbased_endline, index_l0g_RTbased_endline
  ) %>% 
  relocate(id, .before = group_id)   # id를 첫 번째 열로


##############################################################################
# 1. baseline index_cond ─ 테이블 & 비율표
##############################################################################

library(dplyr)
library(ggplot2)

long_df <- long_df %>% 
  mutate(
    index_cond = case_when(
      is.na(index_h0g_RTbased) | is.na(index_l0g_RTbased) ~ NA_integer_,
      index_h0g_RTbased >= index_l0g_RTbased              ~ 1L,
      TRUE                                                ~ 0L
    )
  )

cat("▶ baseline | table(high_dist, index_cond)\n")
tbl_base <- table(long_df$high_dist, long_df$index_cond, useNA = "ifany")
print(tbl_base)
cat("\n 전체 대비(%)\n")
print(round(prop.table(tbl_base), 3) * 100)


##############################################################################
# 2. endline index_cond_end ─ 테이블 & 비율표
##############################################################################
long_df <- long_df %>% 
  mutate(
    index_cond_end = case_when(
      is.na(index_h0g_RTbased_endline) |
        is.na(index_l0g_RTbased_endline)               ~ NA_integer_,
      index_h0g_RTbased_endline >= index_l0g_RTbased_endline ~ 1L,
      TRUE                                             ~ 0L
    )
  )

cat("\n▶ endline  | table(high_dist, index_cond_end)\n")
tbl_end <- table(long_df$high_dist, long_df$index_cond_end, useNA = "ifany")
print(tbl_end)
cat("\n 전체 대비(%)\n")
print(round(prop.table(tbl_end), 3) * 100)

##############################################################################
# 3·4. RT-based endline 지수 분포 히스토그램 함수화  ─ h0g, l0g 두 번 호출
##############################################################################
plot_index_dist <- function(data, var, main_lab) {
  # var: 문자열, data 안 열 이름
  df <- data %>% filter(!is.na(.data[[var]]))
  
  # 세부 히스토그램(0.05 bin)
  h <- hist(df[[var]], breaks = seq(0, 1, by = 0.05), plot = FALSE)
  mid_detail  <- h$mids
  count_detail <- h$counts
  prop_detail  <- round(count_detail / sum(count_detail) * 100, 1)
  
  # 0.2 bin 그룹
  df$bin_group <- cut(df[[var]], breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)
  group_props <- df %>%
    group_by(bin_group) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      prop = round(n / sum(n) * 100, 1),
      mid  = (as.numeric(bin_group) - 0.5) * 0.2       # 해당 bin 중앙값
    )
  
  # 통계량 & y축 상한(50 단위 올림)
  x_mean   <- mean(df[[var]])
  x_median <- median(df[[var]])
  y_max    <- ceiling(max(count_detail) / 50) * 50
  
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(breaks = seq(0, 1, by = 0.05),
                   fill = "gray70", color = "black") +
    
    # 상세 bin(0.05)에서 10% 이상만 라벨 표시
    geom_text(
      data = data.frame(x = mid_detail,
                        y = count_detail / 2,
                        label = ifelse(prop_detail >= 10, paste0(prop_detail, "%"), "")),
      aes(x = x, y = y, label = label),
      size = 4, fontface = "bold"
    ) +
    
    # 0.2 bin 라벨(그래프 상단)
    geom_text(
      data = group_props,
      aes(x = mid, y = y_max * 0.95,
          label = paste0(prop, "%")),
      size = 4, fontface = "bold"
    ) +
    
    geom_vline(xintercept = x_mean,   color = "red",  linetype = "dashed") +
    geom_vline(xintercept = x_median, color = "blue", linetype = "dashed") +
    
    annotate("text", x = x_mean,   y = y_max * 0.50,
             label = paste0("mean = ", round(x_mean, 3)),
             color = "red",  size = 4, hjust = 0.5) +
    annotate("text", x = x_median, y = y_max * 0.35,
             label = paste0("median = ", round(x_median, 3)),
             color = "blue", size = 4, hjust = 0.5) +
    
    scale_x_continuous(breaks = seq(0, 1, by = 0.2),
                       limits = c(0, 1), minor_breaks = NULL) +
    scale_y_continuous(limits = c(0, y_max),
                       breaks = seq(0, y_max, by = 50),
                       expand = c(0, 0)) +
    labs(
      title = main_lab,
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 14) +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin      = margin(10, 20, 10, 10),
      panel.grid.minor = element_blank()
    )
}

# 3) I_h0g_RTbased_endline 분포
plot_index_dist(
  panel_final, 
  "index_h0g_RTbased_endline",
  expression(paste(italic("Distribution of "), I[h[0]*","*g], " (endline, RT-based)"))
)

# 4) I_l0g_RTbased_endline 분포
plot_index_dist(
  panel_final, 
  "index_l0g_RTbased_endline",
  expression(paste(italic("Distribution of "), I[l[0]*","*g], " (endline, RT-based)"))
)

####################################################################

##############################################################################
# 0. 5.31. 교수님 추가 지시사항
##############################################################################
rm(list = ls())                       
load("../results/panel_final.RData")

# 필요 패키지
library(dplyr)

# baseline
panel_final <- panel_final %>%
  mutate(
    ccei_h = if_else(high == 1, ccei_1, ccei_2),
    ccei_l = if_else(high == 1, ccei_2, ccei_1)
  )

# endline
panel_final <- panel_final %>%
  mutate(
    ccei_h_end = if_else(high_end == 1, ccei_1_end, ccei_2_end),
    ccei_l_end = if_else(high_end == 1, ccei_2_end, ccei_1_end)
  )

# 저장
save(panel_final, file = "../results/panel_final.RData")

#########################################################################

rm(list = ls())
load("../results/panel_final.RData")

library(dplyr)
library(tidyr)
library(ggplot2)

panel_final <- panel_final %>%
  rename(
    ccei_h_baseline = ccei_h,
    ccei_l_baseline = ccei_l,
    ccei_g_baseline = ccei_g,
    ccei_h_endline  = ccei_h_end,
    ccei_l_endline  = ccei_l_end,
    ccei_g_endline  = ccei_g_end
  )

ccei_long <- panel_final %>%
  select(
    ccei_h_baseline, ccei_l_baseline, ccei_g_baseline,
    ccei_h_endline,  ccei_l_endline,  ccei_g_endline
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("type", "time"),
    names_pattern = "ccei_(h|l|g)_(baseline|endline)",
    values_to = "ccei"
  ) %>%
  mutate(
    type = recode(type,
                  h = "High CCEI",
                  l = "Low CCEI",
                  g = "Group CCEI"),
    type = factor(type, levels = c("Low CCEI", "Group CCEI", "High CCEI")),
    time = factor(time, levels = c("baseline", "endline"))
  )

plot_ccei_hist <- function(df, time_lab) {
  ggplot(df, aes(x = ccei, fill = type, colour = type)) +
    geom_histogram(breaks = seq(0, 1, 0.05),
                   position = "identity",
                   alpha = 0.5,
                   linewidth = 0.4,
                   na.rm = TRUE) +
    scale_fill_manual(values = c("#E41A1C", "#FFD92F", "#4DAF4A")) +  # 빨,노,초
    scale_colour_manual(values = c("#B2182B", "#B8860B", "#006D2C")) +
    scale_x_continuous(
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 600),
      breaks = seq(0, 600, 100),
      minor_breaks = seq(0, 600, 50),
      expand = c(0, 0)
    ) +
    labs(title = paste("CCEI distribution (", time_lab, ")", sep = ""),
         x = "CCEI (bin = 0.05)", y = "Count",
         fill = NULL, colour = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      panel.grid.minor.y = element_line(linewidth = 0.3, linetype = "dotted"),
      plot.margin = margin(10, 20, 10, 10)
    )
}


plot_ccei_hist(filter(ccei_long, time == "baseline"), "baseline")
plot_ccei_hist(filter(ccei_long, time == "endline"), "endline")

plot_cdf <- function(df, var_name, time_lab, xlab) {
  ggplot(df, aes(x = !!sym(var_name), colour = type, linetype = type)) +
    stat_ecdf(size = 1) +
    scale_colour_manual(
      values = c("Low" = "black", "Group" = "blue", "High" = "red")
    ) +
    scale_linetype_manual(
      values = c("Low" = "solid", "Group" = "dashed", "High" = "solid")
    ) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    labs(
      title = paste0("CDF of ", xlab, " (", time_lab, ")"),
      x = xlab, y = "Cumulative Probability",
      colour = NULL, linetype = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      plot.margin = margin(10, 20, 10, 10)
    )
}

ccei_cdf_long <- ccei_long %>%
  mutate(
    type = recode(type,
                  "Low CCEI"   = "Low",
                  "Group CCEI" = "Group",
                  "High CCEI"  = "High")
  )

plot_cdf(filter(ccei_cdf_long, time == "baseline"), "ccei", "baseline", "CCEI")
plot_cdf(filter(ccei_cdf_long, time == "endline"), "ccei", "endline", "CCEI")


#############################################################

rm(list = ls())
load("../results/panel_final.RData")

library(dplyr)

panel_final <- panel_final %>%
  mutate(
    # baseline
    RT_h_baseline = if_else(RT_1 >= RT_2, RT_1, RT_2),
    RT_l_baseline = if_else(RT_1 <  RT_2, RT_1, RT_2),
    RT_g_baseline = RT_g,
    
    # endline
    RT_h_endline = if_else(RT_1_end >= RT_2_end, RT_1_end, RT_2_end),
    RT_l_endline = if_else(RT_1_end <  RT_2_end, RT_1_end, RT_2_end),
    RT_g_endline = RT_g_end
  )

# 저장 (계속 덮어쓰기 가능)
save(panel_final, file = "../results/panel_final.RData")

#################################################################

rm(list = ls())
load("../results/panel_final.RData")

library(tidyr)
library(ggplot2)
library(dplyr)

rt_long <- panel_final %>%
  select(RT_h_baseline, RT_l_baseline, RT_g_baseline,
         RT_h_endline,  RT_l_endline,  RT_g_endline) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("type", "time"),
    names_pattern = "RT_(h|l|g)_(baseline|endline)",
    values_to = "rt"
  ) %>%
  mutate(
    type = recode(type,
                  h = "High RT",
                  l = "Low RT",
                  g = "Group RT"),
    type = factor(type, levels = c("Low RT", "Group RT", "High RT")),
    time = factor(time, levels = c("baseline", "endline"))
  )

plot_rt_hist <- function(df, time_lab) {
  ggplot(df, aes(x = rt, fill = type, colour = type)) +
    geom_histogram(breaks = seq(0, 1, 0.05),
                   position = "identity",
                   alpha = 0.5,
                   linewidth = 0.4,
                   na.rm = TRUE) +
    scale_fill_manual(values = c("#E41A1C", "#FFD92F", "#4DAF4A")) +
    scale_colour_manual(values = c("#B2182B", "#B8860B", "#006D2C")) +
    scale_x_continuous(
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 200),
      breaks = seq(0, 200, 100),
      minor_breaks = seq(0, 200, 50),
      expand = c(0, 0)
    ) +
    labs(title = paste("RT distribution (", time_lab, ")", sep = ""),
         x = "Risk Tolerance (RT, bin = 0.05)", y = "Count",
         fill = NULL, colour = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      panel.grid.minor.y = element_line(linewidth = 0.3, linetype = "dotted"),
      plot.margin = margin(10, 20, 10, 10)
    )
}

plot_rt_hist(filter(rt_long, time == "baseline"), "baseline")
plot_rt_hist(filter(rt_long, time == "endline"), "endline")

plot_cdf <- function(df, var_name, time_lab, xlab) {
  ggplot(df, aes(x = !!sym(var_name), colour = type, linetype = type)) +
    stat_ecdf(size = 1) +
    scale_colour_manual(
      values = c("Low" = "black", "Group" = "blue", "High" = "red")
    ) +
    scale_linetype_manual(
      values = c("Low" = "solid", "Group" = "dashed", "High" = "solid")
    ) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    labs(
      title = paste0("CDF of ", xlab, " (", time_lab, ")"),
      x = xlab, y = "Cumulative Probability",
      colour = NULL, linetype = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      plot.margin = margin(10, 20, 10, 10)
    )
}

rt_cdf_long <- rt_long %>%
  mutate(
    type = recode(type,
                  "Low RT"   = "Low",
                  "Group RT" = "Group",
                  "High RT"  = "High")
  )

plot_cdf(filter(rt_cdf_long, time == "baseline"), "rt", "baseline", "RT")
plot_cdf(filter(rt_cdf_long, time == "endline"), "rt", "endline", "RT")


##############################################

rm(list = ls())
load("../results/panel_final.RData")

panel_final <- panel_final %>%
  mutate(
    # baseline
    RT_hccei_baseline = if_else(high == 1, RT_1, RT_2),    # CCEI 높은 사람의 RT
    RT_lccei_baseline = if_else(high == 0, RT_1, RT_2),    # CCEI 낮은 사람의 RT
    RT_gccei_baseline = RT_g,
    
    # endline
    RT_hccei_endline = if_else(high_end == 1, RT_1_end, RT_2_end),
    RT_lccei_endline = if_else(high_end == 0, RT_1_end, RT_2_end),
    RT_gccei_endline = RT_g_end
  )

library(tidyr)
library(ggplot2)

# RT of High/Low CCEI + Group RT → long-format 변환
rt_ccei_long <- panel_final %>%
  select(RT_hccei_baseline, RT_lccei_baseline, RT_gccei_baseline,
         RT_hccei_endline,  RT_lccei_endline,  RT_gccei_endline) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("type", "time"),
    names_pattern = "RT_(hccei|lccei|gccei)_(baseline|endline)",
    values_to = "rt"
  ) %>%
  mutate(
    type = recode(type,
                  hccei  = "High CCEI RT",
                  lccei  = "Low CCEI RT",
                  gccei  = "Group RT"),
    type = factor(type, levels = c("Low CCEI RT", "Group RT", "High CCEI RT")),
    time = factor(time, levels = c("baseline", "endline"))
  )
plot_rt_ccei_hist <- function(df, time_lab) {
  ggplot(df, aes(x = rt, fill = type, colour = type)) +
    geom_histogram(breaks = seq(0, 1, 0.05),
                   position = "identity",
                   alpha = 0.5,
                   linewidth = 0.4,
                   na.rm = TRUE) +
    scale_fill_manual(values = c("#E41A1C", "#FFD92F", "#4DAF4A")) +   # 빨-노-초
    scale_colour_manual(values = c("#B2182B", "#B8860B", "#006D2C")) +
    scale_x_continuous(
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 200),
      breaks = seq(0, 200, 100),
      minor_breaks = seq(0, 200, 50),
      expand = c(0, 0)
    ) +
    labs(title = paste("RT distribution by CCEI level (", time_lab, ")", sep = ""),
         x = "Risk Tolerance (RT, bin = 0.05)", y = "Count",
         fill = NULL, colour = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      panel.grid.minor.y = element_line(linewidth = 0.3, linetype = "dotted"),
      plot.margin = margin(10, 20, 10, 10)
    )
}

plot_rt_ccei_hist(filter(rt_ccei_long, time == "baseline"), "baseline")
plot_rt_ccei_hist(filter(rt_ccei_long, time == "endline"), "endline")



plot_cdf <- function(df, var_name, time_lab, xlab) {
  ggplot(df, aes(x = !!sym(var_name), colour = type, linetype = type)) +
    stat_ecdf(size = 1) +
    scale_colour_manual(
      values = c("Low" = "black", "Group" = "blue", "High" = "red")
    ) +
    scale_linetype_manual(
      values = c("Low" = "solid", "Group" = "dashed", "High" = "solid")
    ) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    labs(
      title = paste0("CDF of ", xlab, " (", time_lab, ")"),
      x = xlab, y = "Cumulative Probability",
      colour = NULL, linetype = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      plot.margin = margin(10, 20, 10, 10)
    )
}

rt_ccei_cdf_long <- rt_ccei_long %>%
  mutate(
    type = recode(type,
                  "Low CCEI RT"   = "Low",
                  "Group RT"      = "Group",
                  "High CCEI RT"  = "High")
  )

plot_cdf(filter(rt_ccei_cdf_long, time == "baseline"), "rt", "baseline", "RT (by CCEI)")
plot_cdf(filter(rt_ccei_cdf_long, time == "endline"), "rt", "endline", "RT (by CCEI)")



###########################################################################

rm(list = ls())

load("../results/panel_final.RData")

panel_final$diff_base <- panel_final$ccei_hg - panel_final$ccei_lg

library(ggplot2)
library(dplyr)

df <- panel_final
df <- df[!is.na(df$diff_base), ]

# 히스토그램용 범위 지정
breaks_seq <- seq(-0.4, 1, by = 0.05)

# 히스토그램 정보 계산
hist_detail <- hist(df$diff_base, breaks = breaks_seq, plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 평균 및 중앙값
x_mean <- mean(df$diff_base)
x_median <- median(df$diff_base)
y_max <- max(count_detail)

# 시각화
ggplot(df, aes(x = diff_base)) +
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
    breaks = seq(-0.4, 1, by = 0.2),
    limits = c(-0.4, 1),
    minor_breaks = NULL
  ) +
  
  scale_y_continuous(
    limits = c(0, y_max * 1.2),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Distribution of CCEI_hg - CCEI_lg (Baseline)",
    x = "CCEI_hg - CCEI_lg",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )



library(ggplot2)

ggplot(panel_final, aes(x = diff_base)) +
  stat_ecdf(size = 1, colour = "blue", linetype = "solid") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  labs(
    title = "CDF of CCEI_hg - CCEI_lg (Baseline)",
    x = "Difference",
    y = "Cumulative Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(10, 20, 10, 10)
  )

# 빈도표 (>0, <0)
table(sign(panel_final$diff_base))
# 또는 더 명시적으로
table(panel_final$diff_base > 0)





###############################

panel_final$diff_end <- panel_final$ccei_hg_end - panel_final$ccei_lg_end

library(ggplot2)
library(dplyr)

df_end <- panel_final
df_end <- df_end[!is.na(df_end$diff_end), ]

# 히스토그램용 범위 지정
breaks_seq <- seq(-0.4, 1, by = 0.05)

# 히스토그램 정보 계산
hist_detail <- hist(df_end$diff_end, breaks = breaks_seq, plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

# 평균 및 중앙값
x_mean <- mean(df_end$diff_end)
x_median <- median(df_end$diff_end)
y_max <- max(count_detail)

# 시각화
ggplot(df_end, aes(x = diff_end)) +
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
    breaks = seq(-0.4, 1, by = 0.2),
    limits = c(-0.4, 1),
    minor_breaks = NULL
  ) +
  
  scale_y_continuous(
    limits = c(0, y_max * 1.2),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Distribution of CCEI_hg_end - CCEI_lg_end (Endline)",
    x = "CCEI_hg - CCEI_lg",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )
# 빈도표 (>0, <0)
table(sign(panel_final$diff_end))
# 또는 더 명시적으로
table(panel_final$diff_end > 0)



library(ggplot2)

ggplot(panel_final, aes(x = diff_end)) +
  stat_ecdf(size = 1, colour = "blue", linetype = "solid") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  labs(
    title = "CDF of CCEI_hg - CCEI_lg (Endline)",
    x = "Difference",
    y = "Cumulative Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(10, 20, 10, 10)
  )

# 빈도표 (>0, <0)
table(sign(panel_final$diff_end))
# 또는 더 명시적으로
table(panel_final$diff_end > 0)

#################################################

library(ggplot2)
library(ggpointdensity)
library(dplyr)

# 사분면 정보 계산
panel_final$quadrant <- with(panel_final, case_when(
  diff_base > 0 & diff_end > 0 ~ "Q1",
  diff_base < 0 & diff_end > 0 ~ "Q2",
  diff_base < 0 & diff_end < 0 ~ "Q3",
  diff_base > 0 & diff_end < 0 ~ "Q4",
  TRUE ~ "On axis"
))

# 사분면별 비율 계산
quad_table <- panel_final %>%
  filter(quadrant != "On axis") %>%
  group_by(quadrant) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n / sum(n) * 100, 1))

## 텍스트 라벨 위치 조정 (더 안쪽으로)
quad_labels <- data.frame(
  x = c(0.4, -0.25, -0.25, 0.4),
  y = c(0.4, 0.4, -0.25, -0.25),
  quadrant = c("Q1", "Q2", "Q3", "Q4")
) %>%
  left_join(quad_table, by = "quadrant") %>%
  mutate(label = paste0(quadrant, ": ", prop, "%"))

# 수정된 scatter plot
ggplot(panel_final, aes(x = diff_base, y = diff_end)) +
  geom_pointdensity(adjust = 0.5) +
  scale_color_viridis_c(option = "plasma") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = quad_labels, aes(x = x, y = y, label = label), size = 4.2) +
  labs(
    title = "Density Scatter Plot of (Baseline vs Endline)\nCCEI_hg - CCEI_lg",
    x = "Baseline CCEI_hg - CCEI_lg",
    y = "Endline CCEI_hg - CCEI_lg",
    color = "Density"
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +  # 여전히 클립 해제
  theme(
    plot.margin = margin(10, 20, 10, 10),
    legend.position = "right"
  )

##################################

library(plotly)
library(dplyr)
library(tidyr)

# 결측 제거
df <- panel_final %>% filter(!is.na(diff_base), !is.na(diff_end))

# 2D binning
bin_size <- 0.05
x_breaks <- seq(-0.4, 1, by = bin_size)
y_breaks <- seq(-0.4, 1, by = bin_size)

df$bin_x <- cut(df$diff_base, breaks = x_breaks)
df$bin_y <- cut(df$diff_end, breaks = y_breaks)

# 빈도 계산 및 중심좌표 추출
df_bins <- df %>%
  count(bin_x, bin_y) %>%
  mutate(
    x = as.numeric(sub("\\((.+),.*", "\\1", bin_x)) + bin_size / 2,
    y = as.numeric(sub("\\((.+),.*", "\\1", bin_y)) + bin_size / 2
  )

# z 행렬로 변환 (y 축 기준 정렬 필요)
z_matrix <- df_bins %>%
  select(x, y, n) %>%
  pivot_wider(names_from = x, values_from = n, values_fill = 0) %>%
  arrange(desc(y))  # y축이 위에서 아래로 정렬되게

# 추출
z_values <- as.matrix(z_matrix[,-1])
x_vals <- sort(unique(df_bins$x))
y_vals <- sort(unique(df_bins$y), decreasing = TRUE)

# 실제 카운트로 3D surface plot 생성
plot_ly(
  x = ~x_vals,
  y = ~y_vals,
  z = ~z_values,
  type = "surface",
  colorscale = "Viridis",
  showscale = TRUE,
  colorbar = list(title = "Count", x = 1.05)
) %>%
  layout(
    title = "3D Surface Plot of CCEI_hg - CCEI_lg Differences",
    scene = list(
      xaxis = list(title = "Baseline Difference (diff_base)"),
      yaxis = list(title = "Endline Difference (diff_end)"),
      zaxis = list(title = "Count")
    ),
    margin = list(l = 0, r = 0, b = 0, t = 50)
  )

#########################################

panel_final$diff_end_h0 <- panel_final$ccei_h0g_end - panel_final$ccei_l0g_end

library(ggplot2)
library(dplyr)

df_end <- panel_final
df_end <- df_end[!is.na(df_end$diff_end_h0), ]

# 히스토그램용 범위
breaks_seq <- seq(-0.5, 1, by = 0.05)

# 계산
hist_detail <- hist(df_end$diff_end_h0, breaks = breaks_seq, plot = FALSE)
mid_detail <- hist_detail$mids
count_detail <- hist_detail$counts
prop_detail <- round(count_detail / sum(count_detail) * 100, 1)

x_mean <- mean(df_end$diff_end_h0)
x_median <- median(df_end$diff_end_h0)
y_max <- max(count_detail)

# 시각화
ggplot(df_end, aes(x = diff_end_h0)) +
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
    breaks = seq(-0.4, 1, by = 0.2),
    limits = c(-0.5, 1),
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    limits = c(0, y_max * 1.2),
    expand = c(0, 0)
  ) +
  labs(
    title = "Distribution of CCEI_h0g - CCEI_l0g (Endline)",
    x = "CCEI_h0g_end - CCEI_l0g_end",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    panel.grid.minor.x = element_blank()
  )

# 빈도표 (>0, <0)
table(sign(panel_final$diff_end_h0))
# 또는 더 명시적으로
table(panel_final$diff_end_h0 > 0)



ggplot(panel_final, aes(x = diff_end_h0)) +
  stat_ecdf(size = 1, colour = "blue", linetype = "solid") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  labs(
    title = "CDF of CCEI_h0g - CCEI_l0g (Endline)",
    x = "CCEI_h0g_end - CCEI_l0g_end",
    y = "Cumulative Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(10, 20, 10, 10)
  )

############################


library(ggplot2)
library(ggpointdensity)
library(dplyr)

# 사분면 정보 계산 (baseline 기준은 그대로, endline은 고정된 h/l 차이)
panel_final$quadrant <- with(panel_final, case_when(
  diff_base > 0 & diff_end_h0 > 0 ~ "Q1",
  diff_base < 0 & diff_end_h0 > 0 ~ "Q2",
  diff_base < 0 & diff_end_h0 < 0 ~ "Q3",
  diff_base > 0 & diff_end_h0 < 0 ~ "Q4",
  TRUE ~ "On axis"
))

# 사분면별 비율 계산
quad_table <- panel_final %>%
  filter(quadrant != "On axis") %>%
  group_by(quadrant) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n / sum(n) * 100, 1))

# 라벨 위치 조정
quad_labels <- data.frame(
  x = c(0.4, -0.25, -0.25, 0.4),
  y = c(0.4, 0.4, -0.25, -0.25),
  quadrant = c("Q1", "Q2", "Q3", "Q4")
) %>%
  left_join(quad_table, by = "quadrant") %>%
  mutate(label = paste0(quadrant, ": ", prop, "%"))

# Scatter Plot
ggplot(panel_final, aes(x = diff_base, y = diff_end_h0)) +
  geom_pointdensity(adjust = 0.5) +
  scale_color_viridis_c(option = "plasma") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(data = quad_labels, aes(x = x, y = y, label = label), size = 4.2) +
  labs(
    title = "Density Scatter Plot of (Fixed h/l from Baseline)\n CCEI_h0g - CCEI_l0g (Baseline vs Endline)",
    x = "Baseline CCEI_hg - CCEI_lg",
    y = "Endline CCEI_h0g - CCEI_l0g",
    color = "Density"
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 20, 10, 10),
    legend.position = "right"
  )




library(plotly)
library(dplyr)
library(tidyr)

# 결측 제거
df <- panel_final %>%
  filter(!is.na(diff_base), !is.na(diff_end_h0))

# 2D binning
bin_size <- 0.05
x_breaks <- seq(-0.4, 1, by = bin_size)
y_breaks <- seq(-0.4, 1, by = bin_size)

df$bin_x <- cut(df$diff_base, breaks = x_breaks)
df$bin_y <- cut(df$diff_end_h0, breaks = y_breaks)

# 빈도 계산 및 중심 좌표
df_bins <- df %>%
  count(bin_x, bin_y) %>%
  mutate(
    x = as.numeric(sub("\\((.+),.*", "\\1", bin_x)) + bin_size / 2,
    y = as.numeric(sub("\\((.+),.*", "\\1", bin_y)) + bin_size / 2
  )

# z 행렬로 변환
z_matrix <- df_bins %>%
  select(x, y, n) %>%
  pivot_wider(names_from = x, values_from = n, values_fill = 0) %>%
  arrange(desc(y))

z_values <- as.matrix(z_matrix[,-1])
x_vals <- sort(unique(df_bins$x))
y_vals <- sort(unique(df_bins$y), decreasing = TRUE)

# 3D Surface Plot
plot_ly(
  x = ~x_vals,
  y = ~y_vals,
  z = ~z_values,
  type = "surface",
  colorscale = "Viridis",
  showscale = TRUE,
  colorbar = list(title = "Count", x = 1.05)
) %>%
  layout(
    title = "3D Surface Plot (Fixed h/l from Baseline)",
    scene = list(
      xaxis = list(title = "Baseline CCEI_hg - CCEI_lg"),
      yaxis = list(title = "Endline CCEI_h0g - CCEI_l0g"),
      zaxis = list(title = "Count")
    ),
    margin = list(l = 0, r = 0, b = 0, t = 50)
  )

##########################################
# 준비
rm(list = ls())
load("../results/panel_final.RData")
load("../results/panel_pbl.RData")


library(ggplot2)
library(dplyr)
library(tidyr)

# 정렬 기준 만들기
panel_final_sorted <- panel_final %>%
  arrange(index_hg, index_lg) %>%
  mutate(group_order = row_number())  # 시각화용 x축 인덱스

# long 형식으로 변환
df_long <- panel_final_sorted %>%
  select(group_order, group_id, index_hg, index_lg) %>%
  pivot_longer(cols = c(index_hg, index_lg),
               names_to = "type", values_to = "value")

# 시각화
ggplot(df_long, aes(x = group_order, y = value, color = type)) +
  geom_point(size = 1.2, alpha = 0.8) +
  labs(title = "Group-wise I_hg and I_lg (Baseline)",
       x = "Group (sorted by I_hg, then I_lg)", y = "Index Value",
       color = "Index Type") +
  scale_color_manual(values = c("index_hg" = "red", "index_lg" = "blue"),
                     labels = c("I_hg", "I_lg")) +
  theme_minimal(base_size = 14)



library(ggplot2)
library(dplyr)
library(tidyr)

# 정렬 기준 만들기 (endline 기준)
panel_final_sorted_end <- panel_final %>%
  arrange(index_hg_end, index_lg_end) %>%
  mutate(group_order = row_number())

# long 형식으로 변환
df_long_end <- panel_final_sorted_end %>%
  select(group_order, group_id, index_hg_end, index_lg_end) %>%
  pivot_longer(cols = c(index_hg_end, index_lg_end),
               names_to = "type", values_to = "value")

# 시각화
ggplot(df_long_end, aes(x = group_order, y = value, color = type)) +
  geom_point(size = 1.2, alpha = 0.8) +
  labs(title = "Group-wise I_hg and I_lg (Endline)",
       x = "Group (sorted by I_hg_end, then I_lg_end)", y = "Index Value",
       color = "Index Type") +
  scale_color_manual(values = c("index_hg_end" = "red", "index_lg_end" = "blue"),
                     labels = c("I_hg", "I_lg")) +
  theme_minimal(base_size = 14)


library(ggplot2)
library(dplyr)
library(tidyr)

# 정렬 기준 만들기 (CCEI 기준)
panel_final_sorted_ccei <- panel_final %>%
  arrange(desc(ccei_1g), desc(ccei_2g)) %>%
  mutate(group_order = row_number())

# long 형식 변환
df_ccei_long <- panel_final_sorted_ccei %>%
  select(group_order, group_id, ccei_1g, ccei_2g) %>%
  pivot_longer(cols = c(ccei_1g, ccei_2g),
               names_to = "type", values_to = "value")

# 시각화
ggplot(df_ccei_long, aes(x = group_order, y = value, color = type)) +
  geom_point(size = 1.2, alpha = 0.8) +
  labs(title = "Group-wise CCEI_hg and CCEI_lg (Baseline)",
       x = "Group (sorted by CCEI_hg, then CCEI_lg)", y = "CCEI Value",
       color = "Index Type") +
  scale_color_manual(values = c("ccei_1g" = "red", "ccei_2g" = "blue"),
                     labels = c("CCEI_hg", "CCEI_lg")) +
  theme_minimal(base_size = 14)



# 정렬 기준 만들기 (Endline CCEI 기준)
panel_final_sorted_ccei_end <- panel_final %>%
  arrange(desc(ccei_1g_end), desc(ccei_2g_end)) %>%
  mutate(group_order = row_number())

# long 형식 변환
df_ccei_long_end <- panel_final_sorted_ccei_end %>%
  select(group_order, group_id, ccei_1g_end, ccei_2g_end) %>%
  pivot_longer(cols = c(ccei_1g_end, ccei_2g_end),
               names_to = "type", values_to = "value")

# 시각화
ggplot(df_ccei_long_end, aes(x = group_order, y = value, color = type)) +
  geom_point(size = 1.2, alpha = 0.8) +
  labs(title = "Group-wise CCEI_hg and CCEI_lg (Endline)",
       x = "Group (sorted by CCEI_hg_end, then CCEI_lg_end)", y = "CCEI Value",
       color = "Index Type") +
  scale_color_manual(values = c("ccei_1g_end" = "red", "ccei_2g_end" = "blue"),
                     labels = c("CCEI_hg", "CCEI_lg")) +
  theme_minimal(base_size = 14)

############################



library(dplyr)
library(tidyr)
library(ggplot2)

# 1. 그룹 나누기
group_a <- panel_final %>%
  filter(index_hg < index_lg) %>%
  arrange(index_hg, index_lg) %>%
  mutate(group_order = row_number())

group_b <- panel_final %>%
  filter(index_hg >= index_lg) %>%
  arrange(index_hg, index_lg) %>%
  mutate(group_order = row_number() + nrow(group_a))  # 이어붙이기

# 2. 합치기
panel_sorted <- bind_rows(group_a, group_b)

# 3. long 형식으로 변환
df_long <- panel_sorted %>%
  select(group_order, group_id, index_hg, index_lg) %>%
  pivot_longer(cols = c(index_hg, index_lg),
               names_to = "type", values_to = "value")

# 4. 시각화
ggplot(df_long, aes(x = group_order, y = value, color = type)) +
  geom_point(size = 1.2, alpha = 0.8) +
  labs(
    title = "I_hg and I_lg Across Groups (Baseline)\nSplit by I_hg < I_lg vs. I_hg ≥ I_lg, Then Sorted by I_hg and I_lg"
,
    x = "Group (segmented sort)",
    y = "Index Value",
    color = "Index Type"
  ) +
  scale_color_manual(
    values = c("index_hg" = "red", "index_lg" = "blue"),
    labels = c("I_hg", "I_lg")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 15, lineheight = 1.1)
  )



group_a <- panel_final %>%
  filter(index_hg_end < index_lg_end) %>%
  arrange(index_hg_end, index_lg_end) %>%
  mutate(group_order = row_number())

group_b <- panel_final %>%
  filter(index_hg_end >= index_lg_end) %>%
  arrange(index_hg_end, index_lg_end) %>%
  mutate(group_order = row_number() + nrow(group_a))

panel_sorted <- bind_rows(group_a, group_b)

df_long <- panel_sorted %>%
  select(group_order, group_id, index_hg_end, index_lg_end) %>%
  pivot_longer(cols = c(index_hg_end, index_lg_end),
               names_to = "type", values_to = "value")

ggplot(df_long, aes(x = group_order, y = value, color = type)) +
  geom_point(size = 1.2, alpha = 0.8) +
  labs(
    title = "I_hg and I_lg Across Groups (Endline)\nSplit by I_hg < I_lg vs. I_hg ≥ I_lg, Then Sorted by I_hg and I_lg",
    x = "Group (segmented sort)",
    y = "Index Value",
    color = "Index Type"
  ) +
  scale_color_manual(values = c("index_hg_end" = "red", "index_lg_end" = "blue"),
                     labels = c("I_hg", "I_lg")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 15, lineheight = 1.1))



# 기준: ccei_1g vs. ccei_2g
group_a <- panel_final %>%
  filter(ccei_1g < ccei_2g) %>%
  arrange(ccei_1g, ccei_2g) %>%
  mutate(group_order = row_number())

group_b <- panel_final %>%
  filter(ccei_1g >= ccei_2g) %>%
  arrange(ccei_1g, ccei_2g) %>%
  mutate(group_order = row_number() + nrow(group_a))

panel_sorted <- bind_rows(group_a, group_b)

df_long <- panel_sorted %>%
  select(group_order, group_id, ccei_1g, ccei_2g) %>%
  pivot_longer(cols = c(ccei_1g, ccei_2g),
               names_to = "type", values_to = "value")

ggplot(df_long, aes(x = group_order, y = value, color = type)) +
  geom_point(size = 1.2, alpha = 0.8) +
  labs(
    title = "CCEI_hg and CCEI_lg Across Groups (Baseline)\nSplit by CCEI_hg < CCEI_lg vs. ≥, Then Sorted",
    x = "Group (segmented sort)",
    y = "CCEI Value",
    color = "Index Type"
  ) +
  scale_color_manual(values = c("ccei_1g" = "red", "ccei_2g" = "blue"),
                     labels = c("CCEI_hg", "CCEI_lg")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 15, lineheight = 1.1))


group_a <- panel_final %>%
  filter(ccei_1g_end < ccei_2g_end) %>%
  arrange(ccei_1g_end, ccei_2g_end) %>%
  mutate(group_order = row_number())

group_b <- panel_final %>%
  filter(ccei_1g_end >= ccei_2g_end) %>%
  arrange(ccei_1g_end, ccei_2g_end) %>%
  mutate(group_order = row_number() + nrow(group_a))

panel_sorted <- bind_rows(group_a, group_b)

df_long <- panel_sorted %>%
  select(group_order, group_id, ccei_1g_end, ccei_2g_end) %>%
  pivot_longer(cols = c(ccei_1g_end, ccei_2g_end),
               names_to = "type", values_to = "value")

ggplot(df_long, aes(x = group_order, y = value, color = type)) +
  geom_point(size = 1.2, alpha = 0.8) +
  labs(
    title = "CCEI_hg and CCEI_lg Across Groups (Endline)\nSplit by CCEI_hg < CCEI_lg vs. ≥, Then Sorted",
    x = "Group (segmented sort)",
    y = "CCEI Value",
    color = "Index Type"
  ) +
  scale_color_manual(values = c("ccei_1g_end" = "red", "ccei_2g_end" = "blue"),
                     labels = c("CCEI_hg", "CCEI_lg")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 15, lineheight = 1.1))


#######################################################

rm(list = ls())
load("../results/panel_final.RData")

library(ggplot2)
library(dplyr)
library(tidyr)

names(panel_final)



panel_final <- panel_final %>%
  mutate(
    RT_h_dist_base = abs(RT_h_baseline - RT_g_baseline),
    RT_l_dist_base = abs(RT_l_baseline - RT_g_baseline),
    RT_h_dist_end  = abs(RT_h_endline - RT_g_endline),
    RT_l_dist_end  = abs(RT_l_endline - RT_g_endline)
  )

# baseline
df_base <- panel_final %>%
  select(group_id, RT_h_dist_base, RT_l_dist_base) %>%
  pivot_longer(cols = c(RT_h_dist_base, RT_l_dist_base),
               names_to = "type", values_to = "value") %>%
  mutate(time = "Baseline")

# endline
df_end <- panel_final %>%
  select(group_id, RT_h_dist_end, RT_l_dist_end) %>%
  pivot_longer(cols = c(RT_h_dist_end, RT_l_dist_end),
               names_to = "type", values_to = "value") %>%
  mutate(time = "Endline")

# 결합
df_all <- bind_rows(df_base, df_end) %>%
  mutate(
    type = recode(type,
                  "RT_h_dist_base" = "|RT_h - RT_g|",
                  "RT_l_dist_base" = "|RT_l - RT_g|",
                  "RT_h_dist_end"  = "|RT_h - RT_g|",
                  "RT_l_dist_end"  = "|RT_l - RT_g|")
  )


ggplot(df_all %>% filter(time == "Baseline"), 
       aes(x = value, color = type, linetype = type)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Distribution of |RT - RT[g]| at Baseline",  # ← 추가
    x = expression(paste("|RT - RT[g]|")),
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1)) +
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
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold")  # 제목 스타일
  )


ggplot(df_all %>% filter(time == "Endline"), 
       aes(x = value, color = type, linetype = type)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Distribution of |RT - RT[g]| at Endline",
    x = expression(paste("|RT - RT[g]|")),
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1)) +
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
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold")
  )


# RT_h0: 더 합리적인 개인의 RT (baseline 기준)
panel_final <- panel_final %>%
  mutate(
    RT_h0 = ifelse(high == 1, RT_1, RT_2),
    RT_l0 = ifelse(high == 1, RT_2, RT_1)
  )

# RT_h1: 더 합리적인 개인의 RT (endline 기준)
panel_final <- panel_final %>%
  mutate(
    RT_h1 = ifelse(high_end == 1, RT_1_end, RT_2_end),
    RT_l1 = ifelse(high_end == 1, RT_2_end, RT_1_end)
  )

save(panel_final, file = "../results/panel_final.RData")


# 거리 계산
panel_final <- panel_final %>%
  mutate(
    RT_h0_dist = abs(RT_h0 - RT_g_baseline),
    RT_l0_dist = abs(RT_l0 - RT_g_baseline),
    RT_h1_dist = abs(RT_h1 - RT_g_endline),
    RT_l1_dist = abs(RT_l1 - RT_g_endline)
  )



ggplot(df_all %>% filter(time == "Baseline"), 
       aes(x = value, color = type, linetype = type)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Distribution of |RT - RT[g]| at Baseline (High CCEI-Based)",
    x = expression(paste("|RT - RT[g]|")),
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1)) +
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
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold")
  )


ggplot(df_all %>% filter(time == "Endline"), 
       aes(x = value, color = type, linetype = type)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Distribution of |RT - RT[g]| at Endline (High CCEI-Based)",
    x = expression(paste("|RT - RT[g]|")),
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1)) +
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
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold")
  )

#########################################################

# 초기화 및 데이터 불러오기
rm(list = ls())
load("../results/panel_individual.RData")

library(dplyr)
library(tidyr)
library(ggplot2)

ggplot(panel_individual %>% filter(time == 0),
       aes(x = RT, y = ccei_i)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE, size = 1) +
  labs(
    title = "RT vs. CCEI at Baseline",
    x = "RT",
    y = "CCEI (individual)"
  ) +
  theme_minimal(base_size = 13)

ggplot(panel_individual %>% filter(time == 1),
       aes(x = RT, y = ccei_i)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE, size = 1) +
  labs(
    title = "RT vs. CCEI at Endline",
    x = "RT",
    y = "CCEI (individual)"
  ) +
  theme_minimal(base_size = 13)


#####################

rm(list = ls())
load("../results/panel_individual.RData")

library(dplyr)
library(tidyr)
library(ggplot2)

# 개인별 절댓값 차이 계산
panel_individual <- panel_individual %>%
  mutate(
    ccei_dist = abs(ccei_i - ccei_g),
    type = ifelse(high_dummy == 1, "|ccei_h - ccei_g|", "|ccei_l - ccei_g|"),
    time_label = ifelse(time == 0, "Baseline", "Endline")
  )

# 시각화용 데이터 추출
df_ccei <- panel_individual %>%
  filter(!is.na(ccei_dist)) %>%
  select(id, group_id, time_label, type, ccei_dist)

# CDF 그래프 - Baseline
ggplot(df_ccei %>% filter(time_label == "Baseline"),
       aes(x = ccei_dist, color = type, linetype = type)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Distribution of |ccei_i - ccei_g| at Baseline",
    x = expression(paste("|ccei_i - ccei_g|")),
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1))  +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.65, 0.3),
    legend.justification = c("left", "top"),
    legend.text = element_text(size = 12),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold")
  )


# CDF 그래프 - Endline
ggplot(df_ccei %>% filter(time_label == "Endline"),
       aes(x = ccei_dist, color = type, linetype = type)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Distribution of |ccei_i - ccei_g| at Endline",
    x = expression(paste("|ccei_i - ccei_g|")),
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1)) +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.65, 0.3),
    legend.justification = c("left", "top"),
    legend.text = element_text(size = 12),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold")
  )

##########################################

# 초기화 및 데이터 불러오기
rm(list = ls())
load("../results/panel_individual.RData")

library(dplyr)
library(tidyr)
library(ggplot2)

# 분석용 변수 생성
panel_individual <- panel_individual %>%
  mutate(
    ccei_dist = abs(ccei_i - ccei_g),
    lower = pmin(ccei_i, ccei_j),
    upper = pmax(ccei_i, ccei_j),
    g_between_ij = (ccei_g >= lower & ccei_g <= upper),
    g_position = case_when(
      ccei_g < lower ~ "below",
      ccei_g > upper ~ "above",
      TRUE ~ "between"
    ),
    type = ifelse(high_dummy == 1, "|ccei_h - ccei_g|", "|ccei_l - ccei_g|"),
    time_label = ifelse(time == 0, "Baseline", "Endline")
  )

table(panel_individual$time_label, panel_individual$g_between_ij, useNA = "ifany")

table(panel_individual$time_label, panel_individual$g_position, useNA = "ifany")




# 1. between인 관측치만 필터링
df_ccei <- panel_individual %>%
  filter(g_position == "between", !is.na(ccei_dist)) %>%
  select(id, group_id, time_label, type, ccei_dist)

# 2. CDF 그래프 - Baseline
ggplot(df_ccei %>% filter(time_label == "Baseline"),
       aes(x = ccei_dist, color = type, linetype = type)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Distribution of |ccei_i - ccei_g| (Only between) at Baseline",
    x = expression(paste("|ccei_i - ccei_g|")),
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1))  +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.65, 0.3),
    legend.justification = c("left", "top"),
    legend.text = element_text(size = 12),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold")
  )

# 3. CDF 그래프 - Endline
ggplot(df_ccei %>% filter(time_label == "Endline"),
       aes(x = ccei_dist, color = type, linetype = type)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Distribution of |ccei_i - ccei_g| (Only between) at Endline",
    x = expression(paste("|ccei_i - ccei_g|")),
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1)) +
  scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.65, 0.3),
    legend.justification = c("left", "top"),
    legend.text = element_text(size = 12),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold")
  )
