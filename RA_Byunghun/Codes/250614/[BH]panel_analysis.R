rm(list = ls())

load("../results/panel_final.RData")

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
