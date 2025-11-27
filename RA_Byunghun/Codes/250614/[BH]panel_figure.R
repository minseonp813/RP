library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("../results/panel_final.RData")
load("../results/panel_pbl.RData")

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

