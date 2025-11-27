rm(list = ls())

# 데이터 로드
load("../results/panel_final.RData")
load("../results/panel_individual.RData")
load("../data/baseline_raw.RData")
load("../data/endline_raw.RData")

library(dplyr)
library(haven)

# 0. partner_id == 0 보완
# round_number == 19 기준으로 partner_id 정보 추출
partner_info <- baseline_raw %>%
  filter(round_number == 19 & partner_id != 0) %>%
  select(id, partner_id) %>%
  distinct()

# 기존 partner_id 제거 후, id 기준으로 join 하여 정확한 partner_id 복원
baseline_fixed <- baseline_raw %>%
  select(-partner_id) %>%
  left_join(partner_info, by = "id")

# 1. group_id, groupid 생성
baseline_clean <- baseline_fixed %>%
  mutate(
    big_id   = pmax(id, partner_id, na.rm = TRUE),
    small_id = pmin(id, partner_id, na.rm = TRUE),
    group_id = paste0(big_id, small_id),  # 문자열형 식별자
    groupid  = big_id                     # 숫자형 식별자 (Stata 스타일)
  )

# 2. mover 결측치 보완: 같은 id에 대해 known 값 복사
mover_status <- baseline_clean %>%
  filter(!is.na(mover)) %>%
  distinct(id, mover)

baseline_clean <- baseline_clean %>%
  select(-mover) %>%
  left_join(mover_status, by = "id")

# 3. high_id 생성
baseline_clean <- baseline_clean %>%
  mutate(high_id = ifelse(high == 1, id, partner_id))

# 4. CCEI 정보 merge (group_id 기준, 자료형 통일)
panel_final <- panel_final %>%
  mutate(group_id = as.character(group_id))

baseline_clean <- baseline_clean %>%
  left_join(panel_final %>% select(group_id, ccei_h, ccei_l, ccei_g), by = "group_id")

# 5. relconsmp 계산
baseline_clean <- baseline_clean %>%
  mutate(relconsmp = coord_y / (coord_x + coord_y))

# 6. lnrelprice 계산
baseline_clean <- baseline_clean %>%
  mutate(lnrelprice = log(intercept_x / intercept_y))

# 7. game_type 숫자화
baseline_clean <- baseline_clean %>%
  mutate(game_type = case_when(
    game_type == "individual" ~ 1,
    game_type == "collective" ~ 2,
    TRUE ~ NA_real_
  ))

# 8. I_ig merge (time == 0 기준, id로)
I_ig_baseline <- panel_individual %>%
  filter(time == 0) %>%
  select(id, I_ig)

baseline_clean <- baseline_clean %>%
  left_join(I_ig_baseline, by = "id")

# 9. panel_final 기준 유효 id만 필터링
valid_ids <- unique(c(panel_final$id.x, panel_final$partner_id.x))

baseline_clean <- baseline_clean %>%
  filter(id %in% valid_ids)

# 10. 저장
write_dta(baseline_clean, "C:/Users/hahn0/Dropbox/RP/Data/Risk_merged_Baseline.dta")


############################################################################

rm(list = ls())

# 데이터 로드
load("../results/panel_final.RData")
load("../results/panel_individual.RData")
load("../data/endline_raw.RData")

library(dplyr)
library(haven)

# 0. partner_id == 0 보완
partner_info <- endline_raw %>%
  filter(round_number == 19 & partner_id != 0) %>%
  select(id, partner_id) %>%
  distinct()

endline_fixed <- endline_raw %>%
  select(-partner_id) %>%
  left_join(partner_info, by = "id")

# 1. group_id, groupid 생성
endline_clean <- endline_fixed %>%
  mutate(
    big_id   = pmax(id, partner_id, na.rm = TRUE),
    small_id = pmin(id, partner_id, na.rm = TRUE),
    group_id = paste0(big_id, small_id),  # 문자열형 식별자
    groupid  = big_id                     # 숫자형 식별자 (Stata 스타일)
  )

# 2. mover 결측치 보완
mover_status <- endline_clean %>%
  filter(!is.na(mover)) %>%
  distinct(id, mover)

endline_clean <- endline_clean %>%
  select(-mover) %>%
  left_join(mover_status, by = "id")

# 3. high_id 생성
endline_clean <- endline_clean %>%
  mutate(high_id = ifelse(high == 1, id, partner_id))

# 4. CCEI 정보 merge (Endline용 컬럼 사용)
panel_final <- panel_final %>%
  mutate(group_id = as.character(group_id))

endline_clean <- endline_clean %>%
  left_join(panel_final %>% select(group_id, ccei_h_end, ccei_l_end, ccei_g_end), by = "group_id") %>%
  rename(
    ccei_h = ccei_h_end,
    ccei_l = ccei_l_end,
    ccei_g = ccei_g_end
  )

# 5. relconsmp 계산
endline_clean <- endline_clean %>%
  mutate(relconsmp = coord_y / (coord_x + coord_y))

# 6. lnrelprice 계산
endline_clean <- endline_clean %>%
  mutate(lnrelprice = log(intercept_x / intercept_y))

# 7. game_type 숫자화
endline_clean <- endline_clean %>%
  mutate(game_type = case_when(
    game_type == "individual" ~ 1,
    game_type == "collective" ~ 2,
    TRUE ~ NA_real_
  ))

# 8. I_ig merge (time == 1 기준)
I_ig_endline <- panel_individual %>%
  filter(time == 1) %>%
  select(id, I_ig)

endline_clean <- endline_clean %>%
  left_join(I_ig_endline, by = "id")

# 9. panel_final 기준 유효 id만 필터링
valid_ids <- unique(c(panel_final$id.x, panel_final$partner_id.x))

endline_clean <- endline_clean %>%
  filter(id %in% valid_ids)

# 10. 저장
write_dta(endline_clean, "C:/Users/hahn0/Dropbox/RP/Data/Risk_merged_Endline.dta")
