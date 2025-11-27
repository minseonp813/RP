rm(list = ls())

data <- read.csv("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/Raw Data_Post/risk_preference_solo_pair.csv")

data <- subset(data, !(is.na(player.coord_x) | player.coord_x == ""))
data <- data[order(data$participant.label), ]

library(dplyr)

data <- data %>%
  group_by(participant.label) %>%
  mutate(
    partner_id = player.partner[subsession.round_number == 19][1]
  ) %>%
  ungroup()

partnered_participants <- data %>%
  filter(!is.na(partner_id) & partner_id != "") %>%
  distinct(participant.label)
cat("파트너 정보가 있는 사람 수:", nrow(partnered_participants), "\n")

# 알파벳 치환 맵 정의
replace_map <- c(
  "B" = "11", "P" = "12", "C" = "13", "F" = "14",
  "M" = "15", "R" = "16", "L" = "21", "Q" = "22",
  "J" = "23", "A" = "24", "K" = "25", "E" = "26"
)

# 라벨 변환 함수 (에러 대신 "0" 반환)
convert_label <- function(label) {
  if (is.na(label) || label == "") return("0")  # NA 또는 빈 문자열 처리
  prefix <- substr(label, 1, 1)
  suffix <- substr(label, 2, nchar(label))
  new_prefix <- replace_map[prefix]
  if (is.na(new_prefix)) return("0")  # 잘못된 접두어도 "0"
  paste0(new_prefix, suffix)
}

# 변환 적용 + group_id 생성
data <- data %>%
  mutate(
    numeric_label = sapply(participant.label, convert_label),
    numeric_partner = sapply(partner_id, convert_label),
    big_id = ifelse(numeric_label > numeric_partner, numeric_label, numeric_partner),
    small_id = ifelse(numeric_label > numeric_partner, numeric_partner, numeric_label),
    group_id = paste0(big_id, small_id)
  )

# 확인
head(data[, c("participant.label", "partner_id", "numeric_label", "numeric_partner", "group_id")])

# 1. 유효한 쌍 추출
valid_pairs <- data %>%
  filter(numeric_partner != "0") %>%
  distinct(numeric_label, numeric_partner)

# 2. 0인 행 추출
zero_partner_rows <- data %>%
  filter(numeric_partner == "0") %>%
  select(participant.label, numeric_label)

# 3. 본인의 numeric_label이 다른 쌍의 numeric_partner인 경우 찾기
replacement_partners <- zero_partner_rows %>%
  left_join(valid_pairs, by = c("numeric_label" = "numeric_partner")) %>%
  rename(new_partner = numeric_label.y) %>%
  select(participant.label, new_partner) %>%
  distinct(participant.label, .keep_all = TRUE)  # 중복 제거 핵심

# 4. 원래 데이터에 반영
data <- data %>%
  left_join(replacement_partners, by = "participant.label") %>%
  mutate(
    numeric_partner = ifelse(numeric_partner == "0" & !is.na(new_partner), new_partner, numeric_partner),
    big_id = ifelse(numeric_label > numeric_partner, numeric_label, numeric_partner),
    small_id = ifelse(numeric_label > numeric_partner, numeric_partner, numeric_label),
    group_id = paste0(big_id, small_id)
  ) %>%
  select(-new_partner)


# numeric_partner == "0"인 행 제거
data <- data %>%
  filter(numeric_partner != "0")


# group_id 고유값 개수 확인
length(unique(data$numeric_label))
length(unique(data$numeric_partner))
length(unique(data$group_id))

# 외부 파일 불러오기
load("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/results/baseline_final.RData")
ls()  # 어떤 객체가 로드되었는지 확인

# (예시: baseline_data라는 객체가 있다고 가정)
group_ids_raw <- unique(data$group_id)
group_ids_new <- unique(baseline_final$group_id)

# endline_same 생성
baseline_final$endline_same <- ifelse(baseline_final$group_id %in% group_ids_raw, 1, 0)
table(baseline_final$endline_same)

group_counts <- data %>%
  count(group_id, name = "n_in_raw")  # group_id별 빈도수
baseline_final <- baseline_final %>%
  left_join(group_counts, by = "group_id")
table(baseline_final$endline_same, useNA = "ifany")       # 여전히 총 개수 확인
table(baseline_final$n_in_raw[baseline_final$endline_same == 1])  # 등장 횟수 분포

save(baseline_final, file = "C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/results/baseline_final_updated.RData")





library(dplyr)
library(writexl)

# 1. n_in_raw != 72인 group_id 추출
invalid_groups <- baseline_final %>%
  filter(n_in_raw != 72) %>%
  pull(group_id)

# 2. 해당 group_id에 속한 id, partner_id 추출
invalid_ids <- baseline_final %>%
  filter(group_id %in% invalid_groups) %>%
  select(id, partner_id) %>%
  distinct() %>%
  unlist() %>%
  unique()

# 3. data에서 해당 id나 partner_id 포함된 행 제거
original_n <- nrow(data)

data_cleaned <- data %>%
  filter(!(numeric_label %in% invalid_ids | numeric_partner %in% invalid_ids))

removed_n <- original_n - nrow(data_cleaned)
cat("삭제된 행 수:", removed_n, "\n")

length(unique(data_cleaned$numeric_label))
length(unique(data_cleaned$numeric_partner))
length(unique(data_cleaned$group_id))

data_cleaned <- data_cleaned %>%
  filter(participant.label != partner_id)

write_xlsx(data_cleaned, path = "C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/Raw Data_Post/risk_data_export.xlsx")

############################################################
rm(list = ls())

library(readxl)

endline_raw <- read_excel("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/endline_raw.xlsx")

cat("고유 ID 개수:", length(unique(endline_raw$id)), "\n")

# 새로운 칼럼 생성: partner_id가 id에 존재하면 1, 아니면 0
endline_raw$partner_exists_in_id <- ifelse(endline_raw$partner_id %in% endline_raw$id, 1, 0)
# 확인 (원한다면 분포 보기)
table(endline_raw$partner_exists_in_id)


library(dplyr)

# id별 행 개수 세기
id_counts <- endline_raw %>%
  count(id, name = "n_rows")

# 결과 확인 (예: 상위 10개만 보기)
head(id_counts, 10)

# 전체 분포 요약 (같은 개수 가진 id가 몇 개인지 등)
table(id_counts$n_rows)


save(endline_raw, file = "C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/endline_raw.RData")

####################################################################

library(haven)
library(dplyr)
library(stringr)

# 데이터 불러오기
panel <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")
risk <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/Risk_ByPair.dta")

# group_id 생성 + male 리코딩
risk <- risk %>%
  mutate(
    bigid = pmax(id, partner),
    smallid = pmin(id, partner),
    group_id = str_c(as.character(bigid), as.character(smallid)),
    male = if_else(male == 2, 0, male),
    male2 = if_else(male2 == 2, 0, male2)
  )

# 필요한 변수만 남기기
risk_sub <- risk %>%
  select(group_id, male, male2, outgoing, outgoing2, opened, opened2,
         agreeable, agreeable2, conscientious, conscientious2, stable, stable2,
         height, height2, height_gr, height_gr2)

# 병합
panel_merged <- panel %>%
  left_join(risk_sub, by = "group_id")

# 덮어쓰기 저장
write_dta(panel_merged, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")

###################################################################

library(haven)
library(dplyr)
library(stringr)

# 기존 데이터 로드
rm(list = ls())
load("../results/panel_individual.RData")
load("../results/panel_final.RData")

# 1. partner_id 생성
panel_individual <- panel_individual %>%
  mutate(
    id = as.character(id),
    group_id_str = as.character(group_id),
    id_1 = str_sub(group_id_str, 1, 7),
    id_2 = str_sub(group_id_str, 8, 14),
    partner_id = if_else(id == id_1, id_2, id_1)
  ) %>%
  select(-group_id_str, -id_1, -id_2)

# 2. network 데이터 불러오기 및 id, obj_id 문자형 변환
network_raw <- read_dta("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/network_survey.dta")

network_subset <- network_raw %>%
  mutate(
    id = as.character(id),
    obj_id = as.character(obj_id)
  ) %>%
  select(id, obj_id, t, best, with_sns, with_study, without_study, borrow, consult, be_consulted)

# 3. 머지 (단방향, 그대로 붙이기)
panel_individual <- panel_individual %>%
  left_join(
    network_subset,
    by = c("id" = "id", "partner_id" = "obj_id", "time" = "t")
  )

library(tidyr)

panel_individual <- panel_individual %>%
  mutate(across(
    c(best, with_sns, with_study, without_study, borrow, consult, be_consulted),
    ~ replace_na(., 0)
  ))


# 4. 결과 저장
save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "../results/panel_individual.dta")

##########################################################################

library(haven)
library(dplyr)
library(stringr)

# 기존 데이터 로드
rm(list = ls())
load("../results/panel_individual.RData")
load("../results/panel_final.RData")

# 1. partner_id 생성
panel_individual <- panel_individual %>%
  mutate(
    id = as.character(id),
    group_id_str = as.character(group_id),
    id_1 = str_sub(group_id_str, 1, 7),
    id_2 = str_sub(group_id_str, 8, 14),
    partner_id = if_else(id == id_1, id_2, id_1)
  ) %>%
  select(-group_id_str, -id_1, -id_2)

# 2. network 데이터 불러오기 및 id, obj_id 문자형 변환
network_raw <- read_dta("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/network_survey.dta")

network_subset <- network_raw %>%
  mutate(
    id = as.character(id),
    obj_id = as.character(obj_id)
  ) %>%
  select(id, obj_id, t, best, with_sns, with_study, without_study, borrow, consult, be_consulted)

# 3. 머지 (단방향, 그대로 붙이기)
panel_individual <- panel_individual %>%
  left_join(
    network_subset,
    by = c("id" = "id", "partner_id" = "obj_id", "time" = "t")
  )

library(tidyr)

panel_individual <- panel_individual %>%
  mutate(across(
    c(best, with_sns, with_study, without_study, borrow, consult, be_consulted),
    ~ replace_na(., 0)
  ))


# 4. 결과 저장
save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "../results/panel_individual.dta")

########################################################3

library(haven)
library(dplyr)

# 1. 데이터 로드
rm(list = ls())
load("../results/panel_final.RData")
dta <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")

# 2. 변수 목록 추출
names_rdata <- names(panel_final)
names_dta <- names(dta)

# 3. 변수 비교
common_vars <- intersect(names_rdata, names_dta)      # 둘 다 있는 변수
rdata_only_vars <- setdiff(names_rdata, names_dta)    # RData 에만 있는 변수
dta_only_vars <- setdiff(names_dta, names_rdata)      # dta 에만 있는 변수

# 5. RData 에만 있는 변수만 추출해서 dta에 붙이기
extra_vars <- panel_final %>% select(group_id, all_of(rdata_only_vars))

dta_updated <- dta %>%
  left_join(extra_vars, by = "group_id")

# 6. 최종 데이터 저장 (RData + Stata)
save(dta_updated, file = "../results/panel_final.RData")

dta_for_stata <- dta_updated %>%
  select(-id.x, -partner_id.x, -id.y, -partner_id.y)
write_dta(dta_for_stata, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")

###########################################################3


library(haven)
library(dplyr)

# 데이터 로드
rm(list = ls())
load("../results/panel_final.RData")
network_raw <- read_dta("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/network_survey.dta")

# network 데이터 전처리 (문자형 변환 + 필터링)
network_t0 <- network_raw %>%
  filter(t == 0) %>%
  mutate(id = as.character(id), obj_id = as.character(obj_id)) %>%
  select(id, obj_id, best, with_sns, with_study, without_study, borrow, consult, be_consulted)

network_t1 <- network_raw %>%
  filter(t == 1) %>%
  mutate(id = as.character(id), obj_id = as.character(obj_id)) %>%
  select(id, obj_id, best, with_sns, with_study, without_study, borrow, consult, be_consulted)

# panel_final 의 id.x, partner_id.x 문자형 변환
panel_final <- panel_final %>%
  mutate(id.x = as.character(id.x), partner_id.x = as.character(partner_id.x))

### 1) 초기(t == 0) 데이터 merge (방향 1)
panel_final <- panel_final %>%
  left_join(network_t0, by = c("id.x" = "id", "partner_id.x" = "obj_id")) %>%
  rename_with(~ paste0(., "_1"), .cols = c(best, with_sns, with_study, without_study, borrow, consult, be_consulted))

### 2) 후기(t == 1) 데이터 merge (방향 1, end)
panel_final <- panel_final %>%
  left_join(network_t1, by = c("id.x" = "id", "partner_id.x" = "obj_id")) %>%
  rename_with(~ paste0(., "_1_end"), .cols = c(best, with_sns, with_study, without_study, borrow, consult, be_consulted))

### 3) 초기(t == 0) 데이터 merge (방향 2)
panel_final <- panel_final %>%
  left_join(network_t0, by = c("partner_id.x" = "id", "id.x" = "obj_id")) %>%
  rename_with(~ paste0(., "_2"), .cols = c(best, with_sns, with_study, without_study, borrow, consult, be_consulted))

### 4) 후기(t == 1) 데이터 merge (방향 2, end)
panel_final <- panel_final %>%
  left_join(network_t1, by = c("partner_id.x" = "id", "id.x" = "obj_id")) %>%
  rename_with(~ paste0(., "_2_end"), .cols = c(best, with_sns, with_study, without_study, borrow, consult, be_consulted))

# 새로 만든 변수 목록
new_vars <- c(
  paste0(c("best", "with_sns", "with_study", "without_study", "borrow", "lend", "consult", "be_consulted"), "_1"),
  paste0(c("best", "with_sns", "with_study", "without_study", "borrow", "lend",  "consult", "be_consulted"), "_1_end"),
  paste0(c("best", "with_sns", "with_study", "without_study", "borrow", "lend",  "consult", "be_consulted"), "_2"),
  paste0(c("best", "with_sns", "with_study", "without_study", "borrow", "lend",  "consult", "be_consulted"), "_2_end")
)

# NA를 0으로 변경
library(tidyr)

panel_final <- panel_final %>%
  mutate(across(all_of(new_vars), ~ replace_na(., 0)))


# 6. 저장
save(panel_final, file = "../results/panel_final.RData")



# 어떤 변수들이 있는지 먼저 확인
names(panel_final)[c(92, 93, 94, 95)]
# 예시 결과: "id.x", "partner_id.x", "id.y", "partner_id.y"

# 후자의 중복 변수 제거
panel_final <- panel_final %>%
  select(-c(92, 93, 94, 95))

# 그리고 변수명 변경
panel_final <- panel_final %>%
  rename(
    id_x = id.x,
    partner_id_x = partner_id.x,
    id_y = id.y,
    partner_id_y = partner_id.y
  )

# Stata 파일로 저장
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")


##############################################################################

# panel_pbl

rm(list = ls())
load("../results/panel_final.RData")

names(panel_final)

# ---------------------------
# 0️⃣ mathscore 관련 endline용 변수 생성
# ---------------------------
panel_final$mathscore_max_end <- pmax(panel_final$mathscore_end, panel_final$mathscore2_end, na.rm = TRUE)
panel_final$mathscore_dist_end <- abs(panel_final$mathscore_end - panel_final$mathscore2_end)

panel_final$mathscore_max_missing_end <- ifelse(
  is.na(panel_final$mathscore_end) | is.na(panel_final$mathscore2_end),
  1,
  0
)
panel_final$mathscore_dist_missing_end <- panel_final$mathscore_max_missing_end

# ---------------------------
# 1️⃣ height Winsorize 및 계산
# ---------------------------
height_long <- data.frame(value = c(panel_final$height, panel_final$height2))
q_low <- quantile(height_long$value, probs = 0.01, na.rm = TRUE)
q_high <- quantile(height_long$value, probs = 0.99, na.rm = TRUE)

panel_final$height_w <- ifelse(panel_final$height < q_low, q_low,
                               ifelse(panel_final$height > q_high, q_high, panel_final$height))
panel_final$height2_w <- ifelse(panel_final$height2 < q_low, q_low,
                                ifelse(panel_final$height2 > q_high, q_high, panel_final$height2))

panel_final$height_max <- pmax(panel_final$height_w, panel_final$height2_w, na.rm = TRUE)
panel_final$height_dist <- abs(panel_final$height_w - panel_final$height2_w)

# ---------------------------
# 2️⃣ big5 성격 변수 max, dist 생성 + missing 처리
# ---------------------------
big5_pairs <- list(
  outgoing = "outgoing2",
  opened = "opened2",
  agreeable = "agreeable2",
  conscientious = "conscientious2",
  stable = "stable2"
)

for (var1 in names(big5_pairs)) {
  var2 <- big5_pairs[[var1]]
  
  # max / dist 계산
  panel_final[[paste0(var1, "_max")]] <- pmax(panel_final[[var1]], panel_final[[var2]], na.rm = TRUE)
  panel_final[[paste0(var1, "_dist")]] <- abs(panel_final[[var1]] - panel_final[[var2]])
  
  # missing indicator 계산 (나중에 outgoing 기준으로 하나만 남김)
  panel_final[[paste0(var1, "_max_missing")]] <- ifelse(is.na(panel_final[[var1]]) & is.na(panel_final[[var2]]), 1, 0)
  panel_final[[paste0(var1, "_dist_missing")]] <- ifelse(is.na(panel_final[[var1]]) | is.na(panel_final[[var2]]), 1, 0)
  
  # NA → 0으로 변환
  panel_final[[paste0(var1, "_max")]][is.na(panel_final[[paste0(var1, "_max")]])] <- 0
  panel_final[[paste0(var1, "_dist")]][is.na(panel_final[[paste0(var1, "_dist")]])] <- 0
}

# ---------------------------
# 3️⃣ big5 missing indicator 정리
# ---------------------------
panel_final$big5_max_missing <- panel_final$outgoing_max_missing
panel_final$big5_dist_missing <- panel_final$outgoing_dist_missing

# 나머지 missing indicator 삭제
drop_vars <- c(
  "opened_max_missing", "opened_dist_missing",
  "agreeable_max_missing", "agreeable_dist_missing",
  "conscientious_max_missing", "conscientious_dist_missing",
  "stable_max_missing", "stable_dist_missing",
  "outgoing_max_missing", "outgoing_dist_missing"
)
panel_final <- panel_final[, !(names(panel_final) %in% drop_vars)]

# ---------------------------
# 4️⃣ 변수 목록 설정
# ---------------------------
vars <- c(
  "group_id", "ccei_1", "ccei_2", "ccei_g", "f_ccei_1", "f_ccei_2", "f_ccei_col", "index_hg", "index_lg",
  "ccei_ind_max", "ccei_ind_dist", "height_gr_max", "height_gr_dist",
  "mathscore_max", "mathscore_dist", "mathscore_max_missing", "mathscore_dist_missing",
  "malepair_co", "friendship", "class", "RT_1", "RT_2",
  "height_max", "height_dist", "inclass_n_friends_1", "inclass_popularity_1",
  "inclass_n_friends_2", "inclass_popularity_2",
  # big5 변수
  "outgoing_max", "outgoing_dist",
  "opened_max", "opened_dist",
  "agreeable_max", "agreeable_dist",
  "conscientious_max", "conscientious_dist",
  "stable_max", "stable_dist",
  # big5 missing indicator 통합
  "big5_max_missing", "big5_dist_missing"
)

# ---------------------------
# 5️⃣ baseline 데이터 생성
# ---------------------------
matrix1 <- panel_final[vars]
matrix1$time <- 0

# ---------------------------
# 6️⃣ endline 데이터 생성
# ---------------------------
matrix2 <- matrix(NA, nrow = nrow(panel_final), ncol = length(vars), dimnames = list(NULL, vars))
matrix2 <- as.data.frame(matrix2)
matrix2$group_id <- panel_final$group_id

endline_map <- c(
  "ccei_1_end" = "ccei_1",
  "ccei_2_end" = "ccei_2",
  "ccei_g_end" = "ccei_g",
  "f_ccei_1_end" = "f_ccei_1",
  "f_ccei_2_end" = "f_ccei_2",
  "f_ccei_col_end" = "f_ccei_col",
  "index_hg_end" = "index_hg",
  "index_lg_end" = "index_lg",
  "ccei_ind_max_end" = "ccei_ind_max",
  "ccei_ind_dist_end" = "ccei_ind_dist",
  "friendship_end" = "friendship",
  "RT_1_end" = "RT_1", 
  "RT_2_end" = "RT_2",
  "mathscore_max_end" = "mathscore_max",
  "mathscore_dist_end" = "mathscore_dist",
  "mathscore_max_missing_end" = "mathscore_max_missing",
  "mathscore_dist_missing_end" = "mathscore_dist_missing",
  "inclass_n_friends_1_end" = "inclass_n_friends_1",
  "inclass_popularity_1_end" = "inclass_popularity_1",
  "inclass_n_friends_2_end" = "inclass_n_friends_2",
  "inclass_popularity_2_end" = "inclass_popularity_2"
)

for (orig in names(endline_map)) {
  target <- endline_map[[orig]]
  matrix2[[target]] <- panel_final[[orig]]
}

shared_vars <- setdiff(vars, c(
  "ccei_1", "ccei_2", "ccei_g", "f_ccei_1", "f_ccei_2", "f_ccei_col",
  "index_hg", "index_lg", "ccei_ind_max", "ccei_ind_dist",
  "friendship", "RT_1", "RT_2",
  "mathscore_max", "mathscore_dist", "mathscore_max_missing", "mathscore_dist_missing",
  "inclass_n_friends_1", "inclass_popularity_1", "inclass_n_friends_2", "inclass_popularity_2"
))
for (var in shared_vars) {
  matrix2[[var]] <- panel_final[[var]]
}

matrix2$time <- 1

# ---------------------------
# 7️⃣ 패널 데이터 합치기 및 정렬
# ---------------------------
panel_pbl <- rbind(matrix1, matrix2)
panel_pbl <- panel_pbl[order(panel_pbl$group_id, panel_pbl$time), ]

# ---------------------------
# 8️⃣ 추가 파생변수 생성
# ---------------------------
panel_pbl$risk_aversion_max <- pmax(panel_pbl$RT_1, panel_pbl$RT_2, na.rm = TRUE)
panel_pbl$risk_aversion_distance <- round(abs(panel_pbl$RT_1 - panel_pbl$RT_2), 7)

panel_pbl$f_ccei_max <- pmax(panel_pbl$f_ccei_1, panel_pbl$f_ccei_2, na.rm = TRUE)
panel_pbl$f_ccei_dist <- round(abs(panel_pbl$f_ccei_1 - panel_pbl$f_ccei_2), 7)

# ---------------------------
# 9️⃣ 저장
# ---------------------------
save(panel_pbl, file = "../results/panel_pbl.RData")

library(haven)
write_dta(panel_pbl, "C:/Users/hahn0/Dropbox/RP/Data/panel_pbl.dta")


################################################################

library(haven)
library(dplyr)
library(stringr)

# 기존 데이터 로드
rm(list = ls())
load("../results/panel_individual.RData")
load("../results/panel_final.RData")

names(panel_final)

panel_final <- panel_final %>%
  mutate(
    f_ccei_g = f_ccei_col,
    f_ccei_g_end = f_ccei_col_end
  )


# 1️⃣ New1 계산
panel_final <- panel_final %>%
  mutate(
    f_new1_I_hg = (f_ccei_g - f_ccei_hg) / (f_ccei_g - f_ccei_hlg),
    f_new1_I_lg = 1 - f_new1_I_hg,
    f_new1_I_hg_end = (f_ccei_g_end - f_ccei_hg_end) / (f_ccei_g_end - f_ccei_hlg_end),
    f_new1_I_lg_end = 1 - f_new1_I_hg_end
  )

# 2️⃣ temp lg 계산
panel_final <- panel_final %>%
  mutate(
    temp_lg = (f_ccei_g - f_ccei_lg) / (f_ccei_g - f_ccei_hlg),
    temp_lg_end = (f_ccei_g_end - f_ccei_lg_end) / (f_ccei_g_end - f_ccei_hlg_end)
  )

# 3️⃣ New2 계산
panel_final <- panel_final %>%
  mutate(
    f_new2_I_lg = (f_new1_I_lg + temp_lg) / 2,
    f_new2_I_hg = (f_new1_I_hg + (1 - temp_lg)) / 2,
    f_new2_I_lg_end = (f_new1_I_lg_end + temp_lg_end) / 2,
    f_new2_I_hg_end = (f_new1_I_hg_end + (1 - temp_lg_end)) / 2
  )

# 4️⃣ temp 삭제
panel_final <- panel_final %>%
  select(-temp_lg, -temp_lg_end)

# ✅ panel_final을 RData로 저장
save(panel_final, file = "../results/panel_final.RData")

# 어떤 변수들이 있는지 먼저 확인
names(panel_final)[c(92, 93, 94, 95)]
# 예시 결과: "id.x", "partner_id.x", "id.y", "partner_id.y"

# 후자의 중복 변수 제거
panel_final <- panel_final %>%
  select(-c(92, 93, 94, 95))

# 그리고 변수명 변경
panel_final <- panel_final %>%
  rename(
    id_x = id.x,
    partner_id_x = partner_id.x,
    id_y = id.y,
    partner_id_y = partner_id.y
  )

# Stata 파일로 저장
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")


#################################################################33

library(haven)
library(dplyr)

# 데이터 로드
rm(list = ls())
load("../results/panel_individual.RData")
load("../results/panel_final.RData")

# new_vars 생성
new_vars <- panel_final %>%
  select(
    group_id,
    f_new1_I_hg, f_new1_I_lg, f_new1_I_hg_end, f_new1_I_lg_end,
    f_new2_I_hg, f_new2_I_lg, f_new2_I_hg_end, f_new2_I_lg_end
  )

# 개인 데이터에 join 후 f_high 기준으로 값 할당
panel_individual <- panel_individual %>%
  left_join(new_vars, by = "group_id") %>%
  mutate(
    # new1
    f_new1_i = case_when(
      time == 0 & f_high == 1 ~ f_new1_I_hg,
      time == 0 & f_high == 0 ~ f_new1_I_lg,
      time == 1 & f_high == 1 ~ f_new1_I_hg_end,
      time == 1 & f_high == 0 ~ f_new1_I_lg_end
    ),
    f_new1_j = case_when(
      time == 0 & f_high == 1 ~ f_new1_I_lg,
      time == 0 & f_high == 0 ~ f_new1_I_hg,
      time == 1 & f_high == 1 ~ f_new1_I_lg_end,
      time == 1 & f_high == 0 ~ f_new1_I_hg_end
    ),
    # new2
    f_new2_i = case_when(
      time == 0 & f_high == 1 ~ f_new2_I_hg,
      time == 0 & f_high == 0 ~ f_new2_I_lg,
      time == 1 & f_high == 1 ~ f_new2_I_hg_end,
      time == 1 & f_high == 0 ~ f_new2_I_lg_end
    ),
    f_new2_j = case_when(
      time == 0 & f_high == 1 ~ f_new2_I_lg,
      time == 0 & f_high == 0 ~ f_new2_I_hg,
      time == 1 & f_high == 1 ~ f_new2_I_lg_end,
      time == 1 & f_high == 0 ~ f_new2_I_hg_end
    )
  ) %>%
  # 중간 변수 삭제
  select(-starts_with("f_new1_I_"), -starts_with("f_new2_I_"))

# 결과 저장
save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")




library(dplyr)

# 새로운 변수 생성
panel_final <- panel_final %>%
  mutate(test = f_ccei_g == f_ccei_hlg)
panel_final <- panel_final %>%
  mutate(test2 = f_ccei_g_end == f_ccei_hlg_end)

# 결과 빈도 확인
table(panel_final$test)
table(panel_final$test2)



subdata <- panel_individual %>%
  select(id, group_id, f_high, f_new1_i, f_new1_j, f_new2_i, f_new2_j)
subdata_final <- panel_final %>%
  select(
    id.x, partner_id.x, group_id,
    f_high, f_high_end,
    f_new1_I_hg, f_new1_I_lg, f_new1_I_hg_end, f_new1_I_lg_end,
    f_new2_I_hg, f_new2_I_lg, f_new2_I_hg_end, f_new2_I_lg_end
  )

############################################################


# 라이브러리 로드
library(haven)
library(dplyr)
library(tidyr)

# 기존 데이터 로드
rm(list = ls())
load("../results/panel_final.RData")
network_raw <- read_dta("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/network_survey.dta")

# lend 변수만 추출해서 새로 전처리
network_t0_lend <- network_raw %>%
  filter(t == 0) %>%
  mutate(id = as.character(id), obj_id = as.character(obj_id)) %>%
  select(id, obj_id, lend)

network_t1_lend <- network_raw %>%
  filter(t == 1) %>%
  mutate(id = as.character(id), obj_id = as.character(obj_id)) %>%
  select(id, obj_id, lend)

# 방향 1, t == 0
panel_final <- panel_final %>%
  left_join(network_t0_lend, by = c("id_x" = "id", "partner_id_x" = "obj_id")) %>%
  rename(lend_1 = lend)

# 방향 1, t == 1
panel_final <- panel_final %>%
  left_join(network_t1_lend, by = c("id_x" = "id", "partner_id_x" = "obj_id")) %>%
  rename(lend_1_end = lend)

# 방향 2, t == 0
panel_final <- panel_final %>%
  left_join(network_t0_lend, by = c("partner_id_x" = "id", "id_x" = "obj_id")) %>%
  rename(lend_2 = lend)

# 방향 2, t == 1
panel_final <- panel_final %>%
  left_join(network_t1_lend, by = c("partner_id_x" = "id", "id_x" = "obj_id")) %>%
  rename(lend_2_end = lend)

# NA 값은 0으로 변환
panel_final <- panel_final %>%
  mutate(across(c(lend_1, lend_1_end, lend_2, lend_2_end), ~ replace_na(., 0)))

# 최종 데이터 저장
save(panel_final, file = "../results/panel_final.RData")
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")

#######################################################

# 라이브러리 로드
library(haven)
library(dplyr)
library(tidyr)

# 데이터 로드
rm(list = ls())
load("../results/panel_final.RData")
network_raw <- read_dta("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/network_survey.dta")

### Step 1: network_raw에서 id 별 친구 수 계산
# t == 0
friends_t0 <- network_raw %>%
  filter(t == 0) %>%
  count(id, name = "n_friends")

# t == 1
friends_t1 <- network_raw %>%
  filter(t == 1) %>%
  count(id, name = "n_friends_end")

# 친구 수 계산하면서 id를 character로 변환
friends_t0 <- network_raw %>%
  filter(t == 0) %>%
  mutate(id = as.character(id)) %>%
  count(id, name = "n_friends")

friends_t1 <- network_raw %>%
  filter(t == 1) %>%
  mutate(id = as.character(id)) %>%
  count(id, name = "n_friends_end")


### Step 2: panel_final에 id_x 기준 merge → 학생 1 기준
panel_final <- panel_final %>%
  mutate(id_x = as.character(id_x), partner_id_x = as.character(partner_id_x)) %>%  # 혹시 모를 형변환
  left_join(friends_t0, by = c("id_x" = "id")) %>%
  rename(n_friends_1 = n_friends) %>%
  left_join(friends_t1, by = c("id_x" = "id")) %>%
  rename(n_friends_1_end = n_friends_end)

### Step 3: panel_final에 partner_id_x 기준 merge → 학생 2 기준
panel_final <- panel_final %>%
  left_join(friends_t0, by = c("partner_id_x" = "id")) %>%
  rename(n_friends_2 = n_friends) %>%
  left_join(friends_t1, by = c("partner_id_x" = "id")) %>%
  rename(n_friends_2_end = n_friends_end)

### Step 4: NA를 0으로 변경
panel_final <- panel_final %>%
  mutate(across(c(n_friends_1, n_friends_1_end, n_friends_2, n_friends_2_end), ~ replace_na(., 0)))

### Step 5: 저장
save(panel_final, file = "../results/panel_final.RData")
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")

names(panel_final)

#############################################################333


# ===============================================
# 라이브러리 로드 및 데이터 불러오기
# ===============================================

library(haven)
library(dplyr)
library(readr)

# 환경 초기화 및 데이터 로드
rm(list = ls())
load("../results/panel_final.RData")

# ===============================================
# 기존 mathscore_end 변수 이름 변경 (삭제 아님!)
# ===============================================

panel_final <- panel_final %>%
  rename(
    temp_mathscore_end = mathscore_end,
    temp_mathscore2_end = mathscore2_end
  )

# ===============================================
# Cognitive_end_BH.csv 불러오기
# ===============================================

cog_data <- read_csv("C:/Users/hahn0/Desktop/설문/Raw Data_Post/Cognitive_end_BH.csv")

# 데이터 프레임이 id, score 두 칼럼인지 확인
# head(cog_data)

# ===============================================
# panel_final 에 새로운 변수 추가 (초기값 NA)
# ===============================================

panel_final <- panel_final %>%
  mutate(
    mathscore_end = NA_real_,
    mathscore2_end = NA_real_
  )

# ===============================================
# id 기준으로 mathscore 매칭
# ===============================================

for (i in 1:nrow(cog_data)) {
  id <- cog_data$id[i]
  score <- cog_data$score[i]
  
  # panel_final 에서 해당 id 가 어디에 있는지 확인
  match_row <- which(panel_final$id_x == id | panel_final$partner_id_x == id)
  
  # 매칭된 행이 정확히 1개이면 mathscore 값을 넣음
  if (length(match_row) == 1) {
    if (panel_final$id_x[match_row] == id) {
      panel_final$mathscore_end[match_row] <- score
    } else if (panel_final$partner_id_x[match_row] == id) {
      panel_final$mathscore2_end[match_row] <- score
    }
  }
}

View(panel_final %>% select(id_x, partner_id_x, temp_mathscore_end, temp_mathscore2_end, mathscore_end, mathscore2_end))
panel_final <- panel_final %>%
  select(-temp_mathscore_end, -temp_mathscore2_end)

save(panel_final, file = "../results/panel_final.RData")
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")

names(panel_final)

#################################################################


library(dplyr)
library(haven)

# 데이터 로드
rm(list = ls())
load("../results/panel_final.RData")
panel_individual <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

# =================================================
# time == 1 인 부분만 mathscore 값 삭제
# =================================================

panel_individual <- panel_individual %>%
  mutate(
    mathscore_i = if_else(time == 1, NA_real_, mathscore_i),
    mathscore_j = if_else(time == 1, NA_real_, mathscore_j)
  )

# =================================================
# panel_final 의 각 group_id 별로 매칭
# =================================================

for (i in 1:nrow(panel_final)) {
  group <- panel_final$group_id[i]
  id_x <- panel_final$id_x[i]
  partner_id_x <- panel_final$partner_id_x[i]
  score1 <- panel_final$mathscore_end[i]
  score2 <- panel_final$mathscore2_end[i]
  
  # panel_individual 에서 group_id 가 같은 행들 중 time == 1 만 선택
  rows <- which(panel_individual$group_id == group & panel_individual$time == 1)
  
  # 일반적으로 group_id 로 찾으면 두 명이니까 rows 길이는 2일 것
  for (r in rows) {
    id <- panel_individual$id[r]
    
    if (id == id_x) {
      panel_individual$mathscore_i[r] <- score1
      panel_individual$mathscore_j[r] <- score2
    } else if (id == partner_id_x) {
      panel_individual$mathscore_i[r] <- score2
      panel_individual$mathscore_j[r] <- score1
    }
  }
}

save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

##################################################################

library(dplyr)
library(haven)

# 데이터 로드
rm(list = ls())
load("../results/panel_final.RData")
panel_individual <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

# =================================================
# panel_individual 에 새로운 변수 추가 (NA 초기화)
# =================================================

panel_individual <- panel_individual %>%
  mutate(
    outgoing = NA_real_,
    opened = NA_real_,
    agreeable = NA_real_,
    conscientious = NA_real_,
    stable = NA_real_
  )

# =================================================
# group_id 기준으로 매칭해서 Big5 성격 변수 넣기
# =================================================

for (i in 1:nrow(panel_final)) {
  group <- panel_final$group_id[i]
  id_x <- panel_final$id_x[i]
  partner_id_x <- panel_final$partner_id_x[i]
  
  # 해당 그룹의 panel_individual 중 두 명 (time 구분 X)
  rows <- which(panel_individual$group_id == group)
  
  for (r in rows) {
    id <- panel_individual$id[r]
    
    if (id == id_x) {
      panel_individual$outgoing[r] <- panel_final$outgoing[i]
      panel_individual$opened[r] <- panel_final$opened[i]
      panel_individual$agreeable[r] <- panel_final$agreeable[i]
      panel_individual$conscientious[r] <- panel_final$conscientious[i]
      panel_individual$stable[r] <- panel_final$stable[i]
      
    } else if (id == partner_id_x) {
      panel_individual$outgoing[r] <- panel_final$outgoing2[i]
      panel_individual$opened[r] <- panel_final$opened2[i]
      panel_individual$agreeable[r] <- panel_final$agreeable2[i]
      panel_individual$conscientious[r] <- panel_final$conscientious2[i]
      panel_individual$stable[r] <- panel_final$stable2[i]
    }
  }
}


save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")


###################################################################3

library(haven)
library(dplyr)
library(tidyr)

rm(list = ls())
load("../results/panel_final.RData")
network_raw <- read_dta("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/network_survey.dta")

names(panel_final)

# t == 0
popularity_t0 <- network_raw %>%
  filter(t == 0) %>%
  mutate(obj_id = as.character(obj_id)) %>%
  count(obj_id, name = "popularity")

# t == 1
popularity_t1 <- network_raw %>%
  filter(t == 1) %>%
  mutate(obj_id = as.character(obj_id)) %>%
  count(obj_id, name = "popularity_end")

panel_final <- panel_final %>%
  mutate(id_x = as.character(id_x), partner_id_x = as.character(partner_id_x)) %>% 
  
  # 학생 1 (id_x 기준)
  left_join(popularity_t0, by = c("id_x" = "obj_id")) %>%
  rename(popularity_1 = popularity) %>%
  left_join(popularity_t1, by = c("id_x" = "obj_id")) %>%
  rename(popularity_1_end = popularity_end) %>%
  
  # 학생 2 (partner_id_x 기준)
  left_join(popularity_t0, by = c("partner_id_x" = "obj_id")) %>%
  rename(popularity_2 = popularity) %>%
  left_join(popularity_t1, by = c("partner_id_x" = "obj_id")) %>%
  rename(popularity_2_end = popularity_end)

panel_final <- panel_final %>%
  mutate(across(
    c(popularity_1, popularity_1_end, popularity_2, popularity_2_end),
    ~ replace_na(., 0)
  ))

save(panel_final, file = "../results/panel_final.RData")
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")

######################################################################

library(dplyr)
library(haven)

rm(list = ls())
load("../results/panel_final.RData")
panel_individual <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

panel_individual <- panel_individual %>%
  mutate(
    n_friends = NA_real_,
    popularity = NA_real_
  )

for (i in 1:nrow(panel_final)) {
  group <- panel_final$group_id[i]
  id_x <- panel_final$id_x[i]
  partner_id_x <- panel_final$partner_id_x[i]
  
  # 해당 그룹의 두 명 (time 0, 1 모두 포함)
  rows <- which(panel_individual$group_id == group)
  
  for (r in rows) {
    id <- panel_individual$id[r]
    time <- panel_individual$time[r]
    
    if (id == id_x) {
      panel_individual$n_friends[r] <- if (time == 0) panel_final$n_friends_1[i] else panel_final$n_friends_1_end[i]
      panel_individual$popularity[r] <- if (time == 0) panel_final$popularity_1[i] else panel_final$popularity_1_end[i]
      
    } else if (id == partner_id_x) {
      panel_individual$n_friends[r] <- if (time == 0) panel_final$n_friends_2[i] else panel_final$n_friends_2_end[i]
      panel_individual$popularity[r] <- if (time == 0) panel_final$popularity_2[i] else panel_final$popularity_2_end[i]
    }
  }
}

save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")


###########################################################

library(haven)
library(dplyr)
library(tidyr)

rm(list = ls())
load("../results/panel_final.RData")
network_raw <- read_dta("C:/Users/hahn0/Desktop/Polisson et al. (2020)/Afriat/R/data/network_survey.dta")

network_raw <- network_raw %>%
  mutate(
    id = as.character(id),
    obj_id = as.character(obj_id),
    obj_class = substr(obj_id, 1, 5)  # obj_id 앞의 5자리 추출
  )

network_inclass <- network_raw %>%
  filter(class == obj_class)

# t == 0
friends_t0 <- network_inclass %>%
  filter(t == 0) %>%
  count(id, name = "n_friends")

# t == 1
friends_t1 <- network_inclass %>%
  filter(t == 1) %>%
  count(id, name = "n_friends_end")

popularity_t0 <- network_inclass %>%
  filter(t == 0) %>%
  count(obj_id, name = "popularity")

popularity_t1 <- network_inclass %>%
  filter(t == 1) %>%
  count(obj_id, name = "popularity_end")

panel_final <- panel_final %>%
  mutate(id_x = as.character(id_x), partner_id_x = as.character(partner_id_x)) %>%
  
  # 학생 1
  left_join(friends_t0, by = c("id_x" = "id")) %>%
  rename(inclass_n_friends_1 = n_friends) %>%
  left_join(friends_t1, by = c("id_x" = "id")) %>%
  rename(inclass_n_friends_1_end = n_friends_end) %>%
  
  left_join(popularity_t0, by = c("id_x" = "obj_id")) %>%
  rename(inclass_popularity_1 = popularity) %>%
  left_join(popularity_t1, by = c("id_x" = "obj_id")) %>%
  rename(inclass_popularity_1_end = popularity_end)

panel_final <- panel_final %>%
  left_join(friends_t0, by = c("partner_id_x" = "id")) %>%
  rename(inclass_n_friends_2 = n_friends) %>%
  left_join(friends_t1, by = c("partner_id_x" = "id")) %>%
  rename(inclass_n_friends_2_end = n_friends_end) %>%
  
  left_join(popularity_t0, by = c("partner_id_x" = "obj_id")) %>%
  rename(inclass_popularity_2 = popularity) %>%
  left_join(popularity_t1, by = c("partner_id_x" = "obj_id")) %>%
  rename(inclass_popularity_2_end = popularity_end)

panel_final <- panel_final %>%
  mutate(across(
    c(
      inclass_n_friends_1, inclass_n_friends_1_end,
      inclass_n_friends_2, inclass_n_friends_2_end,
      inclass_popularity_1, inclass_popularity_1_end,
      inclass_popularity_2, inclass_popularity_2_end
    ),
    ~ replace_na(., 0)
  ))


save(panel_final, file = "../results/panel_final.RData")
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")

#######################################################################

library(dplyr)
library(haven)

rm(list = ls())
load("../results/panel_final.RData")
panel_individual <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

names(panel_final)
names(panel_individual)

panel_individual <- panel_individual %>%
  mutate(
    inclass_n_friends = NA_real_,
    inclass_popularity = NA_real_
  )

for (i in 1:nrow(panel_final)) {
  group <- panel_final$group_id[i]
  id_x <- panel_final$id_x[i]
  partner_id_x <- panel_final$partner_id_x[i]
  
  # 해당 group 의 두 명 (time == 0, 1)
  rows <- which(panel_individual$group_id == group)
  
  for (r in rows) {
    id <- panel_individual$id[r]
    time <- panel_individual$time[r]
    
    if (id == id_x) {
      panel_individual$inclass_n_friends[r] <- if (time == 0) panel_final$inclass_n_friends_1[i] else panel_final$inclass_n_friends_1_end[i]
      panel_individual$inclass_popularity[r] <- if (time == 0) panel_final$inclass_popularity_1[i] else panel_final$inclass_popularity_1_end[i]
      
    } else if (id == partner_id_x) {
      panel_individual$inclass_n_friends[r] <- if (time == 0) panel_final$inclass_n_friends_2[i] else panel_final$inclass_n_friends_2_end[i]
      panel_individual$inclass_popularity[r] <- if (time == 0) panel_final$inclass_popularity_2[i] else panel_final$inclass_popularity_2_end[i]
    }
  }
}


save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

###############################################################

library(dplyr)
library(haven)

rm(list = ls())

load("../results/panel_final.RData")
panel_individual <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

panel_individual <- panel_individual %>%
  mutate(
    inclass_n_friends_i = NA_real_,
    inclass_n_friends_j = NA_real_,
    inclass_popularity_i = NA_real_,
    inclass_popularity_j = NA_real_,
    outgoing_i = NA_real_,
    outgoing_j = NA_real_,
    opened_i = NA_real_,
    opened_j = NA_real_,
    agreeable_i = NA_real_,
    agreeable_j = NA_real_,
    conscientious_i = NA_real_,
    conscientious_j = NA_real_,
    stable_i = NA_real_,
    stable_j = NA_real_
  )

for (i in 1:nrow(panel_final)) {
  group <- panel_final$group_id[i]
  id_x <- panel_final$id_x[i]
  partner_id_x <- panel_final$partner_id_x[i]
  
  rows <- which(panel_individual$group_id == group)
  
  for (r in rows) {
    id <- panel_individual$id[r]
    time <- panel_individual$time[r]
    
    suffix <- if (time == 0) "" else "_end"
    
    # friendship/popularity 는 time 구분
    if (id == id_x) {
      panel_individual$inclass_n_friends_i[r] <- panel_final[[paste0("inclass_n_friends_1", suffix)]][i]
      panel_individual$inclass_popularity_i[r] <- panel_final[[paste0("inclass_popularity_1", suffix)]][i]
      panel_individual$inclass_n_friends_j[r] <- panel_final[[paste0("inclass_n_friends_2", suffix)]][i]
      panel_individual$inclass_popularity_j[r] <- panel_final[[paste0("inclass_popularity_2", suffix)]][i]
      
      # 성격 변수는 항상 동일하게 id_x 기준
      panel_individual$outgoing_i[r] <- panel_final$outgoing[i]
      panel_individual$outgoing_j[r] <- panel_final$outgoing2[i]
      panel_individual$opened_i[r] <- panel_final$opened[i]
      panel_individual$opened_j[r] <- panel_final$opened2[i]
      panel_individual$agreeable_i[r] <- panel_final$agreeable[i]
      panel_individual$agreeable_j[r] <- panel_final$agreeable2[i]
      panel_individual$conscientious_i[r] <- panel_final$conscientious[i]
      panel_individual$conscientious_j[r] <- panel_final$conscientious2[i]
      panel_individual$stable_i[r] <- panel_final$stable[i]
      panel_individual$stable_j[r] <- panel_final$stable2[i]
      
    } else if (id == partner_id_x) {
      panel_individual$inclass_n_friends_i[r] <- panel_final[[paste0("inclass_n_friends_2", suffix)]][i]
      panel_individual$inclass_popularity_i[r] <- panel_final[[paste0("inclass_popularity_2", suffix)]][i]
      panel_individual$inclass_n_friends_j[r] <- panel_final[[paste0("inclass_n_friends_1", suffix)]][i]
      panel_individual$inclass_popularity_j[r] <- panel_final[[paste0("inclass_popularity_1", suffix)]][i]
      
      # 성격 변수는 partner 기준
      panel_individual$outgoing_i[r] <- panel_final$outgoing2[i]
      panel_individual$outgoing_j[r] <- panel_final$outgoing[i]
      panel_individual$opened_i[r] <- panel_final$opened2[i]
      panel_individual$opened_j[r] <- panel_final$opened[i]
      panel_individual$agreeable_i[r] <- panel_final$agreeable2[i]
      panel_individual$agreeable_j[r] <- panel_final$agreeable[i]
      panel_individual$conscientious_i[r] <- panel_final$conscientious2[i]
      panel_individual$conscientious_j[r] <- panel_final$conscientious[i]
      panel_individual$stable_i[r] <- panel_final$stable2[i]
      panel_individual$stable_j[r] <- panel_final$stable[i]
    }
  }
}

save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

##########################################################

library(dplyr)
library(haven)

# 1) 기존 개인 패널 불러오기
load("../results/panel_individual.RData")   # panel_individual 객체

# 2) male 변수만 가져올 panel_final 정보 준비
panel_sex <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta") %>% 
  select(group_id, id_x, male, male2)       # 딱 필요한 열만

# 3) male_j 생성
panel_individual <- panel_individual %>% 
  left_join(panel_sex, by = "group_id") %>% 
  mutate(
    male_j = if_else(id == id_x, male2, male)   # 상대방 성별
  ) %>% 
  select(-male, -male2, -id_x)                  # 불필요 열 제거

# 4) 저장
save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

####################################################################

library(haven)
library(dplyr)

# 데이터 로드
rm(list = ls())
load("../results/panel_individual.RData")
load("../results/panel_final.RData")

names(panel_final)

panel_final <- panel_final %>%
  mutate(
    # 1단계: 평균 계산
    mean_mpi_1       = (max_mpi_1 + min_mpi_1) / 2,
    mean_mpi_2       = (max_mpi_2 + min_mpi_2) / 2,
    mean_mpi_1_end   = (max_mpi_1_end + min_mpi_1_end) / 2,
    mean_mpi_2_end   = (max_mpi_2_end + min_mpi_2_end) / 2,
    mean_mpi_g       = (max_mpi_g + min_mpi_g) / 2,
    mean_mpi_g_end   = (max_mpi_g_end + min_mpi_g_end) / 2,
    
    # 2단계: 더 합리적인 쪽에 high 표시 (작거나 같은 쪽이 high)
    min_mpi_high        = if_else(min_mpi_1 <= min_mpi_2, 1L, 0L),
    max_mpi_high        = if_else(max_mpi_1 <= max_mpi_2, 1L, 0L),
    mean_mpi_high       = if_else(mean_mpi_1 <= mean_mpi_2, 1L, 0L),
    min_mpi_high_end    = if_else(min_mpi_1_end <= min_mpi_2_end, 1L, 0L),
    max_mpi_high_end    = if_else(max_mpi_1_end <= max_mpi_2_end, 1L, 0L),
    mean_mpi_high_end   = if_else(mean_mpi_1_end <= mean_mpi_2_end, 1L, 0L)
  )

# 저장
save(panel_final, file = "../results/panel_final.RData")
write_dta(panel_final, "C:/Users/hahn0/Dropbox/RP/Data/panel_final.dta")

################################################################

library(dplyr)
library(haven)

rm(list = ls())
load("../results/panel_final.RData")
panel_individual <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

# 새 변수들 초기화
panel_individual <- panel_individual %>%
  mutate(
    min_mpi_i = NA_real_, min_mpi_j = NA_real_,
    max_mpi_i = NA_real_, max_mpi_j = NA_real_,
    mean_mpi_i = NA_real_, mean_mpi_j = NA_real_,
    
    min_mpi_g = NA_real_, max_mpi_g = NA_real_, mean_mpi_g = NA_real_,
    
    min_mpi_high_ind = NA_integer_,
    max_mpi_high_ind = NA_integer_,
    mean_mpi_high_ind = NA_integer_
  )

for (i in seq_len(nrow(panel_final))) {
  group <- panel_final$group_id[i]
  id_x <- panel_final$id_x[i]
  partner_id_x <- panel_final$partner_id_x[i]
  
  rows <- which(panel_individual$group_id == group)
  
  for (r in rows) {
    id <- panel_individual$id[r]
    time <- panel_individual$time[r]
    
    if (id == id_x) {
      # 개인별 MPI
      panel_individual$min_mpi_i[r] <- if (time == 0) panel_final$min_mpi_1[i] else panel_final$min_mpi_1_end[i]
      panel_individual$max_mpi_i[r] <- if (time == 0) panel_final$max_mpi_1[i] else panel_final$max_mpi_1_end[i]
      panel_individual$mean_mpi_i[r] <- if (time == 0) panel_final$mean_mpi_1[i] else panel_final$mean_mpi_1_end[i]
      
      panel_individual$min_mpi_j[r] <- if (time == 0) panel_final$min_mpi_2[i] else panel_final$min_mpi_2_end[i]
      panel_individual$max_mpi_j[r] <- if (time == 0) panel_final$max_mpi_2[i] else panel_final$max_mpi_2_end[i]
      panel_individual$mean_mpi_j[r] <- if (time == 0) panel_final$mean_mpi_2[i] else panel_final$mean_mpi_2_end[i]
      
      # 그룹별 MPI
      panel_individual$min_mpi_g[r] <- if (time == 0) panel_final$min_mpi_g[i] else panel_final$min_mpi_g_end[i]
      panel_individual$max_mpi_g[r] <- if (time == 0) panel_final$max_mpi_g[i] else panel_final$max_mpi_g_end[i]
      panel_individual$mean_mpi_g[r] <- if (time == 0) panel_final$mean_mpi_g[i] else panel_final$mean_mpi_g_end[i]
      
      # 합리성
      panel_individual$min_mpi_high_ind[r] <- if (time == 0) panel_final$min_mpi_high[i] else panel_final$min_mpi_high_end[i]
      panel_individual$max_mpi_high_ind[r] <- if (time == 0) panel_final$max_mpi_high[i] else panel_final$max_mpi_high_end[i]
      panel_individual$mean_mpi_high_ind[r] <- if (time == 0) panel_final$mean_mpi_high[i] else panel_final$mean_mpi_high_end[i]
      
    } else if (id == partner_id_x) {
      # 반대 값
      panel_individual$min_mpi_i[r] <- if (time == 0) panel_final$min_mpi_2[i] else panel_final$min_mpi_2_end[i]
      panel_individual$max_mpi_i[r] <- if (time == 0) panel_final$max_mpi_2[i] else panel_final$max_mpi_2_end[i]
      panel_individual$mean_mpi_i[r] <- if (time == 0) panel_final$mean_mpi_2[i] else panel_final$mean_mpi_2_end[i]
      
      panel_individual$min_mpi_j[r] <- if (time == 0) panel_final$min_mpi_1[i] else panel_final$min_mpi_1_end[i]
      panel_individual$max_mpi_j[r] <- if (time == 0) panel_final$max_mpi_1[i] else panel_final$max_mpi_1_end[i]
      panel_individual$mean_mpi_j[r] <- if (time == 0) panel_final$mean_mpi_1[i] else panel_final$mean_mpi_1_end[i]
      
      # 그룹별 동일
      panel_individual$min_mpi_g[r] <- if (time == 0) panel_final$min_mpi_g[i] else panel_final$min_mpi_g_end[i]
      panel_individual$max_mpi_g[r] <- if (time == 0) panel_final$max_mpi_g[i] else panel_final$max_mpi_g_end[i]
      panel_individual$mean_mpi_g[r] <- if (time == 0) panel_final$mean_mpi_g[i] else panel_final$mean_mpi_g_end[i]
      
      # 합리성: 반대로 1/0 처리
      panel_individual$min_mpi_high_ind[r] <- if (time == 0) 1 - panel_final$min_mpi_high[i] else 1 - panel_final$min_mpi_high_end[i]
      panel_individual$max_mpi_high_ind[r] <- if (time == 0) 1 - panel_final$max_mpi_high[i] else 1 - panel_final$max_mpi_high_end[i]
      panel_individual$mean_mpi_high_ind[r] <- if (time == 0) 1 - panel_final$mean_mpi_high[i] else 1 - panel_final$mean_mpi_high_end[i]
    }
  }
}

# 저장
save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

###################################################

rm(list = ls())
library(dplyr)
library(haven)

# 현재 패널 데이터
load("../results/panel_pbl.RData")   # panel_pbl  (group_id, time = 0/1)
load("../results/panel_final.RData") # panel_final (MPI 원본)

# 1️⃣  panel_final →  long 형태의 MPI 전용 테이블 만들기
mpi_long <- bind_rows(
  
  # baseline (time = 0)
  panel_final %>%
    transmute(
      group_id,
      time = 0,
      min_mpi_1, max_mpi_1, mean_mpi_1,
      min_mpi_2, max_mpi_2, mean_mpi_2,
      min_mpi_g, max_mpi_g, mean_mpi_g
    ),
  
  # endline (time = 1)  ─ 이름만 baseline용으로 맞춰줌
  panel_final %>%
    transmute(
      group_id,
      time = 1,
      min_mpi_1  = min_mpi_1_end,
      max_mpi_1  = max_mpi_1_end,
      mean_mpi_1 = mean_mpi_1_end,
      min_mpi_2  = min_mpi_2_end,
      max_mpi_2  = max_mpi_2_end,
      mean_mpi_2 = mean_mpi_2_end,
      min_mpi_g  = min_mpi_g_end,
      max_mpi_g  = max_mpi_g_end,
      mean_mpi_g = mean_mpi_g_end
    )
)

# 2️⃣  panel_pbl ← mpi_long  LEFT JOIN  (group_id + time)
panel_pbl <- panel_pbl %>%
  left_join(mpi_long, by = c("group_id", "time"))

# 3️⃣  저장
save(panel_pbl, file = "../results/panel_pbl.RData")
write_dta(panel_pbl, "C:/Users/hahn0/Dropbox/RP/Data/panel_pbl.dta")

########################################################

library(dplyr)
library(haven)

rm(list = ls())

# 데이터 불러오기
load("../results/panel_final.RData")
panel_individual <- read_dta("C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

### 1️⃣ 기존 high 관련 변수 삭제
panel_individual <- panel_individual %>%
  select(-min_mpi_high, -max_mpi_high, -mean_mpi_high)

### 2️⃣ 새 high 변수 재정의 (기준: time 및 id_x 기준)
panel_individual <- panel_individual %>%
  mutate(
    min_mpi_high = NA_integer_,
    max_mpi_high = NA_integer_,
    mean_mpi_high = NA_integer_
  )

for (i in seq_len(nrow(panel_final))) {
  group <- panel_final$group_id[i]
  id1 <- panel_final$id_x[i]
  id2 <- panel_final$partner_id_x[i]
  
  rows <- which(panel_individual$group_id == group)
  
  for (r in rows) {
    id <- panel_individual$id[r]
    time <- panel_individual$time[r]
    is_id1 <- (id == id1)
    
    if (time == 0) {
      panel_individual$min_mpi_high[r]  <- ifelse(is_id1, panel_final$min_mpi_high[i], 1 - panel_final$min_mpi_high[i])
      panel_individual$max_mpi_high[r]  <- ifelse(is_id1, panel_final$max_mpi_high[i], 1 - panel_final$max_mpi_high[i])
      panel_individual$mean_mpi_high[r] <- ifelse(is_id1, panel_final$mean_mpi_high[i], 1 - panel_final$mean_mpi_high[i])
    } else {
      panel_individual$min_mpi_high[r]  <- ifelse(is_id1, panel_final$min_mpi_high_end[i], 1 - panel_final$min_mpi_high_end[i])
      panel_individual$max_mpi_high[r]  <- ifelse(is_id1, panel_final$max_mpi_high_end[i], 1 - panel_final$max_mpi_high_end[i])
      panel_individual$mean_mpi_high[r] <- ifelse(is_id1, panel_final$mean_mpi_high_end[i], 1 - panel_final$mean_mpi_high_end[i])
    }
  }
}

### 3️⃣ 인덱스 변수 new2_*_MPI_ig 추가 (기준: high 여부와 time)
panel_individual <- panel_individual %>%
  mutate(
    new2_min_MPI_ig = NA_real_,
    new2_max_MPI_ig = NA_real_,
    new2_mean_MPI_ig = NA_real_
  )

for (i in seq_len(nrow(panel_final))) {
  group <- panel_final$group_id[i]
  id1 <- panel_final$id_x[i]
  id2 <- panel_final$partner_id_x[i]
  
  rows <- which(panel_individual$group_id == group)
  
  for (r in rows) {
    id <- panel_individual$id[r]
    time <- panel_individual$time[r]
    is_id1 <- (id == id1)
    
    if (time == 0) {
      min_val  <- ifelse(is_id1, panel_final$new2_min_MPI_hg[i], panel_final$new2_min_MPI_lg[i])
      max_val  <- ifelse(is_id1, panel_final$new2_max_MPI_hg[i], panel_final$new2_max_MPI_lg[i])
      mean_val <- ifelse(is_id1, panel_final$new2_mean_MPI_hg[i], panel_final$new2_mean_MPI_lg[i])
    } else {
      min_val  <- ifelse(is_id1, panel_final$new2_min_MPI_hg_end[i], panel_final$new2_min_MPI_lg_end[i])
      max_val  <- ifelse(is_id1, panel_final$new2_max_MPI_hg_end[i], panel_final$new2_max_MPI_lg_end[i])
      mean_val <- ifelse(is_id1, panel_final$new2_mean_MPI_hg_end[i], panel_final$new2_mean_MPI_lg_end[i])
    }
    
    panel_individual$new2_min_MPI_ig[r]  <- min_val
    panel_individual$new2_max_MPI_ig[r]  <- max_val
    panel_individual$new2_mean_MPI_ig[r] <- mean_val
  }
}

### 💾 저장
save(panel_individual, file = "../results/panel_individual.RData")
write_dta(panel_individual, "C:/Users/hahn0/Dropbox/RP/Data/panel_individual.dta")

