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
