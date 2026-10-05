## Byunghun Hahn, June 29
## Clean Raw Data and Save Clean Files

rm(list = ls())

library(rstudioapi)
library(readxl)
library(tidyverse)
library(dplyr)
library(haven)
library(stringr)

# Avoid namespace conflicts from packages loaded earlier in the same R session.
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
arrange <- dplyr::arrange
rename <- dplyr::rename
transmute <- dplyr::transmute
summarise <- dplyr::summarise
count <- dplyr::count
distinct <- dplyr::distinct
left_join <- dplyr::left_join
bind_rows <- dplyr::bind_rows
case_when <- dplyr::case_when
row_number <- dplyr::row_number


setwd(dirname(getSourceEditorContext()$path))


prefix_map <- c(
  "B" = "11",
  "P" = "12",
  "C" = "13",
  "F" = "14",
  "M" = "15",
  "R" = "16",
  "L" = "21",
  "Q" = "22",
  "J" = "23",
  "A" = "24",
  "K" = "25",
  "E" = "26"
)

id_as_char <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_remove(x, "\\.0$")
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  return(x)
}

convert_participant_label <- function(x) {
  x <- id_as_char(x)
  prefix <- toupper(str_sub(x, 1, 1))
  suffix <- str_sub(x, 2, -1)
  school <- unname(prefix_map[prefix])
  out <- ifelse(str_detect(x, "^[0-9]+$"), x, paste0(school, suffix))
  out <- ifelse(is.na(school) & !str_detect(x, "^[0-9]+$"), NA_character_, out)
  return(out)
}

num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

zero_to_na <- function(x) {
  x <- num(x)
  ifelse(x == 0, NA_real_, x)
}

rev4 <- function(x) {
  x <- zero_to_na(x)
  ifelse(is.na(x), NA_real_, 5 - x)
}

rev5 <- function(x) {
  x <- zero_to_na(x)
  ifelse(is.na(x), NA_real_, 6 - x)
}

school_recode <- function(x) {
  x <- zero_to_na(x)
  case_when(
    x %in% c(1, 2) ~ 1,
    x == 3 ~ 2,
    x == 4 ~ 3,
    x == 5 ~ 4,
    x %in% c(6, 7) ~ 5,
    TRUE ~ NA_real_
  )
}

row_mean_strict <- function(...) {
  rowMeans(cbind(...), na.rm = FALSE)
}

correct_or_zero <- function(x, answer) {
  ifelse(is.na(x), 0, as.numeric(x == answer))
}

count_nonmissing <- function(...) {
  rowSums(!is.na(cbind(...)))
}

first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    stop(paste0("None of these variables exist: ", paste(candidates, collapse = ", ")))
  }
  hit[1]
}

dedupe_id <- function(df, module_name, prefer_col = NULL) {
  if (!is.null(prefer_col) && prefer_col %in% names(df)) {
    df <- df %>%
      arrange(id, desc(.data[[prefer_col]]))
  }
  dup_ids <- df %>%
    filter(!is.na(id)) %>%
    count(id) %>%
    filter(n > 1)
  if (nrow(dup_ids) > 0) {
    cat("\nDuplicate ids in", module_name, "before dedupe:\n")
    print(dup_ids)
  }
  df %>%
    filter(!is.na(id), id != "") %>%
    distinct(id, .keep_all = TRUE)
}

drop_empty_target_rows <- function(df, count_col, module_name) {
  n_before <- nrow(df)
  out <- df %>%
    filter(.data[[count_col]] > 0)
  n_after <- nrow(out)
  cat("\nDropped all-empty target rows in", module_name, ":", n_before - n_after, "\n")
  return(out)
}

save_clean <- function(df, path, module_name) {
  write_dta(df, path)
  cat("\nSaved", module_name, ":", path, "\n")
  cat("Rows:", nrow(df), " Columns:", ncol(df), "\n")
}

score_math5 <- function(q1, q2, q3, q4, q5, answers) {
  correct_or_zero(q1, answers[1]) +
    correct_or_zero(q2, answers[2]) +
    correct_or_zero(q3, answers[3]) +
    correct_or_zero(q4, answers[4]) +
    correct_or_zero(q5, answers[5])
}

safe_pmax <- function(x, y) {
  out <- pmax(x, y, na.rm = TRUE)
  out[is.infinite(out)] <- NA_real_
  out
}

safe_pmin <- function(x, y) {
  out <- pmin(x, y, na.rm = TRUE)
  out[is.infinite(out)] <- NA_real_
  out
}

safe_absdiff <- function(x, y) {
  ifelse(is.na(x) | is.na(y), NA_real_, abs(x - y))
}

safe_diff <- function(x, y) {
  ifelse(is.na(x) | is.na(y), NA_real_, x - y)
}

check_row_unchanged <- function(df_before, df_after, step_name) {
  if (nrow(df_before) != nrow(df_after)) {
    stop(paste0("Row count changed in: ", step_name))
  }
}

value_or_na <- function(df, nm) {
  if (nm %in% names(df)) {
    df[[nm]]
  } else {
    rep(NA_real_, nrow(df))
  }
}


noncog_pre_raw <- read_excel("data/NonCognitive_raw_pre.xlsx")
noncog_post_raw <- read_excel("data/NonCognitive_raw_post.xlsx")
cog_pre_raw <- read_excel("data/Cognitive_raw_pre_full.xlsx")
cog_post_raw <- read_excel("data/Cognitive_raw_post.xlsx")
risksurvey_pre_raw <- read_excel("data/Risk_Survey_raw_pre_full.xlsx")
risksurvey_post_raw <- read_excel("data/Risk_Survey_raw_post.xlsx")
rat_pre_raw <- read_excel("data/RAT_raw_pre.xlsx")
rat_post_raw <- read_excel("data/RAT_raw_post.xlsx")
network_raw <- read_dta("data/network_survey.dta")
male_raw <- read_dta("data/male.dta")


noncog_pre_clean <- noncog_pre_raw %>%
  mutate(
    id = id_as_char(.data[["id_new"]]),
    class_sleep_base = zero_to_na(.data[["Player.q2"]]),
    class_participate_base = zero_to_na(.data[["Player.q3"]]),
    teacher_prepare_base = zero_to_na(.data[["Player.q4"]]),
    teacher_induce_base = zero_to_na(.data[["Player.q5"]]),
    class_study_base = zero_to_na(.data[["Player.q6"]]),
    class_dislike_base = zero_to_na(.data[["Player.q7"]]),
    class_practical_base = zero_to_na(.data[["Player.q8"]]),
    class_sociable_base = zero_to_na(.data[["Player.q9"]]),
    class_belonged_base = zero_to_na(.data[["Player.q10"]]),
    class_lonely_base = zero_to_na(.data[["Player.q11"]]),
    peer_sociable_base = rev4(.data[["Player.q12"]]),
    peer_fair_base = rev4(.data[["Player.q13"]]),
    peer_helpful_base = rev4(.data[["Player.q14"]]),
    peer_selfish_base = rev4(.data[["Player.q15"]]),
    peer_reciprocal_base = rev4(.data[["Player.q16"]]),
    class_outcast_base = zero_to_na(.data[["Player.q17"]]),
    class_harass_base = zero_to_na(.data[["Player.q18"]]),
    value_fair_base = zero_to_na(.data[["Player.q19"]]),
    value_warmglow_base = zero_to_na(.data[["Player.q20"]]),
    value_trust_base = zero_to_na(.data[["Player.q21"]]),
    value_cowork_base = zero_to_na(.data[["Player.q22"]]),
    selfesteem_1_base = zero_to_na(.data[["Player.q23"]]),
    selfesteem_2_base = zero_to_na(.data[["Player.q24"]]),
    selfesteem_3_base = rev4(.data[["Player.q25"]]),
    selfesteem_4_base = zero_to_na(.data[["Player.q26"]]),
    selfesteem_5_base = rev4(.data[["Player.q27"]]),
    selfesteem_6_base = zero_to_na(.data[["Player.q28"]]),
    selfesteem_7_base = zero_to_na(.data[["Player.q29"]]),
    selfesteem_8_base = rev4(.data[["Player.q30"]]),
    selfesteem_9_base = rev4(.data[["Player.q31"]]),
    selfesteem_10_base = rev4(.data[["Player.q32"]]),
    selfesteem_base = row_mean_strict(
      selfesteem_1_base,
      selfesteem_2_base,
      selfesteem_3_base,
      selfesteem_4_base,
      selfesteem_5_base,
      selfesteem_6_base,
      selfesteem_7_base,
      selfesteem_8_base,
      selfesteem_9_base,
      selfesteem_10_base
    ),
    outgoing_base = row_mean_strict(
      zero_to_na(.data[["Player.q33"]]),
      rev5(.data[["Player.q38"]])
    ),
    agreeable_base = row_mean_strict(
      rev5(.data[["Player.q34"]]),
      zero_to_na(.data[["Player.q39"]])
    ),
    conscientious_base = row_mean_strict(
      zero_to_na(.data[["Player.q35"]]),
      rev5(.data[["Player.q40"]])
    ),
    stable_base = row_mean_strict(
      rev5(.data[["Player.q36"]]),
      zero_to_na(.data[["Player.q41"]])
    ),
    opened_base = row_mean_strict(
      zero_to_na(.data[["Player.q37"]]),
      rev5(.data[["Player.q42"]])
    ),
    pblclass_horizontal_1_base = zero_to_na(.data[["Player.q43"]]),
    pblclass_horizontal_2_base = zero_to_na(.data[["Player.q44"]]),
    pblclass_horizontal_3_base = rev4(.data[["Player.q45"]]),
    pblclass_horizontal_base = row_mean_strict(
      pblclass_horizontal_1_base,
      pblclass_horizontal_2_base,
      pblclass_horizontal_3_base
    ),
    pblclass_horizontal_q1_base = pblclass_horizontal_1_base,
    pbl_korean_base = zero_to_na(.data[["Player.q46"]]),
    pbl_eng_base = zero_to_na(.data[["Player.q47"]]),
    pbl_math_base = zero_to_na(.data[["Player.q48"]]),
    pbl_science_base = zero_to_na(.data[["Player.q49"]]),
    pbl_socialsci_base = zero_to_na(.data[["Player.q50"]]),
    pbleffect_inclass_1_base = zero_to_na(.data[["Player.q51"]]),
    pbleffect_inclass_2_base = rev4(.data[["Player.q52"]]),
    pbleffect_inclass_3_base = rev4(.data[["Player.q53"]]),
    pbleffect_inclass_base = row_mean_strict(
      pbleffect_inclass_1_base,
      pbleffect_inclass_2_base,
      pbleffect_inclass_3_base
    ),
    pbleffect_sharidea_base = pbleffect_inclass_1_base,
    pbleffect_helpstudy_base = zero_to_na(.data[["Player.q54"]]),
    pbleffect_helpnonstudy_base = zero_to_na(.data[["Player.q55"]]),
    pbleffect_outclass_base = row_mean_strict(
      pbleffect_helpstudy_base,
      pbleffect_helpnonstudy_base
    ),
    pbleffect_oldfriend_base = zero_to_na(.data[["Player.q56"]]),
    pbleffect_newfriend_base = zero_to_na(.data[["Player.q57"]]),
    pbleffect_friend_base = row_mean_strict(
      pbleffect_oldfriend_base,
      pbleffect_newfriend_base
    ),
    pbleffect_confidence_base = zero_to_na(.data[["Player.q59"]]),
    pbleffect_school_base = zero_to_na(.data[["Player.q61"]]),
    lifesatisfied_base = num(.data[["Player.q62"]])
  ) %>%
  select(
    id,
    class_sleep_base,
    class_participate_base,
    teacher_prepare_base,
    teacher_induce_base,
    class_study_base,
    class_dislike_base,
    class_practical_base,
    class_sociable_base,
    class_belonged_base,
    class_lonely_base,
    peer_sociable_base,
    peer_fair_base,
    peer_helpful_base,
    peer_selfish_base,
    peer_reciprocal_base,
    class_outcast_base,
    class_harass_base,
    value_fair_base,
    value_warmglow_base,
    value_trust_base,
    value_cowork_base,
    selfesteem_1_base,
    selfesteem_2_base,
    selfesteem_3_base,
    selfesteem_4_base,
    selfesteem_5_base,
    selfesteem_6_base,
    selfesteem_7_base,
    selfesteem_8_base,
    selfesteem_9_base,
    selfesteem_10_base,
    selfesteem_base,
    outgoing_base,
    agreeable_base,
    conscientious_base,
    stable_base,
    opened_base,
    pblclass_horizontal_1_base,
    pblclass_horizontal_2_base,
    pblclass_horizontal_3_base,
    pblclass_horizontal_base,
    pblclass_horizontal_q1_base,
    pbl_korean_base,
    pbl_eng_base,
    pbl_math_base,
    pbl_science_base,
    pbl_socialsci_base,
    pbleffect_inclass_1_base,
    pbleffect_inclass_2_base,
    pbleffect_inclass_3_base,
    pbleffect_inclass_base,
    pbleffect_sharidea_base,
    pbleffect_helpstudy_base,
    pbleffect_helpnonstudy_base,
    pbleffect_outclass_base,
    pbleffect_oldfriend_base,
    pbleffect_newfriend_base,
    pbleffect_friend_base,
    pbleffect_confidence_base,
    pbleffect_school_base,
    lifesatisfied_base
  ) %>%
  dedupe_id("noncog_pre_clean")


male_clean <- male_raw %>%
  transmute(
    id = id_as_char(.data[["id"]]),
    male = case_when(
      num(.data[["male"]]) == 1 ~ 1,
      num(.data[["male"]]) == 2 ~ 0,
      num(.data[["male"]]) == 0 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(id), id != "") %>%
  dedupe_id("male_clean")

noncog_post_clean <- noncog_post_raw %>%
  mutate(
    id = convert_participant_label(.data[["participant.label"]]),
    noncog_post_n_nonmissing = rowSums(
      sapply(
        dplyr::select(cur_data(), starts_with("player.q")),
        function(x) {
          x <- str_trim(as.character(x))
          !is.na(x) & x != ""
        }
      )
    ),
    pblclass_horizontal_1_end = zero_to_na(.data[["player.q1"]]),
    pblclass_horizontal_2_end = zero_to_na(.data[["player.q2"]]),
    pblclass_horizontal_3_end = rev4(.data[["player.q3"]]),
    pblclass_horizontal_end = row_mean_strict(
      pblclass_horizontal_1_end,
      pblclass_horizontal_2_end,
      pblclass_horizontal_3_end
    ),
    pblclass_horizontal_q1_end = pblclass_horizontal_1_end,
    pbl_korean_end = zero_to_na(.data[["player.q4"]]),
    pbl_eng_end = zero_to_na(.data[["player.q5"]]),
    pbl_math_end = zero_to_na(.data[["player.q6"]]),
    pbl_science_end = zero_to_na(.data[["player.q7"]]),
    pbl_socialsci_end = zero_to_na(.data[["player.q8"]]),
    pbleffect_inclass_1_end = zero_to_na(.data[["player.q9"]]),
    pbleffect_inclass_2_end = rev4(.data[["player.q10"]]),
    pbleffect_inclass_3_end = rev4(.data[["player.q11"]]),
    pbleffect_inclass_end = row_mean_strict(
      pbleffect_inclass_1_end,
      pbleffect_inclass_2_end,
      pbleffect_inclass_3_end
    ),
    pbleffect_sharidea_end = pbleffect_inclass_1_end,
    pbleffect_helpstudy_end = zero_to_na(.data[["player.q12"]]),
    pbleffect_helpnonstudy_end = zero_to_na(.data[["player.q13"]]),
    pbleffect_outclass_end = row_mean_strict(
      pbleffect_helpstudy_end,
      pbleffect_helpnonstudy_end
    ),
    pbleffect_oldfriend_end = zero_to_na(.data[["player.q14"]]),
    pbleffect_newfriend_end = zero_to_na(.data[["player.q15"]]),
    pbleffect_friend_end = row_mean_strict(
      pbleffect_oldfriend_end,
      pbleffect_newfriend_end
    ),
    pbleffect_confidence_end = zero_to_na(.data[["player.q16"]]),
    pbleffect_school_end = zero_to_na(.data[["player.q17"]]),
    brothers_end = num(.data[["player.q19_1"]]),
    sisters_end = num(.data[["player.q19_2"]]),
    birthorder_end = zero_to_na(.data[["player.q19_3"]]),
    fatherage_end = zero_to_na(.data[["player.q20_1"]]),
    motherage_end = zero_to_na(.data[["player.q20_2"]]),
    fathersch_end = school_recode(.data[["player.q21_1"]]),
    mothersch_end = school_recode(.data[["player.q21_2"]]),
    schchoice1_str_end = as.character(.data[["player.q22_1"]]),
    schchoice2_str_end = as.character(.data[["player.q22_2"]]),
    giftcard_end = zero_to_na(.data[["player.q23"]]),
    class_sleep_end = zero_to_na(.data[["player.q24"]]),
    class_participate_end = zero_to_na(.data[["player.q25"]]),
    class_practical_end = zero_to_na(.data[["player.q26"]]),
    teacher_prepare_end = zero_to_na(.data[["player.q27"]]),
    teacher_induce_end = zero_to_na(.data[["player.q28"]]),
    class_sociable_end = zero_to_na(.data[["player.q29"]]),
    class_belonged_end = zero_to_na(.data[["player.q30"]]),
    peer_sociable_end = rev4(.data[["player.q31"]]),
    peer_fair_end = rev4(.data[["player.q32"]]),
    peer_helpful_end = rev4(.data[["player.q33"]]),
    peer_selfish_end = rev4(.data[["player.q34"]]),
    peer_reciprocal_end = rev4(.data[["player.q35"]]),
    class_outcast_end = zero_to_na(.data[["player.q36"]]),
    class_harass_end = zero_to_na(.data[["player.q37"]]),
    value_fair_end = zero_to_na(.data[["player.q38"]]),
    value_warmglow_end = zero_to_na(.data[["player.q39"]]),
    value_trust_end = zero_to_na(.data[["player.q40"]]),
    value_cowork_end = zero_to_na(.data[["player.q41"]]),
    lifesatisfied_end = num(.data[["player.q42"]])
  ) %>%
  select(
    id,
    noncog_post_n_nonmissing,
    pblclass_horizontal_1_end,
    pblclass_horizontal_2_end,
    pblclass_horizontal_3_end,
    pblclass_horizontal_end,
    pblclass_horizontal_q1_end,
    pbl_korean_end,
    pbl_eng_end,
    pbl_math_end,
    pbl_science_end,
    pbl_socialsci_end,
    pbleffect_inclass_1_end,
    pbleffect_inclass_2_end,
    pbleffect_inclass_3_end,
    pbleffect_inclass_end,
    pbleffect_sharidea_end,
    pbleffect_helpstudy_end,
    pbleffect_helpnonstudy_end,
    pbleffect_outclass_end,
    pbleffect_oldfriend_end,
    pbleffect_newfriend_end,
    pbleffect_friend_end,
    pbleffect_confidence_end,
    pbleffect_school_end,
    brothers_end,
    sisters_end,
    birthorder_end,
    fatherage_end,
    motherage_end,
    fathersch_end,
    mothersch_end,
    schchoice1_str_end,
    schchoice2_str_end,
    giftcard_end,
    class_sleep_end,
    class_participate_end,
    class_practical_end,
    teacher_prepare_end,
    teacher_induce_end,
    class_sociable_end,
    class_belonged_end,
    peer_sociable_end,
    peer_fair_end,
    peer_helpful_end,
    peer_selfish_end,
    peer_reciprocal_end,
    class_outcast_end,
    class_harass_end,
    value_fair_end,
    value_warmglow_end,
    value_trust_end,
    value_cowork_end,
    lifesatisfied_end
  ) %>%
  dedupe_id("noncog_post_clean", prefer_col = "noncog_post_n_nonmissing") %>%
  select(-noncog_post_n_nonmissing)


cog_pre_clean <- cog_pre_raw %>%
  mutate(
    id = convert_participant_label(.data[["label"]]),
    cq1 = zero_to_na(.data[["cq1"]]),
    cq2 = zero_to_na(.data[["cq2"]]),
    cq3 = zero_to_na(.data[["cq3"]]),
    cq4 = zero_to_na(.data[["cq4"]]),
    cq5 = zero_to_na(.data[["cq5"]]),
    n_cq_nonmissing = count_nonmissing(cq1, cq2, cq3, cq4, cq5)
  ) %>%
  drop_empty_target_rows(
    count_col = "n_cq_nonmissing",
    module_name = "cog_pre_clean"
  ) %>%
  mutate(
    mathscore_base = score_math5(
      cq1,
      cq2,
      cq3,
      cq4,
      cq5,
      answers = c(2, 4, 3, 5, 1)
    )
  ) %>%
  select(id, mathscore_base, n_cq_nonmissing) %>%
  dedupe_id("cog_pre_clean", prefer_col = "n_cq_nonmissing") %>%
  select(id, mathscore_base)

cog_post_clean <- cog_post_raw %>%
  mutate(
    id = convert_participant_label(.data[["participant.label"]]),
    cq1 = zero_to_na(.data[["player.cq1"]]),
    cq2 = zero_to_na(.data[["player.cq2"]]),
    cq3 = zero_to_na(.data[["player.cq3"]]),
    cq4 = zero_to_na(.data[["player.cq4"]]),
    cq5 = zero_to_na(.data[["player.cq5"]]),
    n_cq_nonmissing = count_nonmissing(cq1, cq2, cq3, cq4, cq5)
  ) %>%
  drop_empty_target_rows(
    count_col = "n_cq_nonmissing",
    module_name = "cog_post_clean"
  ) %>%
  mutate(
    mathscore_end = score_math5(
      cq1,
      cq2,
      cq3,
      cq4,
      cq5,
      answers = c(3, 1, 5, 2, 5)
    )
  ) %>%
  select(id, mathscore_end, n_cq_nonmissing) %>%
  dedupe_id("cog_post_clean", prefer_col = "n_cq_nonmissing") %>%
  select(id, mathscore_end)


risksurvey_pre_clean <- risksurvey_pre_raw %>%
  mutate(
    id = convert_participant_label(.data[["label"]]),
    risk_q1 = zero_to_na(.data[["Risk_q1"]]),
    risk_q2 = zero_to_na(.data[["Risk_q2"]]),
    risk_q3 = zero_to_na(.data[["Risk_q3"]]),
    n_risk_nonmissing = count_nonmissing(risk_q1, risk_q2, risk_q3)
  ) %>%
  drop_empty_target_rows(
    count_col = "n_risk_nonmissing",
    module_name = "risksurvey_pre_clean"
  ) %>%
  mutate(
    risk_cooperation_base = ifelse(is.na(risk_q1), NA_real_, 6 - risk_q1),
    risk_similar_base = risk_q2,
    risk_whose_base = risk_q3
  ) %>%
  select(id, risk_cooperation_base, risk_similar_base, risk_whose_base, n_risk_nonmissing) %>%
  dedupe_id("risksurvey_pre_clean", prefer_col = "n_risk_nonmissing") %>%
  select(id, risk_cooperation_base, risk_similar_base, risk_whose_base)

risksurvey_post_clean <- risksurvey_post_raw %>%
  mutate(
    id = convert_participant_label(.data[["label"]]),
    risk_q1 = zero_to_na(.data[["Risk_q1"]]),
    risk_q2 = zero_to_na(.data[["Risk_q2"]]),
    risk_q3 = zero_to_na(.data[["Risk_q3"]]),
    n_risk_nonmissing = count_nonmissing(risk_q1, risk_q2, risk_q3)
  ) %>%
  drop_empty_target_rows(
    count_col = "n_risk_nonmissing",
    module_name = "risksurvey_post_clean"
  ) %>%
  mutate(
    risk_cooperation_end = ifelse(is.na(risk_q1), NA_real_, 6 - risk_q1),
    risk_similar_end = risk_q2,
    risk_whose_end = risk_q3
  ) %>%
  select(id, risk_cooperation_end, risk_similar_end, risk_whose_end, n_risk_nonmissing) %>%
  dedupe_id("risksurvey_post_clean", prefer_col = "n_risk_nonmissing") %>%
  select(id, risk_cooperation_end, risk_similar_end, risk_whose_end)


score_rat_clean <- function(df, answers, suffix, module_name) {
  id_col <- first_existing(
    df,
    c("Participant.label", "participant.label", "Participantlabel", "participantlabel", "raw_id")
  )
  rat_cols <- map_chr(
    1:10,
    function(i) {
      first_existing(
        df,
        c(
          paste0("player.RAT", i),
          paste0("Player.RAT", i),
          paste0("player.RAT", sprintf("%02d", i)),
          paste0("Player.RAT", sprintf("%02d", i)),
          paste0("RAT", i),
          paste0("RAT", sprintf("%02d", i))
        )
      )
    }
  )
  gen_scores <- list()
  strict_scores <- list()
  for (i in 1:10) {
    response <- str_trim(as.character(df[[rat_cols[i]]]))
    answer_i <- answers[i]
    gen_scores[[i]] <- ifelse(
      is.na(response) | response == "",
      0L,
      as.integer(str_detect(response, fixed(answer_i)))
    )
    strict_scores[[i]] <- ifelse(
      is.na(response) | response == "",
      0L,
      as.integer(response == answer_i)
    )
  }
  gen_mat <- as_tibble(setNames(gen_scores, paste0("RAT", 1:10, "_gen")))
  strict_mat <- as_tibble(setNames(strict_scores, paste0("RAT", 1:10, "_strict")))
  out <- tibble(
    id = convert_participant_label(df[[id_col]]),
    RAT_n_nonmissing = rowSums(
      sapply(
        rat_cols,
        function(x) {
          response <- str_trim(as.character(df[[x]]))
          !is.na(response) & response != ""
        }
      )
    ),
    !!paste0("RAT_generous_", suffix) := rowSums(gen_mat, na.rm = TRUE),
    !!paste0("RAT_strict_", suffix) := rowSums(strict_mat, na.rm = TRUE)
  ) %>%
    dedupe_id(module_name, prefer_col = "RAT_n_nonmissing") %>%
    select(-RAT_n_nonmissing)
  return(out)
}

rat_pre_answers <- c(
  "우유", "단풍", "장미", "고양이", "장갑",
  "호박", "오리", "피리", "떡", "구름"
)

rat_post_answers <- c(
  "참새", "곰", "악어", "진주", "무지개",
  "제비", "솥", "별", "단추", "코끼리"
)

rat_pre_clean <- score_rat_clean(
  df = rat_pre_raw,
  answers = rat_pre_answers,
  suffix = "base",
  module_name = "rat_pre_clean"
)

rat_post_clean <- score_rat_clean(
  df = rat_post_raw,
  answers = rat_post_answers,
  suffix = "end",
  module_name = "rat_post_clean"
)


make_network_all <- function(network_raw) {
  network_raw %>%
    transmute(
      id = id_as_char(.data[["id"]]),
      obj_id = id_as_char(.data[["obj_id"]]),
      t = as.integer(num(.data[["t"]]))
    ) %>%
    filter(
      t %in% c(0, 1),
      !is.na(id),
      id != ""
    ) %>%
    mutate(
      id_class = substr(id, 1, 5),
      obj_class = substr(obj_id, 1, 5),
      inclass = as.integer(!is.na(obj_id) & obj_id != "" & id_class == obj_class)
    )
}

make_network_edges <- function(network_all) {
  network_all %>%
    filter(
      !is.na(obj_id),
      obj_id != "",
      id != obj_id
    ) %>%
    distinct(t, id, obj_id, .keep_all = TRUE) %>%
    mutate(friend_nomination = 1L)
}

make_network_degree <- function(network_all, network_edges, t_value, suffix = NULL) {
  id_list <- bind_rows(
    network_all %>%
      filter(t == t_value) %>%
      select(id),
    network_edges %>%
      filter(t == t_value) %>%
      select(id = obj_id)
  ) %>%
    distinct(id) %>%
    filter(!is.na(id), id != "")
  
  all_outdegree_df <- network_edges %>%
    filter(t == t_value) %>%
    group_by(id) %>%
    summarise(all_friends = n_distinct(obj_id), .groups = "drop")
  
  inclass_outdegree_df <- network_edges %>%
    filter(t == t_value, inclass == 1) %>%
    group_by(id) %>%
    summarise(inclass_friends = n_distinct(obj_id), .groups = "drop")
  
  all_indegree_df <- network_edges %>%
    filter(t == t_value) %>%
    group_by(obj_id) %>%
    summarise(all_popularity = n_distinct(id), .groups = "drop") %>%
    rename(id = obj_id)
  
  inclass_indegree_df <- network_edges %>%
    filter(t == t_value, inclass == 1) %>%
    group_by(obj_id) %>%
    summarise(inclass_popularity = n_distinct(id), .groups = "drop") %>%
    rename(id = obj_id)
  
  out <- id_list %>%
    left_join(all_outdegree_df, by = "id") %>%
    left_join(inclass_outdegree_df, by = "id") %>%
    left_join(all_indegree_df, by = "id") %>%
    left_join(inclass_indegree_df, by = "id") %>%
    mutate(
      all_friends = ifelse(is.na(all_friends), 0L, as.integer(all_friends)),
      inclass_friends = ifelse(is.na(inclass_friends), 0L, as.integer(inclass_friends)),
      all_popularity = ifelse(is.na(all_popularity), 0L, as.integer(all_popularity)),
      inclass_popularity = ifelse(is.na(inclass_popularity), 0L, as.integer(inclass_popularity))
    )
  
  if (!is.null(suffix)) {
    out <- out %>%
      rename(
        !!paste0("all_friends_", suffix) := all_friends,
        !!paste0("inclass_friends_", suffix) := inclass_friends,
        !!paste0("all_popularity_", suffix) := all_popularity,
        !!paste0("inclass_popularity_", suffix) := inclass_popularity
      )
  }
  
  return(out)
}

network_all <- make_network_all(network_raw)
network_edges <- make_network_edges(network_all)

network_pre_clean <- make_network_degree(
  network_all = network_all,
  network_edges = network_edges,
  t_value = 0,
  suffix = NULL
)

network_post_clean <- make_network_degree(
  network_all = network_all,
  network_edges = network_edges,
  t_value = 1,
  suffix = NULL
)


save_clean(noncog_pre_clean, "data/noncog_pre_clean.dta", "noncog_pre_clean")
save_clean(male_clean, "data/male_clean.dta", "male_clean")
save_clean(noncog_post_clean, "data/noncog_post_clean.dta", "noncog_post_clean")

cat("\nCheck 11106XX noncog_post_clean after dedupe:\n")
noncog_post_clean %>%
  filter(str_detect(id, "^11106")) %>%
  select(id, brothers_end, sisters_end) %>%
  arrange(id) %>%
  print(n = Inf, width = Inf)

save_clean(cog_pre_clean, "data/cog_pre_clean.dta", "cog_pre_clean")
save_clean(cog_post_clean, "data/cog_post_clean.dta", "cog_post_clean")
save_clean(risksurvey_pre_clean, "data/risksurvey_pre_clean.dta", "risksurvey_pre_clean")
save_clean(risksurvey_post_clean, "data/risksurvey_post_clean.dta", "risksurvey_post_clean")
save_clean(rat_pre_clean, "data/rat_pre_clean.dta", "rat_pre_clean")
save_clean(rat_post_clean, "data/rat_post_clean.dta", "rat_post_clean")
save_clean(network_pre_clean, "data/network_pre_clean.dta", "network_pre_clean")
save_clean(network_post_clean, "data/network_post_clean.dta", "network_post_clean")

clean_file_summary <- tibble(
  file = c(
    "noncog_pre_clean",
    "noncog_post_clean",
    "cog_pre_clean",
    "cog_post_clean",
    "risksurvey_pre_clean",
    "risksurvey_post_clean",
    "rat_pre_clean",
    "rat_post_clean",
    "network_pre_clean",
    "network_post_clean"
  ),
  n_rows = c(
    nrow(noncog_pre_clean),
    nrow(noncog_post_clean),
    nrow(cog_pre_clean),
    nrow(cog_post_clean),
    nrow(risksurvey_pre_clean),
    nrow(risksurvey_post_clean),
    nrow(rat_pre_clean),
    nrow(rat_post_clean),
    nrow(network_pre_clean),
    nrow(network_post_clean)
  ),
  n_cols = c(
    ncol(noncog_pre_clean),
    ncol(noncog_post_clean),
    ncol(cog_pre_clean),
    ncol(cog_post_clean),
    ncol(risksurvey_pre_clean),
    ncol(risksurvey_post_clean),
    ncol(rat_pre_clean),
    ncol(rat_post_clean),
    ncol(network_pre_clean),
    ncol(network_post_clean)
  )
)

print(clean_file_summary, width = Inf)
