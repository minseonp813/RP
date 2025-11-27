rm(list = ls())

library(dplyr)
library(haven)
library(tidyr)
library(stringr)

load("data/finalized_panel_final.RData")
load("data/finalized_panel_pbl.RData")
survey_all_long <- read_dta("survey_all_long.dta")

survey_all_long <- survey_all_long %>%
  mutate(id = as.character(id))

panel_final <- panel_final %>%
  mutate(id_x = as.character(id_x),
         partner_id_x = as.character(partner_id_x))

vars_to_merge <- c(
  "risk_q1", "risk_q2", "risk_q3",
  "peer_fair", "peer_helpful", "peer_reciprocal", 
  "peer_selfish", "peer_sociable", 
  "max_closeness", "n_closeness", "r_closeness",
  "max_betweenness", "n_betweenness", "r_betweenness",
  "max_eigenvec", "n_eigenvec", "r_eigenvec"
)

survey_wide <- survey_all_long %>%
  select(id, t, all_of(vars_to_merge)) %>%
  pivot_wider(
    id_cols   = id,
    names_from = t,
    values_from = all_of(vars_to_merge),
    names_glue = "{.value}_{t}"
  ) %>%
  rename_with(~ sub("_0$", "", .x), ends_with("_0")) %>%
  rename_with(~ sub("_1$", "_end", .x), ends_with("_1"))

add_actor_suffix <- function(df, actor_suffix = "1") {
  df %>%
    rename_with(
      ~ ifelse(
        grepl("_end$", .x),
        sub("_end$", paste0("_", actor_suffix, "_end"), .x),
        paste0(.x, "_", actor_suffix)
      ),
      -id
    )
}

panel_final <- panel_final %>%
  left_join(add_actor_suffix(survey_wide, "1"), by = c("id_x" = "id")) %>%
  left_join(add_actor_suffix(survey_wide, "2"), by = c("partner_id_x" = "id"))

save(panel_final, file = "data/finalized_panel_final.RData")
write_dta(panel_final, "data/finalized_panel_final.dta")

names(panel_final)

###############################################################

rm(list = ls()) 

library(dplyr)
library(haven)
library(tidyr)
library(stringr)

load("data/finalized_panel_final.RData") 
load("data/finalized_panel_pbl.RData")

vars_to_move <- c(
  "risk_q1", "risk_q2", "risk_q3",
  "peer_fair", "peer_helpful", "peer_reciprocal", 
  "peer_selfish", "peer_sociable", 
  "max_closeness", "n_closeness", "r_closeness",
  "max_betweenness", "n_betweenness", "r_betweenness",
  "max_eigenvec", "n_eigenvec", "r_eigenvec"
)

base_cols <- as.vector(outer(vars_to_move, c("_1","_2"), paste0))
end_cols  <- paste0(base_cols, "_end")

pf_time0 <- panel_final %>%
  select(group_id, all_of(base_cols)) %>%
  mutate(time = 0L)

pf_time1 <- panel_final %>%
  select(group_id, all_of(end_cols)) %>%
  rename_with(~ sub("_end$", "", .x), all_of(end_cols)) %>%
  mutate(time = 1L)

pf_long_by_time <- bind_rows(pf_time0, pf_time1)

panel_pbl <- panel_pbl %>%
  mutate(time = as.integer(time)) %>%
  left_join(pf_long_by_time, by = c("group_id", "time"))


save(panel_pbl, file = "data/finalized_panel_pbl.RData")
write_dta(panel_pbl, "data/finalized_panel_pbl.dta")

##############################################################

rm(list = ls()) 

library(dplyr)
library(haven)
library(tidyr)
library(stringr)

load("data/finalized_panel_final.RData") 
load("data/finalized_panel_individual.RData")

names(panel_individual)

vars_to_handle <- c(
  "risk_q1", "risk_q2", "risk_q3",
  "peer_fair", "peer_helpful", "peer_reciprocal", 
  "peer_selfish", "peer_sociable", 
  "max_closeness", "n_closeness", "r_closeness",
  "max_betweenness", "n_betweenness", "r_betweenness",
  "max_eigenvec", "n_eigenvec", "r_eigenvec"
)

make_long_for_individual <- function(df, var) {
  case1 <- df %>%
    select(group_id, id_x, partner_id_x,
           !!sym(paste0(var, "_1")),
           !!sym(paste0(var, "_2")),
           !!sym(paste0(var, "_1_end")),
           !!sym(paste0(var, "_2_end"))) %>%
    tidyr::pivot_longer(
      cols = c(paste0(var, "_1"), paste0(var, "_2"),
               paste0(var, "_1_end"), paste0(var, "_2_end")),
      names_to = "source", values_to = "val"
    ) %>%
    mutate(time = ifelse(grepl("_end$", source), 1, 0),
           role = case_when(
             source %in% c(paste0(var, "_1"), paste0(var, "_1_end")) ~ "i",
             TRUE ~ "j"
           ),
           id = id_x)
  
  case2 <- df %>%
    select(group_id, id_x, partner_id_x,
           !!sym(paste0(var, "_1")),
           !!sym(paste0(var, "_2")),
           !!sym(paste0(var, "_1_end")),
           !!sym(paste0(var, "_2_end"))) %>%
    tidyr::pivot_longer(
      cols = c(paste0(var, "_1"), paste0(var, "_2"),
               paste0(var, "_1_end"), paste0(var, "_2_end")),
      names_to = "source", values_to = "val"
    ) %>%
    mutate(time = ifelse(grepl("_end$", source), 1, 0),
           role = case_when(
             source %in% c(paste0(var, "_2"), paste0(var, "_2_end")) ~ "i",
             TRUE ~ "j"
           ),
           id = partner_id_x)
  
  out <- bind_rows(case1, case2) %>%
    select(group_id, id, time, role, val) %>%
    tidyr::pivot_wider(names_from = role, values_from = val,
                       names_prefix = paste0(var, "_"))
  
  return(out)
}

for (v in vars_to_handle) {
  long_tbl <- make_long_for_individual(panel_final, v)
  panel_individual <- panel_individual %>%
    left_join(long_tbl, by = c("group_id", "id", "time"))
}

save(panel_individual, file = "data/finalized_panel_individual.RData")
write_dta(panel_individual, "data/finalized_panel_individual.dta")

#####################################################

rm(list = ls()) 

library(dplyr)
library(haven)
library(tidyr)
library(stringr)

load("data/finalized_panel_final.RData") 
load("data/finalized_panel_pbl.RData")

panel_final <- panel_final %>%
  rename(
    mathscore_1     = mathscore,
    mathscore_2     = mathscore2,
    mathscore_1_end = mathscore_end,
    mathscore_2_end = mathscore2_end
  )

vars_to_move <- c(
  "mathscore"
)

base_cols <- as.vector(outer(vars_to_move, c("_1","_2"), paste0))
end_cols  <- paste0(base_cols, "_end")

pf_time0 <- panel_final %>%
  select(group_id, all_of(base_cols)) %>%
  mutate(time = 0L)

pf_time1 <- panel_final %>%
  select(group_id, all_of(end_cols)) %>%
  rename_with(~ sub("_end$", "", .x), all_of(end_cols)) %>%
  mutate(time = 1L)

pf_long_by_time <- bind_rows(pf_time0, pf_time1)

panel_pbl <- panel_pbl %>%
  mutate(time = as.integer(time)) %>%
  left_join(pf_long_by_time, by = c("group_id", "time"))

save(panel_pbl, file = "data/finalized_panel_pbl.RData")
write_dta(panel_pbl, "data/finalized_panel_pbl.dta")

###########################################################


# 여기서부터 수정

# value_fair value_warmglow value_trust value_cowork
# 위 변수들은 4에 가까울수록 해당 가치를 value 하는 것임

# pbl_korean pbl_eng pbl_math pbl_science pbl_socialsci
# 위 변수들은 각 과목 프로젝트 학습을 몇번 하였는지
# 3 에 가까울수록 많이 한 것. (3회 이상)

# pblclass_horizontal pblclass_horizontal_q1 pbleffect_sharidea pbleffect_helpstudy
# pbleffect_helpnonstudy pbleffect_oldfriend pbleffect_newfriend pbleffect_inclass
# pbleffect_outclass pbleffect_friend pbleffect_confidence pbleffect_school
# 위 변수들은 대충 pbl 효과를 묻는 질문들인데, 
# 4에 가까울수록 긍정적인 답변인 것.

# teacher_prepare teacher_induce
# 이 변수들은 4에 가까울수록 열심히 하는 선생님인 것

# brothers sisters
# 이게 숫자 높을수록 형제가 많은 것

rm(list = ls())

library(dplyr)
library(haven)
library(tidyr)
library(stringr)

load("data/finalized_panel_final.RData")
load("data/finalized_panel_pbl.RData")
survey_all_long <- read_dta("survey_all_long.dta")

survey_all_long <- survey_all_long %>%
  mutate(id = as.character(id))

panel_final <- panel_final %>%
  mutate(id_x = as.character(id_x),
         partner_id_x = as.character(partner_id_x))

vars_to_merge <- c(
  "value_fair", "value_warmglow", "value_trust", "value_cowork",
  "pbl_korean", "pbl_eng", "pbl_math", "pbl_science", "pbl_socialsci",
  "pblclass_horizontal", "pblclass_horizontal_q1", "pbleffect_sharidea",
  "pbleffect_helpstudy", "pbleffect_helpnonstudy", "pbleffect_oldfriend",
  "pbleffect_newfriend", "pbleffect_inclass", "pbleffect_outclass",
  "pbleffect_friend", "pbleffect_confidence", "pbleffect_school",
  "teacher_prepare", "teacher_induce",
  "brothers", "sisters"
)


survey_wide <- survey_all_long %>%
  select(id, t, all_of(vars_to_merge)) %>%
  pivot_wider(
    id_cols   = id,
    names_from = t,
    values_from = all_of(vars_to_merge),
    names_glue = "{.value}_{t}"
  ) %>%
  rename_with(~ sub("_0$", "", .x), ends_with("_0")) %>%
  rename_with(~ sub("_1$", "_end", .x), ends_with("_1"))

add_actor_suffix <- function(df, actor_suffix = "1") {
  df %>%
    rename_with(
      ~ ifelse(
        grepl("_end$", .x),
        sub("_end$", paste0("_", actor_suffix, "_end"), .x),
        paste0(.x, "_", actor_suffix)
      ),
      -id
    )
}

panel_final <- panel_final %>%
  left_join(add_actor_suffix(survey_wide, "1"), by = c("id_x" = "id")) %>%
  left_join(add_actor_suffix(survey_wide, "2"), by = c("partner_id_x" = "id"))

save(panel_final, file = "data/finalized_panel_final.RData")
write_dta(panel_final, "data/finalized_panel_final.dta")

names(panel_final)

##################################################


rm(list = ls()) 

library(dplyr)
library(haven)
library(tidyr)
library(stringr)

load("data/finalized_panel_final.RData") 
load("data/finalized_panel_pbl.RData")

vars_to_move <- c(
  "value_fair", "value_warmglow", "value_trust", "value_cowork",
  "pbl_korean", "pbl_eng", "pbl_math", "pbl_science", "pbl_socialsci",
  "pblclass_horizontal", "pblclass_horizontal_q1", "pbleffect_sharidea",
  "pbleffect_helpstudy", "pbleffect_helpnonstudy", "pbleffect_oldfriend",
  "pbleffect_newfriend", "pbleffect_inclass", "pbleffect_outclass",
  "pbleffect_friend", "pbleffect_confidence", "pbleffect_school",
  "teacher_prepare", "teacher_induce",
  "brothers", "sisters"
)

base_cols <- as.vector(outer(vars_to_move, c("_1","_2"), paste0))
end_cols  <- paste0(base_cols, "_end")

pf_time0 <- panel_final %>%
  select(group_id, all_of(base_cols)) %>%
  mutate(time = 0L)

pf_time1 <- panel_final %>%
  select(group_id, all_of(end_cols)) %>%
  rename_with(~ sub("_end$", "", .x), all_of(end_cols)) %>%
  mutate(time = 1L)

pf_long_by_time <- bind_rows(pf_time0, pf_time1)

panel_pbl <- panel_pbl %>%
  mutate(time = as.integer(time)) %>%
  left_join(pf_long_by_time, by = c("group_id", "time"))


save(panel_pbl, file = "data/finalized_panel_pbl.RData")
write_dta(panel_pbl, "data/finalized_panel_pbl.dta")

################################################

rm(list = ls()) 

library(dplyr)
library(haven)
library(tidyr)
library(stringr)

load("data/finalized_panel_final.RData") 
load("data/finalized_panel_individual.RData")

names(panel_individual)

vars_to_handle <- c(
  "value_fair", "value_warmglow", "value_trust", "value_cowork",
  "pbl_korean", "pbl_eng", "pbl_math", "pbl_science", "pbl_socialsci",
  "pblclass_horizontal", "pblclass_horizontal_q1", "pbleffect_sharidea",
  "pbleffect_helpstudy", "pbleffect_helpnonstudy", "pbleffect_oldfriend",
  "pbleffect_newfriend", "pbleffect_inclass", "pbleffect_outclass",
  "pbleffect_friend", "pbleffect_confidence", "pbleffect_school",
  "teacher_prepare", "teacher_induce",
  "brothers", "sisters"
)

make_long_for_individual <- function(df, var) {
  case1 <- df %>%
    select(group_id, id_x, partner_id_x,
           !!sym(paste0(var, "_1")),
           !!sym(paste0(var, "_2")),
           !!sym(paste0(var, "_1_end")),
           !!sym(paste0(var, "_2_end"))) %>%
    tidyr::pivot_longer(
      cols = c(paste0(var, "_1"), paste0(var, "_2"),
               paste0(var, "_1_end"), paste0(var, "_2_end")),
      names_to = "source", values_to = "val"
    ) %>%
    mutate(time = ifelse(grepl("_end$", source), 1, 0),
           role = case_when(
             source %in% c(paste0(var, "_1"), paste0(var, "_1_end")) ~ "i",
             TRUE ~ "j"
           ),
           id = id_x)
  
  case2 <- df %>%
    select(group_id, id_x, partner_id_x,
           !!sym(paste0(var, "_1")),
           !!sym(paste0(var, "_2")),
           !!sym(paste0(var, "_1_end")),
           !!sym(paste0(var, "_2_end"))) %>%
    tidyr::pivot_longer(
      cols = c(paste0(var, "_1"), paste0(var, "_2"),
               paste0(var, "_1_end"), paste0(var, "_2_end")),
      names_to = "source", values_to = "val"
    ) %>%
    mutate(time = ifelse(grepl("_end$", source), 1, 0),
           role = case_when(
             source %in% c(paste0(var, "_2"), paste0(var, "_2_end")) ~ "i",
             TRUE ~ "j"
           ),
           id = partner_id_x)
  
  out <- bind_rows(case1, case2) %>%
    select(group_id, id, time, role, val) %>%
    tidyr::pivot_wider(names_from = role, values_from = val,
                       names_prefix = paste0(var, "_"))
  
  return(out)
}

for (v in vars_to_handle) {
  long_tbl <- make_long_for_individual(panel_final, v)
  panel_individual <- panel_individual %>%
    left_join(long_tbl, by = c("group_id", "id", "time"))
}

save(panel_individual, file = "data/finalized_panel_individual.RData")
write_dta(panel_individual, "data/finalized_panel_individual.dta")