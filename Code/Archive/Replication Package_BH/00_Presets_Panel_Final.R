# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("rstudioapi")

rm(list = ls())

library(rstudioapi)
library(readxl)
library(tidyverse)
library(dplyr)

setwd(dirname(getSourceEditorContext()$path))

base_raw <- read_excel("data/Risk_0907.xlsx")
end_raw  <- read_csv("data/risk_preference_solo_pair.csv")

base_raw <- base_raw %>%
  mutate(
    mover = ifelse(mover == "t", 1L, 0L)
  )

end_raw <- end_raw %>%
  filter(!is.na(player.coord_x) & player.coord_x != "")

end_raw <- end_raw %>%
  group_by(participant.label) %>%
  mutate(
    partner_id = player.partner[subsession.round_number == 19][1]
  ) %>%
  ungroup()

replace_map <- c(
  "B"="11","P"="12","C"="13","F"="14",
  "M"="15","R"="16","L"="21","Q"="22",
  "J"="23","A"="24","K"="25","E"="26"
)

convert_label <- function(x) {
  if (is.na(x) | x == "") return(NA_character_)
  prefix <- substr(x, 1, 1)
  suffix <- substr(x, 2, nchar(x))
  if (!prefix %in% names(replace_map)) return(NA_character_)
  paste0(replace_map[prefix], suffix)
}

end_raw <- end_raw %>%
  mutate(
    id_num      = sapply(participant.label, convert_label),
    partner_num = sapply(partner_id, convert_label)
  ) %>%
  filter(!is.na(id_num)) %>%
  mutate(
    big_id   = pmax(id_num, partner_num, na.rm = TRUE),
    small_id = pmin(id_num, partner_num, na.rm = TRUE),
    group_id = ifelse(
      !is.na(partner_num) & id_num != partner_num,
      paste0(big_id, small_id),
      NA_character_
    )
  )

pair_map <- end_raw %>%
  filter(
    subsession.round_number == 1,
    !is.na(group_id),
    nchar(group_id) == 14
  ) %>%
  distinct(
    big_id,
    small_id,
    group_id
  )

end_raw <- end_raw %>%
  select(
    id            = id_num,
    round_number  = subsession.round_number,
    coord_y       = player.coord_y,
    coord_x       = player.coord_x,
    intercept_y   = player.intercept_y,
    intercept_x   = player.intercept_x,
    partner_id    = partner_num,
    game_type     = subsession.game_type,
    mover         = player.is_mover,
    group_id
  )

end_raw <- end_raw %>%
  rename(old_group_id = group_id)

end_raw <- end_raw %>%
  left_join(
    pair_map,
    by = c("id" = "big_id")
  ) %>%
  mutate(
    new_group_from_big = group_id,
    partner_from_big   = small_id
  ) %>%
  select(-group_id, -small_id)

end_raw <- end_raw %>%
  left_join(
    pair_map,
    by = c("id" = "small_id")
  ) %>%
  mutate(
    new_group_from_small = group_id,
    partner_from_small   = big_id
  ) %>%
  select(-group_id, -big_id)

end_raw <- end_raw %>%
  mutate(
    group_id = coalesce(new_group_from_big, new_group_from_small),
    partner_id = coalesce(partner_from_big, partner_from_small)
  ) %>%
  select(
    -new_group_from_big,
    -new_group_from_small,
    -partner_from_big,
    -partner_from_small
  )

end_raw <- end_raw %>%
  filter(!is.na(group_id))

end_raw <- end_raw %>%
  select(-old_group_id) %>%
  
  group_by(id) %>%
  mutate(
    mover = mover[round_number == 19][1]
  ) %>%
  ungroup() %>%
  
  arrange(id, round_number)

colnames(end_raw)

end_raw %>%
  count(id, name = "n_rows") %>%
  count(n_rows, name = "n_ids")

end_raw %>%
  count(id, name = "n_rows") %>%
  filter(n_rows > 36)

bad_ids <- end_raw %>%
  count(id, name = "n_rows") %>%
  filter(n_rows > 36) %>%
  pull(id)

bad_groups <- end_raw %>%
  filter(id %in% bad_ids) %>%
  distinct(group_id) %>%
  pull(group_id)

end_raw <- end_raw %>%
  filter(!group_id %in% bad_groups)






base_raw <- base_raw %>%
  select(
    id            = id_new,
    round_number,
    coord_x,
    coord_y,
    intercept_x,
    intercept_y,
    mover,
    game_type,
    partner_obs   = partner_newid
  )

base_raw <- base_raw %>%
  group_by(id) %>%
  mutate(
    partner_id = partner_obs[round_number == 19][1]
  ) %>%
  ungroup()

base_raw <- base_raw %>%
  mutate(
    id_num      = as.character(id),
    partner_num = as.character(partner_id)
  ) %>%
  mutate(
    big_id   = pmax(id_num, partner_num, na.rm = TRUE),
    small_id = pmin(id_num, partner_num, na.rm = TRUE),
    group_id = ifelse(
      !is.na(partner_num) & id_num != partner_num,
      paste0(big_id, small_id),
      NA_character_
    )
  )

pair_map_base <- base_raw %>%
  filter(
    round_number == 1,
    !is.na(group_id),
    nchar(group_id) == 14
  ) %>%
  distinct(
    big_id,
    small_id,
    group_id
  )

base_raw <- base_raw %>%
  select(
    id            = id_num,
    round_number,
    coord_y,
    coord_x,
    intercept_y,
    intercept_x,
    partner_id    = partner_num,
    game_type,
    mover,
    group_id
  ) %>%
  rename(old_group_id = group_id)

base_raw <- base_raw %>%
  left_join(
    pair_map_base,
    by = c("id" = "big_id")
  ) %>%
  mutate(
    new_group_from_big = group_id,
    partner_from_big   = small_id
  ) %>%
  select(-group_id, -small_id)

base_raw <- base_raw %>%
  left_join(
    pair_map_base,
    by = c("id" = "small_id")
  ) %>%
  mutate(
    new_group_from_small = group_id,
    partner_from_small   = big_id
  ) %>%
  select(-group_id, -big_id)

base_raw <- base_raw %>%
  mutate(
    group_id   = coalesce(new_group_from_big, new_group_from_small),
    partner_id = coalesce(partner_from_big, partner_from_small)
  ) %>%
  select(
    -new_group_from_big,
    -new_group_from_small,
    -partner_from_big,
    -partner_from_small
  )

base_raw <- base_raw %>%
  filter(!is.na(group_id)) %>%
  select(-old_group_id) %>%
  group_by(id) %>%
  mutate(
    mover = mover[round_number == 19][1]
  ) %>%
  ungroup() %>%
  arrange(id, round_number)

bad_ids_base <- base_raw %>%
  count(id, name = "n_rows") %>%
  filter(n_rows != 36) %>%
  pull(id)

bad_groups_base <- base_raw %>%
  filter(id %in% bad_ids_base) %>%
  distinct(group_id) %>%
  pull(group_id)

base_raw <- base_raw %>%
  filter(!group_id %in% bad_groups_base)

save(
  base_raw,
  file = "data/base_raw.RData"
)

save(
  end_raw,
  file = "data/end_raw.RData"
)




base_pair <- base_raw %>%
  filter(round_number == 1, mover == 1) %>%
  select(group_id, id_mover = id, partner_id) %>%
  rename(id_nonmover = partner_id)

end_pair <- end_raw %>%
  filter(round_number == 1, mover == 1) %>%
  select(group_id, id_mover = id, partner_id) %>%
  rename(id_nonmover = partner_id)

panel_final <- base_pair %>%
  inner_join(
    end_pair,
    by = "group_id",
    suffix = c("_base", "_end")
  )

panel_final <- panel_final %>%
  arrange(group_id)

# These groups are excluded because:
# - One group chose (coord_x = 32768, coord_y = 32767),
# - The other three groups chose (coord_x, coord_y) = (0, 0),
#   which violates the budget constraint (theoretical budget should equal 1)

drop_groups <- c(
  "11106161110601",
  "21204152120413",
  "21204162120405",
  "21204212120411"
)

panel_final <- panel_final %>%
  filter(!group_id %in% drop_groups) %>%
  arrange(group_id)

save(
  panel_final,
  file = "data/panel_final.RData"
)

panel_final %>%
  mutate(same_mover = id_mover_base == id_mover_end) %>%
  count(same_mover)

################################################################

rm(list = ls())

source("programs/ccei_garp.R")
source("programs/garp.R")
source("programs/warshall.R")

load("data/end_raw.RData")
load("data/panel_final.RData")

end_raw$id <- as.character(end_raw$id)
panel_final$id_mover_end <- as.character(panel_final$id_mover_end)
panel_final$id_nonmover_end <- as.character(panel_final$id_nonmover_end)

raw_filtered <- end_raw %>%
  filter(
    game_type == "individual",
    round_number %in% 1:18
  )

panel_final$ccei_1_end <- NA
panel_final$ccei_2_end <- NA

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_end[i]
  id2 <- panel_final$id_nonmover_end[i]
  
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    panel_final$ccei_1_end[i] <- ccei_garp(p1, x1)
  }
  
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    panel_final$ccei_2_end[i] <- ccei_garp(p2, x2)
  }
}

end_raw$id <- as.character(end_raw$id)
panel_final$id_mover_end <- as.character(panel_final$id_mover_end)
panel_final$id_nonmover_end <- as.character(panel_final$id_nonmover_end)

raw_filtered <- end_raw %>%
  filter(
    round_number >= 19,
    round_number <= 36,
    id != partner_id,
    !is.na(partner_id)
  )

panel_final$ccei_g_1_end <- NA
panel_final$ccei_g_2_end <- NA

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_end[i]
  id2 <- panel_final$id_nonmover_end[i]
  
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    panel_final$ccei_g_1_end[i] <- ccei_garp(p1, x1)
  }
  
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    panel_final$ccei_g_2_end[i] <- ccei_garp(p2, x2)
  }
}

all.equal(
  panel_final$ccei_g_1_end,
  panel_final$ccei_g_2_end,
  tolerance = 1e-12
)


panel_final$ccei_g_end <- panel_final$ccei_g_1_end

panel_final$ccei_g_1_end <- NULL
panel_final$ccei_g_2_end <- NULL

end_raw$id <- as.character(end_raw$id)
panel_final$id_mover_end <- as.character(panel_final$id_mover_end)
panel_final$id_nonmover_end <- as.character(panel_final$id_nonmover_end)

raw_filtered <- end_raw %>%
  filter(
    (game_type == "individual" & round_number %in% 1:18) |
      (round_number >= 19 & round_number <= 36),
    id != partner_id
  )

panel_final$ccei_1g_end <- NA
panel_final$ccei_2g_end <- NA

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_end[i]
  id2 <- panel_final$id_nonmover_end[i]
  
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    panel_final$ccei_1g_end[i] <- ccei_garp(p1, x1)
  }
  
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    panel_final$ccei_2g_end[i] <- ccei_garp(p2, x2)
  }
}

end_raw$id <- as.character(end_raw$id)
end_raw$partner_id <- as.character(end_raw$partner_id)

panel_final$id_mover_end <- as.character(panel_final$id_mover_end)
panel_final$id_nonmover_end <- as.character(panel_final$id_nonmover_end)

end_raw <- end_raw[end_raw$id != end_raw$partner_id, ]

end_raw$high_end <- 0

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_end[i]
  id2 <- panel_final$id_nonmover_end[i]
  
  c1  <- panel_final$ccei_1_end[i]
  c2  <- panel_final$ccei_2_end[i]
  c1g <- panel_final$ccei_1g_end[i]
  c2g <- panel_final$ccei_2g_end[i]
  
  high <- id1
  
  if (!is.na(c1) && !is.na(c2)) {
    if (c2 > c1) {
      high <- id2
    } else if (c1 == c2 && !is.na(c1g) && !is.na(c2g)) {
      if (c2g > c1g) high <- id2
    }
  }
  
  end_raw$high_end[end_raw$id == high] <- 1
}

panel_final$id_mover_end <- as.character(panel_final$id_mover_end)
panel_final$id_nonmover_end <- as.character(panel_final$id_nonmover_end)

panel_final$high_end <- NA

for (i in seq_len(nrow(panel_final))) {
  
  c1  <- panel_final$ccei_1_end[i]
  c2  <- panel_final$ccei_2_end[i]
  c1g <- panel_final$ccei_1g_end[i]
  c2g <- panel_final$ccei_2g_end[i]
  
  high <- 1
  
  if (!is.na(c1) && !is.na(c2)) {
    if (c2 > c1) {
      high <- 0
    } else if (c1 == c2 && !is.na(c1g) && !is.na(c2g)) {
      if (c2g > c1g) high <- 0
    }
  }
  
  panel_final$high_end[i] <- high
}


end_raw$id <- as.character(end_raw$id)
end_raw$partner_id <- as.character(end_raw$partner_id)

panel_final$id_mover_end <- as.character(panel_final$id_mover_end)
panel_final$id_nonmover_end <- as.character(panel_final$id_nonmover_end)

data <- end_raw[end_raw$high_end == 1, ]

data <- data[
  (data$round_number >= 1 & data$round_number <= 18) |
    (data$round_number >= 19 & data$round_number <= 36),
]

panel_final$ccei_hg_end <- NA

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_end[i]
  id2 <- panel_final$id_nonmover_end[i]
  
  sub1 <- data[data$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    val1 <- ccei_garp(p1, x1)
  } else {
    val1 <- NA
  }
  
  sub2 <- data[data$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    val2 <- ccei_garp(p2, x2)
  } else {
    val2 <- NA
  }
  
  if (!is.na(val1)) {
    panel_final$ccei_hg_end[i] <- val1
  } else if (!is.na(val2)) {
    panel_final$ccei_hg_end[i] <- val2
  }
}

###################################

end_raw$id <- as.character(end_raw$id)
end_raw$partner_id <- as.character(end_raw$partner_id)

panel_final$id_mover_end <- as.character(panel_final$id_mover_end)
panel_final$id_nonmover_end <- as.character(panel_final$id_nonmover_end)

data <- end_raw[end_raw$high_end == 0, ]

data <- data[
  (data$round_number >= 1 & data$round_number <= 18) |
    (data$round_number >= 19 & data$round_number <= 36),
]

panel_final$ccei_lg_end <- NA

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_end[i]
  id2 <- panel_final$id_nonmover_end[i]
  
  sub1 <- data[data$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    val1 <- ccei_garp(p1, x1)
  } else {
    val1 <- NA
  }
  
  sub2 <- data[data$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    val2 <- ccei_garp(p2, x2)
  } else {
    val2 <- NA
  }
  
  if (!is.na(val1)) {
    panel_final$ccei_lg_end[i] <- val1
  } else if (!is.na(val2)) {
    panel_final$ccei_lg_end[i] <- val2
  }
}

###################################

end_raw$id <- as.character(end_raw$id)
end_raw$partner_id <- as.character(end_raw$partner_id)
end_raw$mover <- as.character(end_raw$mover)

panel_final$group_id <- as.character(panel_final$group_id)
panel_final$id_mover_end <- as.character(panel_final$id_mover_end)
panel_final$id_nonmover_end <- as.character(panel_final$id_nonmover_end)

data <- end_raw[
  (end_raw$round_number >= 1 & end_raw$round_number <= 18) |
    (end_raw$round_number >= 19 & end_raw$round_number <= 36),
]

data <- data[data$id != data$partner_id, ]

data$group_id <- paste0(
  pmax(data$id, data$partner_id),
  pmin(data$id, data$partner_id)
)

data_indiv <- data[data$round_number <= 18, ]
data_group <- data[data$round_number >= 19 & data$mover == 1, ]

data_combined <- rbind(data_indiv, data_group)

panel_final$ccei_hlg_end <- NA

group_list <- unique(panel_final$group_id)

for (i in seq_along(group_list)) {
  
  g <- group_list[i]
  sub <- data_combined[data_combined$group_id == g, ]
  
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  panel_final$ccei_hlg_end[panel_final$group_id == g] <- ccei_garp(p, x)
}

###################################


panel_final <- panel_final %>%
  mutate(
    I_hg_end = ifelse(
      ccei_g_end != ccei_hlg_end,
      (ccei_g_end - ccei_hg_end) / (ccei_g_end - ccei_hlg_end),
      NA_real_
    ),
    I_lg_end = 1 - I_hg_end
  )

panel_final <- panel_final %>%
  mutate(
    temp_lg_end = ifelse(
      ccei_g_end != ccei_hlg_end,
      (ccei_g_end - ccei_lg_end) / (ccei_g_end - ccei_hlg_end),
      NA_real_
    )
  )

panel_final <- panel_final %>%
  mutate(
    I_lg_end = (I_lg_end + temp_lg_end) / 2,
    I_hg_end = (I_hg_end + (1 - temp_lg_end)) / 2
  ) %>%
  select(-temp_lg_end)

###################################

df <- end_raw %>%
  filter(game_type == "collective") %>%
  mutate(
    x_expensive = ifelse(intercept_x < intercept_y, coord_x, coord_y),
    RA = x_expensive / (coord_x + coord_y)
  ) %>%
  group_by(id) %>%
  summarise(RA = round(mean(RA, na.rm = TRUE), 5))

panel_final$RA_g_end <- df$RA[match(panel_final$id_mover_end, df$id)]

df <- end_raw %>%
  filter(game_type == "individual") %>%
  mutate(
    x_expensive = ifelse(intercept_x < intercept_y, coord_x, coord_y),
    RA = x_expensive / (coord_x + coord_y)
  ) %>%
  group_by(id) %>%
  summarise(RA = round(mean(RA, na.rm = TRUE), 5))

panel_final$RA_1_end <- df$RA[match(panel_final$id_mover_end, df$id)]
panel_final$RA_2_end <- df$RA[match(panel_final$id_nonmover_end, df$id)]

save(panel_final, file = "data/panel_final.RData")

################################################################

rm(list = ls())

source("programs/ccei_garp.R")
source("programs/garp.R")
source("programs/warshall.R")

load("data/base_raw.RData")
load("data/panel_final.RData")

base_raw$id <- as.character(base_raw$id)
panel_final$id_mover_base <- as.character(panel_final$id_mover_base)
panel_final$id_nonmover_base <- as.character(panel_final$id_nonmover_base)

raw_filtered <- base_raw %>%
  filter(
    game_type == "individual",
    round_number %in% 1:18
  )

panel_final$ccei_1_base <- NA
panel_final$ccei_2_base <- NA

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_base[i]
  id2 <- panel_final$id_nonmover_base[i]
  
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    panel_final$ccei_1_base[i] <- ccei_garp(p1, x1)
  }
  
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    panel_final$ccei_2_base[i] <- ccei_garp(p2, x2)
  }
}

raw_filtered <- base_raw %>%
  filter(
    round_number >= 19,
    round_number <= 36,
    id != partner_id,
    !is.na(partner_id)
  )

panel_final$ccei_g_1_base <- NA
panel_final$ccei_g_2_base <- NA

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_base[i]
  id2 <- panel_final$id_nonmover_base[i]
  
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    panel_final$ccei_g_1_base[i] <- ccei_garp(p1, x1)
  }
  
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    panel_final$ccei_g_2_base[i] <- ccei_garp(p2, x2)
  }
}

panel_final$ccei_g_base <- panel_final$ccei_g_1_base
panel_final$ccei_g_1_base <- NULL
panel_final$ccei_g_2_base <- NULL

raw_filtered <- base_raw %>%
  filter(
    (game_type == "individual" & round_number %in% 1:18) |
      (round_number >= 19 & round_number <= 36),
    id != partner_id
  )

panel_final$ccei_1g_base <- NA
panel_final$ccei_2g_base <- NA

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_base[i]
  id2 <- panel_final$id_nonmover_base[i]
  
  sub1 <- raw_filtered[raw_filtered$id == id1, ]
  if (nrow(sub1) > 0) {
    p1 <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x1 <- rbind(sub1$coord_x, sub1$coord_y)
    panel_final$ccei_1g_base[i] <- ccei_garp(p1, x1)
  }
  
  sub2 <- raw_filtered[raw_filtered$id == id2, ]
  if (nrow(sub2) > 0) {
    p2 <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x2 <- rbind(sub2$coord_x, sub2$coord_y)
    panel_final$ccei_2g_base[i] <- ccei_garp(p2, x2)
  }
}

base_raw$high_base <- 0

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_base[i]
  id2 <- panel_final$id_nonmover_base[i]
  
  c1  <- panel_final$ccei_1_base[i]
  c2  <- panel_final$ccei_2_base[i]
  c1g <- panel_final$ccei_1g_base[i]
  c2g <- panel_final$ccei_2g_base[i]
  
  high <- id1
  
  if (!is.na(c1) && !is.na(c2)) {
    if (c2 > c1) {
      high <- id2
    } else if (c1 == c2 && !is.na(c1g) && !is.na(c2g)) {
      if (c2g > c1g) high <- id2
    }
  }
  
  base_raw$high_base[base_raw$id == high] <- 1
}

panel_final$high_base <- NA

for (i in seq_len(nrow(panel_final))) {
  
  c1  <- panel_final$ccei_1_base[i]
  c2  <- panel_final$ccei_2_base[i]
  c1g <- panel_final$ccei_1g_base[i]
  c2g <- panel_final$ccei_2g_base[i]
  
  high <- 1
  
  if (!is.na(c1) && !is.na(c2)) {
    if (c2 > c1) {
      high <- 0
    } else if (c1 == c2 && !is.na(c1g) && !is.na(c2g)) {
      if (c2g > c1g) high <- 0
    }
  }
  
  panel_final$high_base[i] <- high
}

data <- base_raw[base_raw$high_base == 1, ]

data <- data[
  (data$round_number >= 1 & data$round_number <= 18) |
    (data$round_number >= 19 & data$round_number <= 36),
]

panel_final$ccei_hg_base <- NA

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_base[i]
  id2 <- panel_final$id_nonmover_base[i]
  
  sub1 <- data[data$id == id1, ]
  sub2 <- data[data$id == id2, ]
  
  if (nrow(sub1) > 0) {
    p <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x <- rbind(sub1$coord_x, sub1$coord_y)
    panel_final$ccei_hg_base[i] <- ccei_garp(p, x)
  } else if (nrow(sub2) > 0) {
    p <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x <- rbind(sub2$coord_x, sub2$coord_y)
    panel_final$ccei_hg_base[i] <- ccei_garp(p, x)
  }
}

data <- base_raw[base_raw$high_base == 0, ]

data <- data[
  (data$round_number >= 1 & data$round_number <= 18) |
    (data$round_number >= 19 & data$round_number <= 36),
]

panel_final$ccei_lg_base <- NA

for (i in seq_len(nrow(panel_final))) {
  
  id1 <- panel_final$id_mover_base[i]
  id2 <- panel_final$id_nonmover_base[i]
  
  sub1 <- data[data$id == id1, ]
  sub2 <- data[data$id == id2, ]
  
  if (nrow(sub1) > 0) {
    p <- rbind(1 / sub1$intercept_x, 1 / sub1$intercept_y)
    x <- rbind(sub1$coord_x, sub1$coord_y)
    panel_final$ccei_lg_base[i] <- ccei_garp(p, x)
  } else if (nrow(sub2) > 0) {
    p <- rbind(1 / sub2$intercept_x, 1 / sub2$intercept_y)
    x <- rbind(sub2$coord_x, sub2$coord_y)
    panel_final$ccei_lg_base[i] <- ccei_garp(p, x)
  }
}

data <- base_raw[
  (base_raw$round_number >= 1 & base_raw$round_number <= 18) |
    (base_raw$round_number >= 19 & base_raw$round_number <= 36),
]

data <- data[data$id != data$partner_id, ]

data$group_id <- paste0(
  pmax(data$id, data$partner_id),
  pmin(data$id, data$partner_id)
)

data_indiv <- data[data$round_number <= 18, ]
data_group <- data[data$round_number >= 19 & data$mover == 1, ]

data_combined <- rbind(data_indiv, data_group)

panel_final$ccei_hlg_base <- NA

for (g in unique(panel_final$group_id)) {
  
  sub <- data_combined[data_combined$group_id == g, ]
  if (nrow(sub) == 0) next
  
  p <- rbind(1 / sub$intercept_x, 1 / sub$intercept_y)
  x <- rbind(sub$coord_x, sub$coord_y)
  
  panel_final$ccei_hlg_base[panel_final$group_id == g] <- ccei_garp(p, x)
}

panel_final <- panel_final %>%
  mutate(
    I_hg_base = ifelse(
      ccei_g_base != ccei_hlg_base,
      (ccei_g_base - ccei_hg_base) / (ccei_g_base - ccei_hlg_base),
      NA_real_
    ),
    I_lg_base = 1 - I_hg_base
  )

panel_final <- panel_final %>%
  mutate(
    temp_lg_base = ifelse(
      ccei_g_base != ccei_hlg_base,
      (ccei_g_base - ccei_lg_base) / (ccei_g_base - ccei_hlg_base),
      NA_real_
    )
  )

panel_final <- panel_final %>%
  mutate(
    I_lg_base = (I_lg_base + temp_lg_base) / 2,
    I_hg_base = (I_hg_base + (1 - temp_lg_base)) / 2
  ) %>%
  select(-temp_lg_base)

df <- base_raw %>%
  filter(game_type == "collective") %>%
  mutate(
    x_expensive = ifelse(intercept_x < intercept_y, coord_x, coord_y),
    RA = x_expensive / (coord_x + coord_y)
  ) %>%
  group_by(id) %>%
  summarise(RA = round(mean(RA, na.rm = TRUE), 5))

panel_final$RA_g_base <- df$RA[match(panel_final$id_mover_base, df$id)]

df <- base_raw %>%
  filter(game_type == "individual") %>%
  mutate(
    x_expensive = ifelse(intercept_x < intercept_y, coord_x, coord_y),
    RA = x_expensive / (coord_x + coord_y)
  ) %>%
  group_by(id) %>%
  summarise(RA = round(mean(RA, na.rm = TRUE), 5))

panel_final$RA_1_base <- df$RA[match(panel_final$id_mover_base, df$id)]
panel_final$RA_2_base <- df$RA[match(panel_final$id_nonmover_base, df$id)]

save(panel_final, file = "data/panel_final.RData")

library(haven)
write_dta(panel_final, "data/panel_final.dta")

#########################################################


rm(list = ls())

library(rstudioapi)
library(tidyverse)
library(dplyr)
library(haven)

setwd(dirname(getSourceEditorContext()$path))

panel_final <- read_dta("data/panel_final.dta")
cog_base <- read_csv("data/Cognitive_Raw_Base.csv")

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

convert_participant_label <- function(x) {
  x <- trimws(as.character(x))
  
  if (is.na(x) || x == "") return(NA_character_)
  
  prefix <- substr(x, 1, 1)
  suffix <- substr(x, 2, nchar(x))
  
  if (!prefix %in% names(prefix_map)) return(NA_character_)
  
  paste0(prefix_map[prefix], suffix)
}

panel_final <- panel_final %>%
  mutate(
    id_mover_base = as.character(id_mover_base),
    id_nonmover_base = as.character(id_nonmover_base)
  )

panel_ids <- unique(c(panel_final$id_mover_base, panel_final$id_nonmover_base))
panel_ids <- panel_ids[!is.na(panel_ids)]

cog_base_score <- cog_base %>%
  select(
    Participant.label,
    Player.cq1,
    Player.cq2,
    Player.cq3,
    Player.cq4,
    Player.cq5
  ) %>%
  mutate(
    Participant.label = sapply(Participant.label, convert_participant_label),
    Participant.label = as.character(Participant.label),
    Player.cq1 = as.numeric(Player.cq1),
    Player.cq2 = as.numeric(Player.cq2),
    Player.cq3 = as.numeric(Player.cq3),
    Player.cq4 = as.numeric(Player.cq4),
    Player.cq5 = as.numeric(Player.cq5),
    mathscore_base =
      (Player.cq1 == 2) +
      (Player.cq2 == 4) +
      (Player.cq3 == 3) +
      (Player.cq4 == 5) +
      (Player.cq5 == 1)
  ) %>%
  filter(Participant.label %in% panel_ids) %>%
  distinct(Participant.label, .keep_all = TRUE) %>%
  select(Participant.label, mathscore_base)

panel_final <- panel_final %>%
  left_join(
    cog_base_score %>%
      rename(
        id_mover_base = Participant.label,
        mathscore_1_base = mathscore_base
      ),
    by = "id_mover_base"
  )

panel_final <- panel_final %>%
  left_join(
    cog_base_score %>%
      rename(
        id_nonmover_base = Participant.label,
        mathscore_2_base = mathscore_base
      ),
    by = "id_nonmover_base"
  )


#########################################################

cog_end <- read_csv("data/Cognitive_Raw_Post.csv")

panel_final <- panel_final %>%
  mutate(
    id_mover_end = as.character(id_mover_end),
    id_nonmover_end = as.character(id_nonmover_end)
  )

panel_ids_end <- unique(c(panel_final$id_mover_end, panel_final$id_nonmover_end))
panel_ids_end <- panel_ids_end[!is.na(panel_ids_end)]

cog_end_score <- cog_end %>%
  select(
    participant.label,
    player.cq1,
    player.cq2,
    player.cq3,
    player.cq4,
    player.cq5
  ) %>%
  mutate(
    participant.label = sapply(participant.label, convert_participant_label),
    participant.label = as.character(participant.label),
    player.cq1 = as.numeric(player.cq1),
    player.cq2 = as.numeric(player.cq2),
    player.cq3 = as.numeric(player.cq3),
    player.cq4 = as.numeric(player.cq4),
    player.cq5 = as.numeric(player.cq5),
    mathscore_end =
      (player.cq1 == 3) +
      (player.cq2 == 1) +
      (player.cq3 == 5) +
      (player.cq4 == 2) +
      (player.cq5 == 5)
  ) %>%
  filter(participant.label %in% panel_ids_end) %>%
  distinct(participant.label, .keep_all = TRUE) %>%
  select(participant.label, mathscore_end)

panel_final <- panel_final %>%
  left_join(
    cog_end_score %>%
      rename(
        id_mover_end = participant.label,
        mathscore_1_end = mathscore_end
      ),
    by = "id_mover_end"
  )

panel_final <- panel_final %>%
  left_join(
    cog_end_score %>%
      rename(
        id_nonmover_end = participant.label,
        mathscore_2_end = mathscore_end
      ),
    by = "id_nonmover_end"
  )

write_dta(panel_final, "data/panel_final.dta")

##############################################################33



noncog_base <- read_csv("data/NonCognitive_Raw_Base.csv")

convert_likert <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

reverse_score <- function(x) {
  ifelse(is.na(x), NA_real_, 6 - x)
}

panel_final <- panel_final %>%
  mutate(
    id_mover_base = as.character(id_mover_base),
    id_nonmover_base = as.character(id_nonmover_base)
  )

panel_ids_base <- unique(c(panel_final$id_mover_base, panel_final$id_nonmover_base))
panel_ids_base <- panel_ids_base[!is.na(panel_ids_base)]

personality_base <- noncog_base %>%
  select(
    id_new,
    Player.q33,
    Player.q34,
    Player.q35,
    Player.q36,
    Player.q37,
    Player.q38,
    Player.q39,
    Player.q40,
    Player.q41,
    Player.q42
  ) %>%
  mutate(
    id_new = as.character(id_new),
    q33 = convert_likert(Player.q33),
    q34 = convert_likert(Player.q34),
    q35 = convert_likert(Player.q35),
    q36 = convert_likert(Player.q36),
    q37 = convert_likert(Player.q37),
    q38 = convert_likert(Player.q38),
    q39 = convert_likert(Player.q39),
    q40 = convert_likert(Player.q40),
    q41 = convert_likert(Player.q41),
    q42 = convert_likert(Player.q42),
    outgoing = (q33 + reverse_score(q38)) / 2,
    opened = (q37 + reverse_score(q42)) / 2,
    agreeable = (q39 + reverse_score(q34)) / 2,
    conscientious = (q35 + reverse_score(q40)) / 2,
    stable = (q41 + reverse_score(q36)) / 2
  ) %>%
  filter(id_new %in% panel_ids_base) %>%
  distinct(id_new, .keep_all = TRUE) %>%
  select(
    id_new,
    outgoing,
    opened,
    agreeable,
    conscientious,
    stable
  )

panel_final <- panel_final %>%
  left_join(
    personality_base %>%
      rename(
        id_mover_base = id_new,
        outgoing_1_base = outgoing,
        opened_1_base = opened,
        agreeable_1_base = agreeable,
        conscientious_1_base = conscientious,
        stable_1_base = stable
      ),
    by = "id_mover_base"
  )

panel_final <- panel_final %>%
  left_join(
    personality_base %>%
      rename(
        id_nonmover_base = id_new,
        outgoing_2_base = outgoing,
        opened_2_base = opened,
        agreeable_2_base = agreeable,
        conscientious_2_base = conscientious,
        stable_2_base = stable
      ),
    by = "id_nonmover_base"
  )

write_dta(panel_final, "data/panel_final.dta")

#############################################################

rm(list = ls())

library(rstudioapi)
library(tidyverse)
library(dplyr)
library(haven)

setwd(dirname(getSourceEditorContext()$path))

source("programs/ccei_fgarp.R")
source("programs/fgarp.R")
source("programs/warshall.R")

panel_final <- read_dta("data/panel_final.dta")

load("data/base_raw.RData")
load("data/end_raw.RData")

pi <- matrix(1/2, 2, 1)


compute_f_ccei_block <- function(raw, panel_final, suffix) {
  
  # suffix: "base" or "end"
  
  id1_col <- paste0("id_mover_", suffix)
  id2_col <- paste0("id_nonmover_", suffix)
  
  f1_col      <- paste0("f_ccei_1_", suffix)
  f2_col      <- paste0("f_ccei_2_", suffix)
  fg_col      <- paste0("f_ccei_g_", suffix)
  f1g_col     <- paste0("f_ccei_1g_", suffix)
  f2g_col     <- paste0("f_ccei_2g_", suffix)
  fhg_col     <- paste0("f_ccei_hg_", suffix)
  flg_col     <- paste0("f_ccei_lg_", suffix)
  fhlg_col    <- paste0("f_ccei_hlg_", suffix)
  fhigh_col   <- paste0("f_high_", suffix)
  
  raw$id <- as.character(raw$id)
  raw$partner_id <- as.character(raw$partner_id)
  raw$mover <- as.character(raw$mover)
  
  panel_final$group_id <- as.character(panel_final$group_id)
  panel_final[[id1_col]] <- as.character(panel_final[[id1_col]])
  panel_final[[id2_col]] <- as.character(panel_final[[id2_col]])
  

  
  raw_filtered <- raw %>%
    filter(
      game_type == "individual",
      round_number %in% 1:18
    )
  
  panel_final[[f1_col]] <- NA_real_
  panel_final[[f2_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel_final))) {
    
    id1 <- panel_final[[id1_col]][i]
    id2 <- panel_final[[id2_col]][i]
    
    sub1 <- raw_filtered[raw_filtered$id == id1, ]
    
    if (nrow(sub1) > 0) {
      p1 <- rbind(
        1 / sub1$intercept_x,
        1 / sub1$intercept_y
      )
      
      x1 <- rbind(
        sub1$coord_x,
        sub1$coord_y
      )
      
      panel_final[[f1_col]][i] <- ccei_fgarp(p1, x1, pi)
    }
    
    sub2 <- raw_filtered[raw_filtered$id == id2, ]
    
    if (nrow(sub2) > 0) {
      p2 <- rbind(
        1 / sub2$intercept_x,
        1 / sub2$intercept_y
      )
      
      x2 <- rbind(
        sub2$coord_x,
        sub2$coord_y
      )
      
      panel_final[[f2_col]][i] <- ccei_fgarp(p2, x2, pi)
    }
  }

  
  raw_filtered <- raw %>%
    filter(
      round_number >= 19,
      round_number <= 36,
      id != partner_id,
      !is.na(partner_id)
    )
  
  f_g_1 <- rep(NA_real_, nrow(panel_final))
  f_g_2 <- rep(NA_real_, nrow(panel_final))
  
  for (i in seq_len(nrow(panel_final))) {
    
    id1 <- panel_final[[id1_col]][i]
    id2 <- panel_final[[id2_col]][i]
    
    sub1 <- raw_filtered[raw_filtered$id == id1, ]
    
    if (nrow(sub1) > 0) {
      p1 <- rbind(
        1 / sub1$intercept_x,
        1 / sub1$intercept_y
      )
      
      x1 <- rbind(
        sub1$coord_x,
        sub1$coord_y
      )
      
      f_g_1[i] <- ccei_fgarp(p1, x1, pi)
    }
    
    sub2 <- raw_filtered[raw_filtered$id == id2, ]
    
    if (nrow(sub2) > 0) {
      p2 <- rbind(
        1 / sub2$intercept_x,
        1 / sub2$intercept_y
      )
      
      x2 <- rbind(
        sub2$coord_x,
        sub2$coord_y
      )
      
      f_g_2[i] <- ccei_fgarp(p2, x2, pi)
    }
  }
  
  panel_final[[fg_col]] <- f_g_1
  
  
  raw_filtered <- raw %>%
    filter(
      (
        game_type == "individual" &
          round_number %in% 1:18
      ) |
        (
          round_number >= 19 &
            round_number <= 36
        ),
      id != partner_id
    )
  
  panel_final[[f1g_col]] <- NA_real_
  panel_final[[f2g_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel_final))) {
    
    id1 <- panel_final[[id1_col]][i]
    id2 <- panel_final[[id2_col]][i]
    
    sub1 <- raw_filtered[raw_filtered$id == id1, ]
    
    if (nrow(sub1) > 0) {
      p1 <- rbind(
        1 / sub1$intercept_x,
        1 / sub1$intercept_y
      )
      
      x1 <- rbind(
        sub1$coord_x,
        sub1$coord_y
      )
      
      panel_final[[f1g_col]][i] <- ccei_fgarp(p1, x1, pi)
    }
    
    sub2 <- raw_filtered[raw_filtered$id == id2, ]
    
    if (nrow(sub2) > 0) {
      p2 <- rbind(
        1 / sub2$intercept_x,
        1 / sub2$intercept_y
      )
      
      x2 <- rbind(
        sub2$coord_x,
        sub2$coord_y
      )
      
      panel_final[[f2g_col]][i] <- ccei_fgarp(p2, x2, pi)
    }
  }

  
  raw_work <- raw[raw$id != raw$partner_id, ]
  
  raw_work$f_high_tmp <- 0
  raw_work$f_low_tmp <- 0
  
  panel_final[[fhigh_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel_final))) {
    
    id1 <- panel_final[[id1_col]][i]
    id2 <- panel_final[[id2_col]][i]
    
    f1 <- panel_final[[f1_col]][i]
    f2 <- panel_final[[f2_col]][i]
    f1g <- panel_final[[f1g_col]][i]
    f2g <- panel_final[[f2g_col]][i]
    
    high_id <- id1
    low_id <- id2
    high_binary <- 1
    
    if (!is.na(f1) && !is.na(f2)) {
      
      if (f2 > f1) {
        high_id <- id2
        low_id <- id1
        high_binary <- 0
      } else if (f1 == f2 && !is.na(f1g) && !is.na(f2g)) {
        if (f2g > f1g) {
          high_id <- id2
          low_id <- id1
          high_binary <- 0
        }
      }
    }
    
    panel_final[[fhigh_col]][i] <- high_binary
    
    raw_work$f_high_tmp[raw_work$id == high_id] <- 1
    raw_work$f_low_tmp[raw_work$id == low_id] <- 1
  }

  
  data_high <- raw_work[raw_work$f_high_tmp == 1, ]
  
  data_high <- data_high[
    (data_high$round_number >= 1 & data_high$round_number <= 18) |
      (data_high$round_number >= 19 & data_high$round_number <= 36),
  ]
  
  panel_final[[fhg_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel_final))) {
    
    id1 <- panel_final[[id1_col]][i]
    id2 <- panel_final[[id2_col]][i]
    
    sub1 <- data_high[data_high$id == id1, ]
    sub2 <- data_high[data_high$id == id2, ]
    
    if (nrow(sub1) > 0) {
      
      p <- rbind(
        1 / sub1$intercept_x,
        1 / sub1$intercept_y
      )
      
      x <- rbind(
        sub1$coord_x,
        sub1$coord_y
      )
      
      panel_final[[fhg_col]][i] <- ccei_fgarp(p, x, pi)
      
    } else if (nrow(sub2) > 0) {
      
      p <- rbind(
        1 / sub2$intercept_x,
        1 / sub2$intercept_y
      )
      
      x <- rbind(
        sub2$coord_x,
        sub2$coord_y
      )
      
      panel_final[[fhg_col]][i] <- ccei_fgarp(p, x, pi)
    }
  }

  
  data_low <- raw_work[raw_work$f_low_tmp == 1, ]
  
  data_low <- data_low[
    (data_low$round_number >= 1 & data_low$round_number <= 18) |
      (data_low$round_number >= 19 & data_low$round_number <= 36),
  ]
  
  panel_final[[flg_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel_final))) {
    
    id1 <- panel_final[[id1_col]][i]
    id2 <- panel_final[[id2_col]][i]
    
    sub1 <- data_low[data_low$id == id1, ]
    sub2 <- data_low[data_low$id == id2, ]
    
    if (nrow(sub1) > 0) {
      
      p <- rbind(
        1 / sub1$intercept_x,
        1 / sub1$intercept_y
      )
      
      x <- rbind(
        sub1$coord_x,
        sub1$coord_y
      )
      
      panel_final[[flg_col]][i] <- ccei_fgarp(p, x, pi)
      
    } else if (nrow(sub2) > 0) {
      
      p <- rbind(
        1 / sub2$intercept_x,
        1 / sub2$intercept_y
      )
      
      x <- rbind(
        sub2$coord_x,
        sub2$coord_y
      )
      
      panel_final[[flg_col]][i] <- ccei_fgarp(p, x, pi)
    }
  }

  
  data_all <- raw[
    (raw$round_number >= 1 & raw$round_number <= 18) |
      (raw$round_number >= 19 & raw$round_number <= 36),
  ]
  
  data_all <- data_all[data_all$id != data_all$partner_id, ]
  
  data_all$group_id <- paste0(
    pmax(data_all$id, data_all$partner_id),
    pmin(data_all$id, data_all$partner_id)
  )
  
  data_indiv <- data_all[data_all$round_number <= 18, ]
  data_group <- data_all[data_all$round_number >= 19 & data_all$mover == 1, ]
  
  data_combined <- rbind(data_indiv, data_group)
  
  panel_final[[fhlg_col]] <- NA_real_
  
  group_list <- unique(panel_final$group_id)
  
  for (g in group_list) {
    
    sub <- data_combined[data_combined$group_id == g, ]
    
    if (nrow(sub) == 0) next
    
    p <- rbind(
      1 / sub$intercept_x,
      1 / sub$intercept_y
    )
    
    x <- rbind(
      sub$coord_x,
      sub$coord_y
    )
    
    panel_final[[fhlg_col]][panel_final$group_id == g] <- ccei_fgarp(p, x, pi)
  }
  
  
  return(panel_final)
}



panel_final <- compute_f_ccei_block(
  raw = end_raw,
  panel_final = panel_final,
  suffix = "end"
)


panel_final <- compute_f_ccei_block(
  raw = base_raw,
  panel_final = panel_final,
  suffix = "base"
)


panel_final <- panel_final %>%
  mutate(
    f_I_hg_end = ifelse(
      f_ccei_g_end != f_ccei_hlg_end,
      (f_ccei_g_end - f_ccei_hg_end) /
        (f_ccei_g_end - f_ccei_hlg_end),
      NA_real_
    ),
    f_I_lg_end = 1 - f_I_hg_end
  )

panel_final <- panel_final %>%
  mutate(
    f_temp_lg_end = ifelse(
      f_ccei_g_end != f_ccei_hlg_end,
      (f_ccei_g_end - f_ccei_lg_end) /
        (f_ccei_g_end - f_ccei_hlg_end),
      NA_real_
    )
  )

panel_final <- panel_final %>%
  mutate(
    f_I_lg_end = (f_I_lg_end + f_temp_lg_end) / 2,
    f_I_hg_end = (f_I_hg_end + (1 - f_temp_lg_end)) / 2
  ) %>%
  select(-f_temp_lg_end)


panel_final <- panel_final %>%
  mutate(
    f_I_hg_base = ifelse(
      f_ccei_g_base != f_ccei_hlg_base,
      (f_ccei_g_base - f_ccei_hg_base) /
        (f_ccei_g_base - f_ccei_hlg_base),
      NA_real_
    ),
    f_I_lg_base = 1 - f_I_hg_base
  )

panel_final <- panel_final %>%
  mutate(
    f_temp_lg_base = ifelse(
      f_ccei_g_base != f_ccei_hlg_base,
      (f_ccei_g_base - f_ccei_lg_base) /
        (f_ccei_g_base - f_ccei_hlg_base),
      NA_real_
    )
  )

panel_final <- panel_final %>%
  mutate(
    f_I_lg_base = (f_I_lg_base + f_temp_lg_base) / 2,
    f_I_hg_base = (f_I_hg_base + (1 - f_temp_lg_base)) / 2
  ) %>%
  select(-f_temp_lg_base)


write_dta(panel_final, "data/panel_final.dta")


#############################################################

rm(list = ls())

library(rstudioapi)
library(dplyr)
library(haven)
library(revpref)

setwd(dirname(getSourceEditorContext()$path))

panel_final <- read_dta("data/panel_final.dta")

load("data/base_raw.RData")
load("data/end_raw.RData")


calc_mpi_pair <- function(dat) {
  
  if (nrow(dat) == 0) {
    return(c(NA_real_, NA_real_))
  }
  
  p <- cbind(
    1 / dat$intercept_x,
    1 / dat$intercept_y
  )
  
  q <- cbind(
    dat$coord_x,
    dat$coord_y
  )
  
  out <- tryCatch(
    mpi(p, q),
    error = function(e) c(NA_real_, NA_real_)
  )
  
  return(c(out[1], out[2]))
}


compute_mpi_block <- function(raw, panel_final, suffix) {
  
  # suffix: "base" or "end"
  
  id1_col <- paste0("id_mover_", suffix)
  id2_col <- paste0("id_nonmover_", suffix)
  
  min_1_col    <- paste0("min_mpi_1_", suffix)
  max_1_col    <- paste0("max_mpi_1_", suffix)
  min_2_col    <- paste0("min_mpi_2_", suffix)
  max_2_col    <- paste0("max_mpi_2_", suffix)
  
  min_g_col    <- paste0("min_mpi_g_", suffix)
  max_g_col    <- paste0("max_mpi_g_", suffix)
  
  min_1g_col   <- paste0("min_mpi_1g_", suffix)
  max_1g_col   <- paste0("max_mpi_1g_", suffix)
  min_2g_col   <- paste0("min_mpi_2g_", suffix)
  max_2g_col   <- paste0("max_mpi_2g_", suffix)
  
  min_hg_col   <- paste0("min_mpi_hg_", suffix)
  max_hg_col   <- paste0("max_mpi_hg_", suffix)
  min_lg_col   <- paste0("min_mpi_lg_", suffix)
  max_lg_col   <- paste0("max_mpi_lg_", suffix)
  
  min_hlg_col  <- paste0("min_mpi_hlg_", suffix)
  max_hlg_col  <- paste0("max_mpi_hlg_", suffix)
  
  min_high_col <- paste0("min_mpi_high_", suffix)
  max_high_col <- paste0("max_mpi_high_", suffix)
  
  raw$id <- as.character(raw$id)
  raw$partner_id <- as.character(raw$partner_id)
  raw$mover <- as.character(raw$mover)
  raw$group_id <- as.character(raw$group_id)
  
  panel_final$group_id <- as.character(panel_final$group_id)
  panel_final[[id1_col]] <- as.character(panel_final[[id1_col]])
  panel_final[[id2_col]] <- as.character(panel_final[[id2_col]])
  
  
  indiv_raw <- raw %>%
    filter(
      game_type == "individual",
      round_number %in% 1:18,
      id != partner_id
    )
  
  panel_final[[min_1_col]] <- NA_real_
  panel_final[[max_1_col]] <- NA_real_
  panel_final[[min_2_col]] <- NA_real_
  panel_final[[max_2_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel_final))) {
    
    id1 <- panel_final[[id1_col]][i]
    id2 <- panel_final[[id2_col]][i]
    
    sub1 <- indiv_raw[indiv_raw$id == id1, ]
    v1 <- calc_mpi_pair(sub1)
    
    panel_final[[min_1_col]][i] <- v1[1]
    panel_final[[max_1_col]][i] <- v1[2]
    
    sub2 <- indiv_raw[indiv_raw$id == id2, ]
    v2 <- calc_mpi_pair(sub2)
    
    panel_final[[min_2_col]][i] <- v2[1]
    panel_final[[max_2_col]][i] <- v2[2]
  }
  

  
  group_raw <- raw %>%
    filter(
      round_number >= 19,
      round_number <= 36,
      id != partner_id,
      !is.na(partner_id),
      partner_id != "0"
    )
  
  panel_final[[min_g_col]] <- NA_real_
  panel_final[[max_g_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel_final))) {
    
    id1 <- panel_final[[id1_col]][i]
    
    sub_g <- group_raw[group_raw$id == id1, ]
    vg <- calc_mpi_pair(sub_g)
    
    panel_final[[min_g_col]][i] <- vg[1]
    panel_final[[max_g_col]][i] <- vg[2]
  }
  

  
  full_raw <- raw %>%
    filter(
      round_number %in% 1:36,
      id != partner_id
    )
  
  panel_final[[min_1g_col]] <- NA_real_
  panel_final[[max_1g_col]] <- NA_real_
  panel_final[[min_2g_col]] <- NA_real_
  panel_final[[max_2g_col]] <- NA_real_
  
  for (i in seq_len(nrow(panel_final))) {
    
    id1 <- panel_final[[id1_col]][i]
    id2 <- panel_final[[id2_col]][i]
    
    sub1g <- full_raw[full_raw$id == id1, ]
    v1g <- calc_mpi_pair(sub1g)
    
    panel_final[[min_1g_col]][i] <- v1g[1]
    panel_final[[max_1g_col]][i] <- v1g[2]
    
    sub2g <- full_raw[full_raw$id == id2, ]
    v2g <- calc_mpi_pair(sub2g)
    
    panel_final[[min_2g_col]][i] <- v2g[1]
    panel_final[[max_2g_col]][i] <- v2g[2]
  }
  
  

  data_all <- raw[
    (raw$round_number >= 1 & raw$round_number <= 18) |
      (raw$round_number >= 19 & raw$round_number <= 36),
  ]
  
  data_all <- data_all[data_all$id != data_all$partner_id, ]
  
  data_all$group_id <- paste0(
    pmax(data_all$id, data_all$partner_id),
    pmin(data_all$id, data_all$partner_id)
  )
  
  data_indiv <- data_all[data_all$round_number <= 18, ]
  data_group <- data_all[data_all$round_number >= 19 & data_all$mover == 1, ]
  
  data_hlg <- rbind(data_indiv, data_group)
  
  panel_final[[min_hlg_col]] <- NA_real_
  panel_final[[max_hlg_col]] <- NA_real_
  
  for (g in unique(panel_final$group_id)) {
    
    sub_hlg <- data_hlg[data_hlg$group_id == g, ]
    vhlg <- calc_mpi_pair(sub_hlg)
    
    panel_final[[min_hlg_col]][panel_final$group_id == g] <- vhlg[1]
    panel_final[[max_hlg_col]][panel_final$group_id == g] <- vhlg[2]
  }
  
  
  
  panel_final[[min_high_col]] <- ifelse(
    panel_final[[min_1_col]] < panel_final[[min_2_col]], 1,
    ifelse(
      panel_final[[min_1_col]] > panel_final[[min_2_col]], 0,
      ifelse(
        panel_final[[min_1g_col]] < panel_final[[min_2g_col]], 1,
        ifelse(
          panel_final[[min_1g_col]] > panel_final[[min_2g_col]], 0,
          1
        )
      )
    )
  )
  
  panel_final[[max_high_col]] <- ifelse(
    panel_final[[max_1_col]] < panel_final[[max_2_col]], 1,
    ifelse(
      panel_final[[max_1_col]] > panel_final[[max_2_col]], 0,
      ifelse(
        panel_final[[max_1g_col]] < panel_final[[max_2g_col]], 1,
        ifelse(
          panel_final[[max_1g_col]] > panel_final[[max_2g_col]], 0,
          1
        )
      )
    )
  )
  
  
  
  panel_final[[min_hg_col]] <- ifelse(
    panel_final[[min_high_col]] == 1,
    panel_final[[min_1g_col]],
    panel_final[[min_2g_col]]
  )
  
  panel_final[[min_lg_col]] <- ifelse(
    panel_final[[min_high_col]] == 1,
    panel_final[[min_2g_col]],
    panel_final[[min_1g_col]]
  )
  
  panel_final[[max_hg_col]] <- ifelse(
    panel_final[[max_high_col]] == 1,
    panel_final[[max_1g_col]],
    panel_final[[max_2g_col]]
  )
  
  panel_final[[max_lg_col]] <- ifelse(
    panel_final[[max_high_col]] == 1,
    panel_final[[max_2g_col]],
    panel_final[[max_1g_col]]
  )
  
  
  return(panel_final)
}



panel_final <- compute_mpi_block(
  raw = end_raw,
  panel_final = panel_final,
  suffix = "end"
)



panel_final <- compute_mpi_block(
  raw = base_raw,
  panel_final = panel_final,
  suffix = "base"
)



max_mpi_vars <- grep("^max_mpi_", names(panel_final), value = TRUE)

for (v in max_mpi_vars) {
  
  new_v <- sub("^max_mpi_", "rev_max_mpi_", v)
  
  panel_final[[new_v]] <- ifelse(
    is.na(panel_final[[v]]),
    NA_real_,
    1 - panel_final[[v]]
  )
}



safe_ratio <- function(num, den) {
  ifelse(abs(den) < 1e-10, NA_real_, num / den)
}



for (p in c("min", "max")) {
  
  for (sfx in c("_base", "_end")) {
    
    g   <- panel_final[[paste0(p, "_mpi_g", sfx)]]
    hg  <- panel_final[[paste0(p, "_mpi_hg", sfx)]]
    lg  <- panel_final[[paste0(p, "_mpi_lg", sfx)]]
    hlg <- panel_final[[paste0(p, "_mpi_hlg", sfx)]]
    
    Ihg1 <- safe_ratio(hg - g, hlg - g)
    Ilg1 <- 1 - Ihg1
    
    Ilg2 <- safe_ratio(lg - g, hlg - g)
    Ihg2 <- 1 - Ilg2
    
    panel_final[[paste0("I_", p, "_mpi_hg", sfx)]] <- rowMeans(
      cbind(Ihg1, Ihg2),
      na.rm = TRUE
    )
    
    panel_final[[paste0("I_", p, "_mpi_lg", sfx)]] <- rowMeans(
      cbind(Ilg1, Ilg2),
      na.rm = TRUE
    )
  }
}



for (sfx in c("_base", "_end")) {
  
  g   <- panel_final[[paste0("rev_max_mpi_g", sfx)]]
  hg  <- panel_final[[paste0("rev_max_mpi_hg", sfx)]]
  lg  <- panel_final[[paste0("rev_max_mpi_lg", sfx)]]
  hlg <- panel_final[[paste0("rev_max_mpi_hlg", sfx)]]
  
  Ihg1 <- safe_ratio(g - hg, g - hlg)
  Ilg1 <- 1 - Ihg1
  
  Ilg2 <- safe_ratio(g - lg, g - hlg)
  Ihg2 <- 1 - Ilg2
  
  panel_final[[paste0("I_rev_max_mpi_hg", sfx)]] <- rowMeans(
    cbind(Ihg1, Ihg2),
    na.rm = TRUE
  )
  
  panel_final[[paste0("I_rev_max_mpi_lg", sfx)]] <- rowMeans(
    cbind(Ilg1, Ilg2),
    na.rm = TRUE
  )
}


write_dta(panel_final, "data/panel_final.dta")


#############################################################


rm(list = ls())

library(rstudioapi)
library(dplyr)
library(haven)

setwd(dirname(getSourceEditorContext()$path))

panel_final <- read_dta("data/panel_final.dta")

survey_cleaned <- read_dta("data/Survey_Cleaned.dta")


survey_vars <- survey_cleaned %>%
  select(
    id,
    male,
    class_sleep,
    class_participate,
    teacher_prepare,
    teacher_induce,
    class_study,
    class_dislike,
    class_practical,
    pbl_korean,
    pbl_eng,
    pbl_math,
    pbl_science,
    pbl_socialsci,
    height,
    weight
  ) %>%
  mutate(
    id = as.character(id),
    male = ifelse(male == 2, 0, male)
  ) %>%
  distinct(id, .keep_all = TRUE)


panel_final <- panel_final %>%
  mutate(
    id_mover_base = as.character(id_mover_base),
    id_nonmover_base = as.character(id_nonmover_base),
    id_mover_end = as.character(id_mover_end),
    id_nonmover_end = as.character(id_nonmover_end)
  )


survey_1_base <- survey_vars %>%
  rename(
    id_mover_base = id,
    
    male_1_base = male,
    class_sleep_1_base = class_sleep,
    class_participate_1_base = class_participate,
    teacher_prepare_1_base = teacher_prepare,
    teacher_induce_1_base = teacher_induce,
    class_study_1_base = class_study,
    class_dislike_1_base = class_dislike,
    class_practical_1_base = class_practical,
    pbl_korean_1_base = pbl_korean,
    pbl_eng_1_base = pbl_eng,
    pbl_math_1_base = pbl_math,
    pbl_science_1_base = pbl_science,
    pbl_socialsci_1_base = pbl_socialsci,
    height_1_base = height,
    weight_1_base = weight
  )

panel_final <- panel_final %>%
  left_join(
    survey_1_base,
    by = "id_mover_base"
  )


survey_2_base <- survey_vars %>%
  rename(
    id_nonmover_base = id,
    
    male_2_base = male,
    class_sleep_2_base = class_sleep,
    class_participate_2_base = class_participate,
    teacher_prepare_2_base = teacher_prepare,
    teacher_induce_2_base = teacher_induce,
    class_study_2_base = class_study,
    class_dislike_2_base = class_dislike,
    class_practical_2_base = class_practical,
    pbl_korean_2_base = pbl_korean,
    pbl_eng_2_base = pbl_eng,
    pbl_math_2_base = pbl_math,
    pbl_science_2_base = pbl_science,
    pbl_socialsci_2_base = pbl_socialsci,
    height_2_base = height,
    weight_2_base = weight
  )

panel_final <- panel_final %>%
  left_join(
    survey_2_base,
    by = "id_nonmover_base"
  )


panel_final <- panel_final %>%
  mutate(
    height_max_base = pmax(height_1_base, height_2_base, na.rm = TRUE),
    height_min_base = pmin(height_1_base, height_2_base, na.rm = TRUE),
    height_dist_base = abs(height_1_base - height_2_base)
  )


write_dta(panel_final, "data/panel_final.dta")

##########################################################

rm(list = ls())

library(rstudioapi)
library(dplyr)
library(haven)

setwd(dirname(getSourceEditorContext()$path))

panel_final <- read_dta("data/panel_final.dta")
network_raw <- read_dta("data/network_survey.dta")

network_clean <- network_raw %>%
  select(
    id,
    obj_id,
    t
  ) %>%
  mutate(
    id = as.character(id),
    obj_id = as.character(obj_id),
    t = as.integer(t),
    id_class = substr(id, 1, 5),
    obj_class = substr(obj_id, 1, 5),
    inclass = as.integer(id_class == obj_class)
  ) %>%
  filter(
    t %in% c(0, 1),
    !is.na(id),
    !is.na(obj_id),
    id != "",
    obj_id != "",
    id != obj_id
  ) %>%
  distinct(t, id, obj_id, .keep_all = TRUE)

id_time_list <- bind_rows(
  network_clean %>%
    select(t, id),
  network_clean %>%
    select(t, id = obj_id)
) %>%
  distinct(t, id) %>%
  mutate(
    class = substr(id, 1, 5)
  )

outdegree_df <- network_clean %>%
  filter(inclass == 1) %>%
  group_by(t, id) %>%
  summarise(
    inclass_n_friends = n_distinct(obj_id),
    .groups = "drop"
  )

indegree_df <- network_clean %>%
  filter(inclass == 1) %>%
  group_by(t, obj_id) %>%
  summarise(
    inclass_popularity = n_distinct(id),
    .groups = "drop"
  ) %>%
  rename(id = obj_id)

network_stats <- id_time_list %>%
  left_join(outdegree_df, by = c("t", "id")) %>%
  left_join(indegree_df, by = c("t", "id")) %>%
  mutate(
    inclass_n_friends = ifelse(is.na(inclass_n_friends), 0L, inclass_n_friends),
    inclass_popularity = ifelse(is.na(inclass_popularity), 0L, inclass_popularity)
  )

network_pair <- network_clean %>%
  select(
    t,
    id,
    obj_id
  ) %>%
  mutate(
    friend_nomination = 1L
  ) %>%
  distinct(t, id, obj_id, .keep_all = TRUE)

panel_final <- panel_final %>%
  mutate(
    group_id = as.character(group_id),
    id_mover_base = as.character(id_mover_base),
    id_nonmover_base = as.character(id_nonmover_base),
    id_mover_end = as.character(id_mover_end),
    id_nonmover_end = as.character(id_nonmover_end)
  )

network_old_cols <- grep(
  paste0(
    "(",
    "^class_[12]_(base|end)$|",
    "^class_(base|end)$|",
    "^inclass_n_friends_[12]_(base|end)$|",
    "^inclass_popularity_[12]_(base|end)$|",
    "^inclass_n_friends_(max|dist)_(base|end)$|",
    "^inclass_popularity_(max|dist)_(base|end)$|",
    "^friendship_[12]_(base|end)$|",
    "^mutual_friendship_(base|end)$|",
    "^oneway_friendship_(base|end)$|",
    "^none_friendship_(base|end)$|",
    "^friendship_(base|end)$",
    ")"
  ),
  names(panel_final),
  value = TRUE
)

if (length(network_old_cols) > 0) {
  panel_final <- panel_final %>%
    select(-all_of(network_old_cols))
}

add_network_time <- function(panel_final, suffix, t_value) {
  
  id1_col <- paste0("id_mover_", suffix)
  id2_col <- paste0("id_nonmover_", suffix)
  
  class_1_col <- paste0("class_1_", suffix)
  class_2_col <- paste0("class_2_", suffix)
  
  out_1_col <- paste0("inclass_n_friends_1_", suffix)
  out_2_col <- paste0("inclass_n_friends_2_", suffix)
  
  pop_1_col <- paste0("inclass_popularity_1_", suffix)
  pop_2_col <- paste0("inclass_popularity_2_", suffix)
  
  friend_1_col <- paste0("friendship_1_", suffix)
  friend_2_col <- paste0("friendship_2_", suffix)
  
  mutual_col <- paste0("mutual_friendship_", suffix)
  oneway_col <- paste0("oneway_friendship_", suffix)
  none_col <- paste0("none_friendship_", suffix)
  friendship_col <- paste0("friendship_", suffix)
  
  stats_1 <- network_stats %>%
    filter(t == t_value) %>%
    select(
      id,
      class,
      inclass_n_friends,
      inclass_popularity
    ) %>%
    rename(
      !!id1_col := id,
      !!class_1_col := class,
      !!out_1_col := inclass_n_friends,
      !!pop_1_col := inclass_popularity
    )
  
  panel_final <- panel_final %>%
    left_join(stats_1, by = id1_col)
  
  stats_2 <- network_stats %>%
    filter(t == t_value) %>%
    select(
      id,
      class,
      inclass_n_friends,
      inclass_popularity
    ) %>%
    rename(
      !!id2_col := id,
      !!class_2_col := class,
      !!out_2_col := inclass_n_friends,
      !!pop_2_col := inclass_popularity
    )
  
  panel_final <- panel_final %>%
    left_join(stats_2, by = id2_col)
  
  pair_1 <- network_pair %>%
    filter(t == t_value) %>%
    select(
      id,
      obj_id,
      friend_nomination
    ) %>%
    rename(
      !!id1_col := id,
      !!id2_col := obj_id,
      !!friend_1_col := friend_nomination
    )
  
  panel_final <- panel_final %>%
    left_join(pair_1, by = c(id1_col, id2_col))
  
  pair_2 <- network_pair %>%
    filter(t == t_value) %>%
    select(
      id,
      obj_id,
      friend_nomination
    ) %>%
    rename(
      !!id2_col := id,
      !!id1_col := obj_id,
      !!friend_2_col := friend_nomination
    )
  
  panel_final <- panel_final %>%
    left_join(pair_2, by = c(id1_col, id2_col))
  
  panel_final[[friend_1_col]] <- ifelse(
    is.na(panel_final[[friend_1_col]]),
    0L,
    as.integer(panel_final[[friend_1_col]])
  )
  
  panel_final[[friend_2_col]] <- ifelse(
    is.na(panel_final[[friend_2_col]]),
    0L,
    as.integer(panel_final[[friend_2_col]])
  )
  
  panel_final[[mutual_col]] <- as.integer(
    panel_final[[friend_1_col]] == 1 &
      panel_final[[friend_2_col]] == 1
  )
  
  panel_final[[oneway_col]] <- as.integer(
    panel_final[[friend_1_col]] + panel_final[[friend_2_col]] == 1
  )
  
  panel_final[[none_col]] <- as.integer(
    panel_final[[friend_1_col]] == 0 &
      panel_final[[friend_2_col]] == 0
  )
  
  panel_final[[friendship_col]] <- ifelse(
    panel_final[[mutual_col]] == 1,
    2L,
    ifelse(panel_final[[oneway_col]] == 1, 1L, 0L)
  )
  
  return(panel_final)
}

panel_final <- add_network_time(
  panel_final = panel_final,
  suffix = "base",
  t_value = 0
)

panel_final <- add_network_time(
  panel_final = panel_final,
  suffix = "end",
  t_value = 1
)

panel_final <- panel_final %>%
  mutate(
    inclass_n_friends_max_base = pmax(
      inclass_n_friends_1_base,
      inclass_n_friends_2_base,
      na.rm = TRUE
    ),
    inclass_n_friends_dist_base = abs(
      inclass_n_friends_1_base - inclass_n_friends_2_base
    ),
    inclass_popularity_max_base = pmax(
      inclass_popularity_1_base,
      inclass_popularity_2_base,
      na.rm = TRUE
    ),
    inclass_popularity_dist_base = abs(
      inclass_popularity_1_base - inclass_popularity_2_base
    ),
    inclass_n_friends_max_end = pmax(
      inclass_n_friends_1_end,
      inclass_n_friends_2_end,
      na.rm = TRUE
    ),
    inclass_n_friends_dist_end = abs(
      inclass_n_friends_1_end - inclass_n_friends_2_end
    ),
    inclass_popularity_max_end = pmax(
      inclass_popularity_1_end,
      inclass_popularity_2_end,
      na.rm = TRUE
    ),
    inclass_popularity_dist_end = abs(
      inclass_popularity_1_end - inclass_popularity_2_end
    ),
    class_base = class_1_base,
    class_end = class_1_end
  )

write_dta(panel_final, "data/panel_final.dta")

#####################################################

rm(list = ls())

library(rstudioapi)
library(dplyr)
library(haven)

setwd(dirname(getSourceEditorContext()$path))

panel_final <- read_dta("data/panel_final.dta")

rat_base <- read_dta("data/RAT_score_baseline.dta")
rat_end  <- read_dta("data/RAT_score_endline.dta")


rat_base <- rat_base %>%
  select(
    id,
    RAT_score_generous,
    RAT_score_strict
  ) %>%
  mutate(
    id = as.character(id)
  ) %>%
  distinct(id, .keep_all = TRUE)

rat_end <- rat_end %>%
  select(
    id,
    RAT_score_generous,
    RAT_score_strict
  ) %>%
  mutate(
    id = as.character(id)
  ) %>%
  distinct(id, .keep_all = TRUE)


panel_final <- panel_final %>%
  mutate(
    id_mover_base = as.character(id_mover_base),
    id_nonmover_base = as.character(id_nonmover_base),
    id_mover_end = as.character(id_mover_end),
    id_nonmover_end = as.character(id_nonmover_end)
  )


rat_1_base <- rat_base %>%
  rename(
    id_mover_base = id,
    RAT_score_generous_1_base = RAT_score_generous,
    RAT_score_strict_1_base = RAT_score_strict
  )

panel_final <- panel_final %>%
  left_join(
    rat_1_base,
    by = "id_mover_base"
  )


rat_2_base <- rat_base %>%
  rename(
    id_nonmover_base = id,
    RAT_score_generous_2_base = RAT_score_generous,
    RAT_score_strict_2_base = RAT_score_strict
  )

panel_final <- panel_final %>%
  left_join(
    rat_2_base,
    by = "id_nonmover_base"
  )


rat_1_end <- rat_end %>%
  rename(
    id_mover_end = id,
    RAT_score_generous_1_end = RAT_score_generous,
    RAT_score_strict_1_end = RAT_score_strict
  )

panel_final <- panel_final %>%
  left_join(
    rat_1_end,
    by = "id_mover_end"
  )


rat_2_end <- rat_end %>%
  rename(
    id_nonmover_end = id,
    RAT_score_generous_2_end = RAT_score_generous,
    RAT_score_strict_2_end = RAT_score_strict
  )

panel_final <- panel_final %>%
  left_join(
    rat_2_end,
    by = "id_nonmover_end"
  )


panel_final <- panel_final %>%
  mutate(
    RAT_score_generous_max_base = pmax(
      RAT_score_generous_1_base,
      RAT_score_generous_2_base,
      na.rm = TRUE
    ),
    RAT_score_generous_dist_base = abs(
      RAT_score_generous_1_base - RAT_score_generous_2_base
    ),
    
    RAT_score_strict_max_base = pmax(
      RAT_score_strict_1_base,
      RAT_score_strict_2_base,
      na.rm = TRUE
    ),
    RAT_score_strict_dist_base = abs(
      RAT_score_strict_1_base - RAT_score_strict_2_base
    ),
    
    RAT_score_generous_max_end = pmax(
      RAT_score_generous_1_end,
      RAT_score_generous_2_end,
      na.rm = TRUE
    ),
    RAT_score_generous_dist_end = abs(
      RAT_score_generous_1_end - RAT_score_generous_2_end
    ),
    
    RAT_score_strict_max_end = pmax(
      RAT_score_strict_1_end,
      RAT_score_strict_2_end,
      na.rm = TRUE
    ),
    RAT_score_strict_dist_end = abs(
      RAT_score_strict_1_end - RAT_score_strict_2_end
    )
  )



write_dta(panel_final, "data/panel_final.dta")
