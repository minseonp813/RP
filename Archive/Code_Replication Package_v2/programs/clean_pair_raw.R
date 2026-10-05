## Reusable: keep_cols + clean_pair_raw(), extracted verbatim from 01_calculate_ccei.R
## (used so ex_cross_tests.R can load real data self-containedly, no file writes)
keep_cols <- c(
  "id",
  "partner_id",
  "mover",
  "t",
  "coord_x",
  "coord_y",
  "intercept_x",
  "intercept_y",
  "round_number",
  "game_type"
)

clean_pair_raw <- function(df) {
  df <- df %>%
    mutate(
      id = as.character(id),
      partner_id = as.character(partner_id),
      round_number = as.integer(round_number),
      mover = as.integer(mover)
    )
  df <- df %>%
    group_by(id) %>%
    mutate(
      partner_id = partner_id[round_number == 19][1],
      mover = mover[round_number == 19][1]
    ) %>%
    ungroup()
  df <- df %>%
    mutate(
      big_id = pmax(id, partner_id, na.rm = TRUE),
      small_id = pmin(id, partner_id, na.rm = TRUE),
      group_id = ifelse(
        !is.na(partner_id) & id != partner_id,
        paste0(big_id, small_id),
        NA_character_
      )
    )
  pair_map <- df %>%
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
  df <- df %>%
    select(
      id,
      partner_id,
      mover,
      t,
      coord_x,
      coord_y,
      intercept_x,
      intercept_y,
      round_number,
      game_type,
      group_id
    ) %>%
    rename(old_group_id = group_id)
  df <- df %>%
    left_join(
      pair_map,
      by = c("id" = "big_id")
    ) %>%
    mutate(
      new_group_from_big = group_id,
      partner_from_big = small_id
    ) %>%
    select(-group_id, -small_id)
  df <- df %>%
    left_join(
      pair_map,
      by = c("id" = "small_id")
    ) %>%
    mutate(
      new_group_from_small = group_id,
      partner_from_small = big_id
    ) %>%
    select(-group_id, -big_id)
  df <- df %>%
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
  df <- df %>%
    filter(!is.na(group_id)) %>%
    select(-old_group_id) %>%
    arrange(id, round_number)
  good_groups <- df %>%
    count(group_id, id, name = "n_rows") %>%
    group_by(group_id) %>%
    summarise(
      n_members = n_distinct(id),
      both_have_36 = all(n_rows == 36),
      .groups = "drop"
    ) %>%
    filter(
      n_members == 2,
      both_have_36
    ) %>%
    pull(group_id)
  df <- df %>%
    filter(group_id %in% good_groups) %>%
    arrange(group_id, id, round_number)
  return(df)
}
