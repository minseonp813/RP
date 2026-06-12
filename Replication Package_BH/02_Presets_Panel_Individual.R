#############################################################
# Create panel_individual from panel_final
#
# panel_final:
#   652 rows
#   one row per pair
#   base/end and person 1/person 2 are all in one row
#
# panel_individual:
#   2,608 rows
#   one row per pair-time-person
#
# Variables:
#   id          = my id
#   partner_id  = partner's id
#   group_id
#   endline     = 0 baseline, 1 endline
#   post        = same as endline
#   person      = 1 or 2 within pair-time
#
#   ccei_i      = my individual CCEI
#   ccei_j      = partner's individual CCEI
#   ccei_ig     = my individual+group CCEI
#   ccei_jg     = partner's individual+group CCEI
#   I_ig        = my index
#
#   Same structure for f_ccei, min_mpi, max_mpi, rev_max_mpi
#############################################################

rm(list = ls())

library(rstudioapi)
library(dplyr)
library(haven)

setwd(dirname(getSourceEditorContext()$path))

panel_final <- read_dta("data/panel_final.dta")




make_individual_rows <- function(panel_final, suffix, person_num) {
  
  # suffix: "base" or "end"
  # person_num: 1 or 2
  #
  # In panel_final:
  #   person 1 = id_mover
  #   person 2 = id_nonmover
  
  endline_value <- ifelse(suffix == "end", 1L, 0L)
  
  id1_col <- paste0("id_mover_", suffix)
  id2_col <- paste0("id_nonmover_", suffix)
  
  
  ccei_1_col   <- paste0("ccei_1_", suffix)
  ccei_2_col   <- paste0("ccei_2_", suffix)
  ccei_g_col   <- paste0("ccei_g_", suffix)
  ccei_1g_col  <- paste0("ccei_1g_", suffix)
  ccei_2g_col  <- paste0("ccei_2g_", suffix)
  ccei_hg_col  <- paste0("ccei_hg_", suffix)
  ccei_lg_col  <- paste0("ccei_lg_", suffix)
  ccei_hlg_col <- paste0("ccei_hlg_", suffix)
  high_col     <- paste0("high_", suffix)
  I_hg_col     <- paste0("I_hg_", suffix)
  I_lg_col     <- paste0("I_lg_", suffix)
  
  f_ccei_1_col   <- paste0("f_ccei_1_", suffix)
  f_ccei_2_col   <- paste0("f_ccei_2_", suffix)
  f_ccei_g_col   <- paste0("f_ccei_g_", suffix)
  f_ccei_1g_col  <- paste0("f_ccei_1g_", suffix)
  f_ccei_2g_col  <- paste0("f_ccei_2g_", suffix)
  f_ccei_hg_col  <- paste0("f_ccei_hg_", suffix)
  f_ccei_lg_col  <- paste0("f_ccei_lg_", suffix)
  f_ccei_hlg_col <- paste0("f_ccei_hlg_", suffix)
  f_high_col     <- paste0("f_high_", suffix)
  f_I_hg_col     <- paste0("f_I_hg_", suffix)
  f_I_lg_col     <- paste0("f_I_lg_", suffix)
  
  RA_g_col <- paste0("RA_g_", suffix)
  RA_1_col <- paste0("RA_1_", suffix)
  RA_2_col <- paste0("RA_2_", suffix)
  
  math_1_col <- paste0("mathscore_1_", suffix)
  math_2_col <- paste0("mathscore_2_", suffix)
  
  min_mpi_1_col   <- paste0("min_mpi_1_", suffix)
  min_mpi_2_col   <- paste0("min_mpi_2_", suffix)
  min_mpi_g_col   <- paste0("min_mpi_g_", suffix)
  min_mpi_1g_col  <- paste0("min_mpi_1g_", suffix)
  min_mpi_2g_col  <- paste0("min_mpi_2g_", suffix)
  min_mpi_hg_col  <- paste0("min_mpi_hg_", suffix)
  min_mpi_lg_col  <- paste0("min_mpi_lg_", suffix)
  min_mpi_hlg_col <- paste0("min_mpi_hlg_", suffix)
  min_high_col    <- paste0("min_mpi_high_", suffix)
  I_min_hg_col    <- paste0("I_min_mpi_hg_", suffix)
  I_min_lg_col    <- paste0("I_min_mpi_lg_", suffix)
  
  max_mpi_1_col   <- paste0("max_mpi_1_", suffix)
  max_mpi_2_col   <- paste0("max_mpi_2_", suffix)
  max_mpi_g_col   <- paste0("max_mpi_g_", suffix)
  max_mpi_1g_col  <- paste0("max_mpi_1g_", suffix)
  max_mpi_2g_col  <- paste0("max_mpi_2g_", suffix)
  max_mpi_hg_col  <- paste0("max_mpi_hg_", suffix)
  max_mpi_lg_col  <- paste0("max_mpi_lg_", suffix)
  max_mpi_hlg_col <- paste0("max_mpi_hlg_", suffix)
  max_high_col    <- paste0("max_mpi_high_", suffix)
  I_max_hg_col    <- paste0("I_max_mpi_hg_", suffix)
  I_max_lg_col    <- paste0("I_max_mpi_lg_", suffix)
  
  rev_max_mpi_1_col   <- paste0("rev_max_mpi_1_", suffix)
  rev_max_mpi_2_col   <- paste0("rev_max_mpi_2_", suffix)
  rev_max_mpi_g_col   <- paste0("rev_max_mpi_g_", suffix)
  rev_max_mpi_1g_col  <- paste0("rev_max_mpi_1g_", suffix)
  rev_max_mpi_2g_col  <- paste0("rev_max_mpi_2g_", suffix)
  rev_max_mpi_hg_col  <- paste0("rev_max_mpi_hg_", suffix)
  rev_max_mpi_lg_col  <- paste0("rev_max_mpi_lg_", suffix)
  rev_max_mpi_hlg_col <- paste0("rev_max_mpi_hlg_", suffix)
  I_rev_hg_col        <- paste0("I_rev_max_mpi_hg_", suffix)
  I_rev_lg_col        <- paste0("I_rev_max_mpi_lg_", suffix)
  
  male_1_col <- "male_1_base"
  male_2_col <- "male_2_base"
  
  height_1_col <- "height_1_base"
  height_2_col <- "height_2_base"
  weight_1_col <- "weight_1_base"
  weight_2_col <- "weight_2_base"
  
  class_sleep_1_col <- "class_sleep_1_base"
  class_sleep_2_col <- "class_sleep_2_base"
  class_participate_1_col <- "class_participate_1_base"
  class_participate_2_col <- "class_participate_2_base"
  teacher_prepare_1_col <- "teacher_prepare_1_base"
  teacher_prepare_2_col <- "teacher_prepare_2_base"
  teacher_induce_1_col <- "teacher_induce_1_base"
  teacher_induce_2_col <- "teacher_induce_2_base"
  class_study_1_col <- "class_study_1_base"
  class_study_2_col <- "class_study_2_base"
  class_dislike_1_col <- "class_dislike_1_base"
  class_dislike_2_col <- "class_dislike_2_base"
  class_practical_1_col <- "class_practical_1_base"
  class_practical_2_col <- "class_practical_2_base"
  
  pbl_korean_1_col <- "pbl_korean_1_base"
  pbl_korean_2_col <- "pbl_korean_2_base"
  pbl_eng_1_col <- "pbl_eng_1_base"
  pbl_eng_2_col <- "pbl_eng_2_base"
  pbl_math_1_col <- "pbl_math_1_base"
  pbl_math_2_col <- "pbl_math_2_base"
  pbl_science_1_col <- "pbl_science_1_base"
  pbl_science_2_col <- "pbl_science_2_base"
  pbl_socialsci_1_col <- "pbl_socialsci_1_base"
  pbl_socialsci_2_col <- "pbl_socialsci_2_base"
  
  RAT_generous_1_col <- paste0("RAT_score_generous_1_", suffix)
  RAT_generous_2_col <- paste0("RAT_score_generous_2_", suffix)
  RAT_strict_1_col <- paste0("RAT_score_strict_1_", suffix)
  RAT_strict_2_col <- paste0("RAT_score_strict_2_", suffix)
  
  class_1_col <- paste0("class_1_", suffix)
  class_2_col <- paste0("class_2_", suffix)
  
  inclass_n_friends_1_col <- paste0("inclass_n_friends_1_", suffix)
  inclass_n_friends_2_col <- paste0("inclass_n_friends_2_", suffix)
  inclass_popularity_1_col <- paste0("inclass_popularity_1_", suffix)
  inclass_popularity_2_col <- paste0("inclass_popularity_2_", suffix)
  
  friendship_1_col <- paste0("friendship_1_", suffix)
  friendship_2_col <- paste0("friendship_2_", suffix)
  mutual_friendship_col <- paste0("mutual_friendship_", suffix)
  oneway_friendship_col <- paste0("oneway_friendship_", suffix)
  none_friendship_col <- paste0("none_friendship_", suffix)
  friendship_col <- paste0("friendship_", suffix)
  
  
  
  if (person_num == 1) {
    
    out <- panel_final %>%
      transmute(
        
        group_id = as.character(group_id),
        endline = endline_value,
        post = endline_value,
        time = endline_value,
        person = 1L,
        
        id = as.character(.data[[id1_col]]),
        partner_id = as.character(.data[[id2_col]]),
        
        # By construction, person 1 is mover side in each time period
        mover = 1L,
        
        
        ccei_i = .data[[ccei_1_col]],
        ccei_j = .data[[ccei_2_col]],
        ccei_g = .data[[ccei_g_col]],
        ccei_ig = .data[[ccei_1g_col]],
        ccei_jg = .data[[ccei_2g_col]],
        ccei_hg = .data[[ccei_hg_col]],
        ccei_lg = .data[[ccei_lg_col]],
        ccei_hlg = .data[[ccei_hlg_col]],
        
        HighCCEI = .data[[high_col]],
        I_ig = ifelse(
          .data[[high_col]] == 1,
          .data[[I_hg_col]],
          .data[[I_lg_col]]
        ),
        
        I_hg = .data[[I_hg_col]],
        I_lg = .data[[I_lg_col]],
        
        
        f_ccei_i = .data[[f_ccei_1_col]],
        f_ccei_j = .data[[f_ccei_2_col]],
        f_ccei_g = .data[[f_ccei_g_col]],
        f_ccei_ig = .data[[f_ccei_1g_col]],
        f_ccei_jg = .data[[f_ccei_2g_col]],
        f_ccei_hg = .data[[f_ccei_hg_col]],
        f_ccei_lg = .data[[f_ccei_lg_col]],
        f_ccei_hlg = .data[[f_ccei_hlg_col]],
        
        HighF_CCEI = .data[[f_high_col]],
        f_I_ig = ifelse(
          .data[[f_high_col]] == 1,
          .data[[f_I_hg_col]],
          .data[[f_I_lg_col]]
        ),
        
        f_I_hg = .data[[f_I_hg_col]],
        f_I_lg = .data[[f_I_lg_col]],
        
        
        RA_i = .data[[RA_1_col]],
        RA_j = .data[[RA_2_col]],
        RA_g = .data[[RA_g_col]],
        
        
        mathscore_i = .data[[math_1_col]],
        mathscore_j = .data[[math_2_col]],
        
        
        outgoing_i = outgoing_1_base,
        opened_i = opened_1_base,
        agreeable_i = agreeable_1_base,
        conscientious_i = conscientious_1_base,
        stable_i = stable_1_base,
        
        outgoing_j = outgoing_2_base,
        opened_j = opened_2_base,
        agreeable_j = agreeable_2_base,
        conscientious_j = conscientious_2_base,
        stable_j = stable_2_base,
        
        
        male_i = .data[[male_1_col]],
        male_j = .data[[male_2_col]],
        
        height_i = .data[[height_1_col]],
        height_j = .data[[height_2_col]],
        weight_i = .data[[weight_1_col]],
        weight_j = .data[[weight_2_col]],
        
        
        class_sleep_i = .data[[class_sleep_1_col]],
        class_sleep_j = .data[[class_sleep_2_col]],
        class_participate_i = .data[[class_participate_1_col]],
        class_participate_j = .data[[class_participate_2_col]],
        teacher_prepare_i = .data[[teacher_prepare_1_col]],
        teacher_prepare_j = .data[[teacher_prepare_2_col]],
        teacher_induce_i = .data[[teacher_induce_1_col]],
        teacher_induce_j = .data[[teacher_induce_2_col]],
        class_study_i = .data[[class_study_1_col]],
        class_study_j = .data[[class_study_2_col]],
        class_dislike_i = .data[[class_dislike_1_col]],
        class_dislike_j = .data[[class_dislike_2_col]],
        class_practical_i = .data[[class_practical_1_col]],
        class_practical_j = .data[[class_practical_2_col]],
        
        pbl_korean_i = .data[[pbl_korean_1_col]],
        pbl_korean_j = .data[[pbl_korean_2_col]],
        pbl_eng_i = .data[[pbl_eng_1_col]],
        pbl_eng_j = .data[[pbl_eng_2_col]],
        pbl_math_i = .data[[pbl_math_1_col]],
        pbl_math_j = .data[[pbl_math_2_col]],
        pbl_science_i = .data[[pbl_science_1_col]],
        pbl_science_j = .data[[pbl_science_2_col]],
        pbl_socialsci_i = .data[[pbl_socialsci_1_col]],
        pbl_socialsci_j = .data[[pbl_socialsci_2_col]],
        
        
        RAT_score_generous_i = .data[[RAT_generous_1_col]],
        RAT_score_generous_j = .data[[RAT_generous_2_col]],
        RAT_score_strict_i = .data[[RAT_strict_1_col]],
        RAT_score_strict_j = .data[[RAT_strict_2_col]],
        
        
        class_i = .data[[class_1_col]],
        class_j = .data[[class_2_col]],
        
        inclass_n_friends_i = .data[[inclass_n_friends_1_col]],
        inclass_n_friends_j = .data[[inclass_n_friends_2_col]],
        inclass_popularity_i = .data[[inclass_popularity_1_col]],
        inclass_popularity_j = .data[[inclass_popularity_2_col]],
        
        friendship_i_to_j = .data[[friendship_1_col]],
        friendship_j_to_i = .data[[friendship_2_col]],
        mutual_friendship = .data[[mutual_friendship_col]],
        oneway_friendship = .data[[oneway_friendship_col]],
        oneside_friendship = .data[[oneway_friendship_col]],
        none_friendship = .data[[none_friendship_col]],
        friendship = .data[[friendship_col]],
        
        
        min_mpi_i = .data[[min_mpi_1_col]],
        min_mpi_j = .data[[min_mpi_2_col]],
        min_mpi_g = .data[[min_mpi_g_col]],
        min_mpi_ig = .data[[min_mpi_1g_col]],
        min_mpi_jg = .data[[min_mpi_2g_col]],
        min_mpi_hg = .data[[min_mpi_hg_col]],
        min_mpi_lg = .data[[min_mpi_lg_col]],
        min_mpi_hlg = .data[[min_mpi_hlg_col]],
        
        HighMinMPI = .data[[min_high_col]],
        I_min_mpi_ig = ifelse(
          .data[[min_high_col]] == 1,
          .data[[I_min_hg_col]],
          .data[[I_min_lg_col]]
        ),
        
        I_min_mpi_hg = .data[[I_min_hg_col]],
        I_min_mpi_lg = .data[[I_min_lg_col]],
        
        max_mpi_i = .data[[max_mpi_1_col]],
        max_mpi_j = .data[[max_mpi_2_col]],
        max_mpi_g = .data[[max_mpi_g_col]],
        max_mpi_ig = .data[[max_mpi_1g_col]],
        max_mpi_jg = .data[[max_mpi_2g_col]],
        max_mpi_hg = .data[[max_mpi_hg_col]],
        max_mpi_lg = .data[[max_mpi_lg_col]],
        max_mpi_hlg = .data[[max_mpi_hlg_col]],
        
        HighMaxMPI = .data[[max_high_col]],
        I_max_mpi_ig = ifelse(
          .data[[max_high_col]] == 1,
          .data[[I_max_hg_col]],
          .data[[I_max_lg_col]]
        ),
        
        I_max_mpi_hg = .data[[I_max_hg_col]],
        I_max_mpi_lg = .data[[I_max_lg_col]],
        
        
        rev_max_mpi_i = .data[[rev_max_mpi_1_col]],
        rev_max_mpi_j = .data[[rev_max_mpi_2_col]],
        rev_max_mpi_g = .data[[rev_max_mpi_g_col]],
        rev_max_mpi_ig = .data[[rev_max_mpi_1g_col]],
        rev_max_mpi_jg = .data[[rev_max_mpi_2g_col]],
        rev_max_mpi_hg = .data[[rev_max_mpi_hg_col]],
        rev_max_mpi_lg = .data[[rev_max_mpi_lg_col]],
        rev_max_mpi_hlg = .data[[rev_max_mpi_hlg_col]],
        
        HighRevMaxMPI = .data[[max_high_col]],
        I_rev_max_mpi_ig = ifelse(
          .data[[max_high_col]] == 1,
          .data[[I_rev_hg_col]],
          .data[[I_rev_lg_col]]
        ),
        
        I_rev_max_mpi_hg = .data[[I_rev_hg_col]],
        I_rev_max_mpi_lg = .data[[I_rev_lg_col]]
      )
  }
  
  
  
  if (person_num == 2) {
    
    out <- panel_final %>%
      transmute(
        
        group_id = as.character(group_id),
        endline = endline_value,
        post = endline_value,
        time = endline_value,
        person = 2L,
        
        id = as.character(.data[[id2_col]]),
        partner_id = as.character(.data[[id1_col]]),
        
        # By construction, person 2 is non-mover side in each time period
        mover = 0L,
        
        
        ccei_i = .data[[ccei_2_col]],
        ccei_j = .data[[ccei_1_col]],
        ccei_g = .data[[ccei_g_col]],
        ccei_ig = .data[[ccei_2g_col]],
        ccei_jg = .data[[ccei_1g_col]],
        ccei_hg = .data[[ccei_hg_col]],
        ccei_lg = .data[[ccei_lg_col]],
        ccei_hlg = .data[[ccei_hlg_col]],
        
        HighCCEI = 1 - .data[[high_col]],
        I_ig = ifelse(
          .data[[high_col]] == 1,
          .data[[I_lg_col]],
          .data[[I_hg_col]]
        ),
        
        I_hg = .data[[I_hg_col]],
        I_lg = .data[[I_lg_col]],
        
        
        f_ccei_i = .data[[f_ccei_2_col]],
        f_ccei_j = .data[[f_ccei_1_col]],
        f_ccei_g = .data[[f_ccei_g_col]],
        f_ccei_ig = .data[[f_ccei_2g_col]],
        f_ccei_jg = .data[[f_ccei_1g_col]],
        f_ccei_hg = .data[[f_ccei_hg_col]],
        f_ccei_lg = .data[[f_ccei_lg_col]],
        f_ccei_hlg = .data[[f_ccei_hlg_col]],
        
        HighF_CCEI = 1 - .data[[f_high_col]],
        f_I_ig = ifelse(
          .data[[f_high_col]] == 1,
          .data[[f_I_lg_col]],
          .data[[f_I_hg_col]]
        ),
        
        f_I_hg = .data[[f_I_hg_col]],
        f_I_lg = .data[[f_I_lg_col]],
        
        
        RA_i = .data[[RA_2_col]],
        RA_j = .data[[RA_1_col]],
        RA_g = .data[[RA_g_col]],
        
        
        mathscore_i = .data[[math_2_col]],
        mathscore_j = .data[[math_1_col]],
        
        
        outgoing_i = outgoing_2_base,
        opened_i = opened_2_base,
        agreeable_i = agreeable_2_base,
        conscientious_i = conscientious_2_base,
        stable_i = stable_2_base,
        
        outgoing_j = outgoing_1_base,
        opened_j = opened_1_base,
        agreeable_j = agreeable_1_base,
        conscientious_j = conscientious_1_base,
        stable_j = stable_1_base,
        
        
        male_i = .data[[male_2_col]],
        male_j = .data[[male_1_col]],
        
        height_i = .data[[height_2_col]],
        height_j = .data[[height_1_col]],
        weight_i = .data[[weight_2_col]],
        weight_j = .data[[weight_1_col]],
        
        
        class_sleep_i = .data[[class_sleep_2_col]],
        class_sleep_j = .data[[class_sleep_1_col]],
        class_participate_i = .data[[class_participate_2_col]],
        class_participate_j = .data[[class_participate_1_col]],
        teacher_prepare_i = .data[[teacher_prepare_2_col]],
        teacher_prepare_j = .data[[teacher_prepare_1_col]],
        teacher_induce_i = .data[[teacher_induce_2_col]],
        teacher_induce_j = .data[[teacher_induce_1_col]],
        class_study_i = .data[[class_study_2_col]],
        class_study_j = .data[[class_study_1_col]],
        class_dislike_i = .data[[class_dislike_2_col]],
        class_dislike_j = .data[[class_dislike_1_col]],
        class_practical_i = .data[[class_practical_2_col]],
        class_practical_j = .data[[class_practical_1_col]],
        
        pbl_korean_i = .data[[pbl_korean_2_col]],
        pbl_korean_j = .data[[pbl_korean_1_col]],
        pbl_eng_i = .data[[pbl_eng_2_col]],
        pbl_eng_j = .data[[pbl_eng_1_col]],
        pbl_math_i = .data[[pbl_math_2_col]],
        pbl_math_j = .data[[pbl_math_1_col]],
        pbl_science_i = .data[[pbl_science_2_col]],
        pbl_science_j = .data[[pbl_science_1_col]],
        pbl_socialsci_i = .data[[pbl_socialsci_2_col]],
        pbl_socialsci_j = .data[[pbl_socialsci_1_col]],
        
        
        RAT_score_generous_i = .data[[RAT_generous_2_col]],
        RAT_score_generous_j = .data[[RAT_generous_1_col]],
        RAT_score_strict_i = .data[[RAT_strict_2_col]],
        RAT_score_strict_j = .data[[RAT_strict_1_col]],
        
        
        class_i = .data[[class_2_col]],
        class_j = .data[[class_1_col]],
        
        inclass_n_friends_i = .data[[inclass_n_friends_2_col]],
        inclass_n_friends_j = .data[[inclass_n_friends_1_col]],
        inclass_popularity_i = .data[[inclass_popularity_2_col]],
        inclass_popularity_j = .data[[inclass_popularity_1_col]],
        
        friendship_i_to_j = .data[[friendship_2_col]],
        friendship_j_to_i = .data[[friendship_1_col]],
        mutual_friendship = .data[[mutual_friendship_col]],
        oneway_friendship = .data[[oneway_friendship_col]],
        oneside_friendship = .data[[oneway_friendship_col]],
        none_friendship = .data[[none_friendship_col]],
        friendship = .data[[friendship_col]],
        
        
        min_mpi_i = .data[[min_mpi_2_col]],
        min_mpi_j = .data[[min_mpi_1_col]],
        min_mpi_g = .data[[min_mpi_g_col]],
        min_mpi_ig = .data[[min_mpi_2g_col]],
        min_mpi_jg = .data[[min_mpi_1g_col]],
        min_mpi_hg = .data[[min_mpi_hg_col]],
        min_mpi_lg = .data[[min_mpi_lg_col]],
        min_mpi_hlg = .data[[min_mpi_hlg_col]],
        
        HighMinMPI = 1 - .data[[min_high_col]],
        I_min_mpi_ig = ifelse(
          .data[[min_high_col]] == 1,
          .data[[I_min_lg_col]],
          .data[[I_min_hg_col]]
        ),
        
        I_min_mpi_hg = .data[[I_min_hg_col]],
        I_min_mpi_lg = .data[[I_min_lg_col]],
        
        
        max_mpi_i = .data[[max_mpi_2_col]],
        max_mpi_j = .data[[max_mpi_1_col]],
        max_mpi_g = .data[[max_mpi_g_col]],
        max_mpi_ig = .data[[max_mpi_2g_col]],
        max_mpi_jg = .data[[max_mpi_1g_col]],
        max_mpi_hg = .data[[max_mpi_hg_col]],
        max_mpi_lg = .data[[max_mpi_lg_col]],
        max_mpi_hlg = .data[[max_mpi_hlg_col]],
        
        HighMaxMPI = 1 - .data[[max_high_col]],
        I_max_mpi_ig = ifelse(
          .data[[max_high_col]] == 1,
          .data[[I_max_lg_col]],
          .data[[I_max_hg_col]]
        ),
        
        I_max_mpi_hg = .data[[I_max_hg_col]],
        I_max_mpi_lg = .data[[I_max_lg_col]],
        
        rev_max_mpi_i = .data[[rev_max_mpi_2_col]],
        rev_max_mpi_j = .data[[rev_max_mpi_1_col]],
        rev_max_mpi_g = .data[[rev_max_mpi_g_col]],
        rev_max_mpi_ig = .data[[rev_max_mpi_2g_col]],
        rev_max_mpi_jg = .data[[rev_max_mpi_1g_col]],
        rev_max_mpi_hg = .data[[rev_max_mpi_hg_col]],
        rev_max_mpi_lg = .data[[rev_max_mpi_lg_col]],
        rev_max_mpi_hlg = .data[[rev_max_mpi_hlg_col]],
        
        HighRevMaxMPI = 1 - .data[[max_high_col]],
        I_rev_max_mpi_ig = ifelse(
          .data[[max_high_col]] == 1,
          .data[[I_rev_lg_col]],
          .data[[I_rev_hg_col]]
        ),
        
        I_rev_max_mpi_hg = .data[[I_rev_hg_col]],
        I_rev_max_mpi_lg = .data[[I_rev_lg_col]]
      )
  }
  
  return(out)
}


panel_individual <- bind_rows(
  make_individual_rows(panel_final, suffix = "base", person_num = 1),
  make_individual_rows(panel_final, suffix = "base", person_num = 2),
  make_individual_rows(panel_final, suffix = "end",  person_num = 1),
  make_individual_rows(panel_final, suffix = "end",  person_num = 2)
) %>%
  arrange(group_id, endline, person)



panel_individual <- panel_individual %>%
  mutate(
    
    ccei_diff = ccei_i - ccei_j,
    ccei_dist = abs(ccei_i - ccei_j),
    ccei_pair_max = pmax(ccei_i, ccei_j, na.rm = TRUE),
    ccei_pair_min = pmin(ccei_i, ccei_j, na.rm = TRUE),
    
    ccei_ig_diff = ccei_ig - ccei_jg,
    ccei_ig_dist = abs(ccei_ig - ccei_jg),
    
    HighCCEI_post = HighCCEI * post,
    
    
    f_ccei_diff = f_ccei_i - f_ccei_j,
    f_ccei_dist = abs(f_ccei_i - f_ccei_j),
    f_ccei_pair_max = pmax(f_ccei_i, f_ccei_j, na.rm = TRUE),
    f_ccei_pair_min = pmin(f_ccei_i, f_ccei_j, na.rm = TRUE),
    
    f_ccei_ig_diff = f_ccei_ig - f_ccei_jg,
    f_ccei_ig_dist = abs(f_ccei_ig - f_ccei_jg),
    
    HighF_CCEI_post = HighF_CCEI * post,
    
    
    min_mpi_diff = min_mpi_i - min_mpi_j,
    min_mpi_dist = abs(min_mpi_i - min_mpi_j),
    min_mpi_pair_lowest = pmin(min_mpi_i, min_mpi_j, na.rm = TRUE),
    min_mpi_pair_highest = pmax(min_mpi_i, min_mpi_j, na.rm = TRUE),
    
    max_mpi_diff = max_mpi_i - max_mpi_j,
    max_mpi_dist = abs(max_mpi_i - max_mpi_j),
    max_mpi_pair_lowest = pmin(max_mpi_i, max_mpi_j, na.rm = TRUE),
    max_mpi_pair_highest = pmax(max_mpi_i, max_mpi_j, na.rm = TRUE),
    
    
    rev_max_mpi_diff = rev_max_mpi_i - rev_max_mpi_j,
    rev_max_mpi_dist = abs(rev_max_mpi_i - rev_max_mpi_j),
    rev_max_mpi_pair_max = pmax(rev_max_mpi_i, rev_max_mpi_j, na.rm = TRUE),
    rev_max_mpi_pair_min = pmin(rev_max_mpi_i, rev_max_mpi_j, na.rm = TRUE),
    
    HighMinMPI_post = HighMinMPI * post,
    HighMaxMPI_post = HighMaxMPI * post,
    HighRevMaxMPI_post = HighRevMaxMPI * post,
    
    
    RA_diff = RA_i - RA_j,
    RA_dist = abs(RA_i - RA_j),
    
    
    math_diff = mathscore_i - mathscore_j,
    math_dist = abs(mathscore_i - mathscore_j),
    
    
    outgoing_diff = outgoing_i - outgoing_j,
    opened_diff = opened_i - opened_j,
    agreeable_diff = agreeable_i - agreeable_j,
    conscientious_diff = conscientious_i - conscientious_j,
    stable_diff = stable_i - stable_j,
    
    
    male_diff = male_i - male_j,
    male_dist = abs(male_i - male_j),
    malepair = male_i + male_j,
    
    height_diff = height_i - height_j,
    height_dist = abs(height_i - height_j),
    
    weight_diff = weight_i - weight_j,
    weight_dist = abs(weight_i - weight_j),
    
    
    class_sleep_diff = class_sleep_i - class_sleep_j,
    class_sleep_dist = abs(class_sleep_i - class_sleep_j),
    class_participate_diff = class_participate_i - class_participate_j,
    class_participate_dist = abs(class_participate_i - class_participate_j),
    teacher_prepare_diff = teacher_prepare_i - teacher_prepare_j,
    teacher_prepare_dist = abs(teacher_prepare_i - teacher_prepare_j),
    teacher_induce_diff = teacher_induce_i - teacher_induce_j,
    teacher_induce_dist = abs(teacher_induce_i - teacher_induce_j),
    class_study_diff = class_study_i - class_study_j,
    class_study_dist = abs(class_study_i - class_study_j),
    class_dislike_diff = class_dislike_i - class_dislike_j,
    class_dislike_dist = abs(class_dislike_i - class_dislike_j),
    class_practical_diff = class_practical_i - class_practical_j,
    class_practical_dist = abs(class_practical_i - class_practical_j),
    
    pbl_korean_diff = pbl_korean_i - pbl_korean_j,
    pbl_korean_dist = abs(pbl_korean_i - pbl_korean_j),
    pbl_eng_diff = pbl_eng_i - pbl_eng_j,
    pbl_eng_dist = abs(pbl_eng_i - pbl_eng_j),
    pbl_math_diff = pbl_math_i - pbl_math_j,
    pbl_math_dist = abs(pbl_math_i - pbl_math_j),
    pbl_science_diff = pbl_science_i - pbl_science_j,
    pbl_science_dist = abs(pbl_science_i - pbl_science_j),
    pbl_socialsci_diff = pbl_socialsci_i - pbl_socialsci_j,
    pbl_socialsci_dist = abs(pbl_socialsci_i - pbl_socialsci_j),
    
    
    RAT_score_generous_diff = RAT_score_generous_i - RAT_score_generous_j,
    RAT_score_generous_dist = abs(RAT_score_generous_i - RAT_score_generous_j),
    RAT_score_strict_diff = RAT_score_strict_i - RAT_score_strict_j,
    RAT_score_strict_dist = abs(RAT_score_strict_i - RAT_score_strict_j),
    
    
    inclass_n_diff = inclass_n_friends_i - inclass_n_friends_j,
    inclass_n_dist = abs(inclass_n_friends_i - inclass_n_friends_j),
    inclass_pop_diff = inclass_popularity_i - inclass_popularity_j,
    inclass_pop_dist = abs(inclass_popularity_i - inclass_popularity_j)
  )




panel_individual <- panel_individual %>%
  mutate(
    
    new2_I_ig = I_ig,
    
    
    new2_f_I_ig = f_I_ig,
    
    new2_I_min_mpi_ig = I_min_mpi_ig,
    new2_I_max_mpi_ig = I_max_mpi_ig,
    new2_I_rev_max_mpi_ig = I_rev_max_mpi_ig,
    
    
    rmpi_i = rev_max_mpi_i,
    rmpi_j = rev_max_mpi_j,
    rmpi_g = rev_max_mpi_g,
    rmpi_ig = rev_max_mpi_ig,
    rmpi_jg = rev_max_mpi_jg,
    I_rmpi_ig = I_rev_max_mpi_ig,
    HighRMPI = HighRevMaxMPI,
    HighRMPI_post = HighRevMaxMPI_post,
    
    HighCCEI_post = HighCCEI * post,
    HighF_CCEI_post = HighF_CCEI * post,
    HighRMPI_post = HighRMPI * post
  )




write_dta(panel_individual, "data/panel_individual.dta")
