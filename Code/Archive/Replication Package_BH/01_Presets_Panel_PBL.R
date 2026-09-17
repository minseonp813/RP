#############################################################
# Create panel_pbl from panel_final
# 
# panel_final:
#   652 rows, one row per pair
#   baseline/endline information are in the same row
#
# panel_pbl:
#   1,304 rows, one row per pair-time
#   endline = 0 for baseline
#   endline = 1 for endline
#############################################################

rm(list = ls())

library(rstudioapi)
library(dplyr)
library(haven)

setwd(dirname(getSourceEditorContext()$path))

panel_final <- read_dta("data/panel_final.dta")



panel_pbl_base <- panel_final %>%
  transmute(
    group_id = as.character(group_id),
    endline = 0L,
    
    
    id_1 = as.character(id_mover_base),
    id_2 = as.character(id_nonmover_base),
    
    
    ccei_1   = ccei_1_base,
    ccei_2   = ccei_2_base,
    ccei_g   = ccei_g_base,
    ccei_1g  = ccei_1g_base,
    ccei_2g  = ccei_2g_base,
    ccei_hg  = ccei_hg_base,
    ccei_lg  = ccei_lg_base,
    ccei_hlg = ccei_hlg_base,
    
    high_ccei = high_base,
    I_hg = I_hg_base,
    I_lg = I_lg_base,
    
    
    RA_g = RA_g_base,
    RA_1 = RA_1_base,
    RA_2 = RA_2_base,
    
    
    mathscore_1 = mathscore_1_base,
    mathscore_2 = mathscore_2_base,
    
    
    outgoing_1 = outgoing_1_base,
    opened_1 = opened_1_base,
    agreeable_1 = agreeable_1_base,
    conscientious_1 = conscientious_1_base,
    stable_1 = stable_1_base,
    
    outgoing_2 = outgoing_2_base,
    opened_2 = opened_2_base,
    agreeable_2 = agreeable_2_base,
    conscientious_2 = conscientious_2_base,
    stable_2 = stable_2_base,
    
    
    male_1 = male_1_base,
    male_2 = male_2_base,
    
    height_1 = height_1_base,
    height_2 = height_2_base,
    weight_1 = weight_1_base,
    weight_2 = weight_2_base,
    
    
    class_sleep_1 = class_sleep_1_base,
    class_participate_1 = class_participate_1_base,
    teacher_prepare_1 = teacher_prepare_1_base,
    teacher_induce_1 = teacher_induce_1_base,
    class_study_1 = class_study_1_base,
    class_dislike_1 = class_dislike_1_base,
    class_practical_1 = class_practical_1_base,
    pbl_korean_1 = pbl_korean_1_base,
    pbl_eng_1 = pbl_eng_1_base,
    pbl_math_1 = pbl_math_1_base,
    pbl_science_1 = pbl_science_1_base,
    pbl_socialsci_1 = pbl_socialsci_1_base,
    
    class_sleep_2 = class_sleep_2_base,
    class_participate_2 = class_participate_2_base,
    teacher_prepare_2 = teacher_prepare_2_base,
    teacher_induce_2 = teacher_induce_2_base,
    class_study_2 = class_study_2_base,
    class_dislike_2 = class_dislike_2_base,
    class_practical_2 = class_practical_2_base,
    pbl_korean_2 = pbl_korean_2_base,
    pbl_eng_2 = pbl_eng_2_base,
    pbl_math_2 = pbl_math_2_base,
    pbl_science_2 = pbl_science_2_base,
    pbl_socialsci_2 = pbl_socialsci_2_base,
    
    
    RAT_score_generous_1 = RAT_score_generous_1_base,
    RAT_score_generous_2 = RAT_score_generous_2_base,
    RAT_score_strict_1 = RAT_score_strict_1_base,
    RAT_score_strict_2 = RAT_score_strict_2_base,
    
    RAT_score_generous_max = RAT_score_generous_max_base,
    RAT_score_generous_dist = RAT_score_generous_dist_base,
    RAT_score_strict_max = RAT_score_strict_max_base,
    RAT_score_strict_dist = RAT_score_strict_dist_base,
    
    
    class_1 = class_1_base,
    class_2 = class_2_base,
    class = class_base,
    
    inclass_n_friends_1 = inclass_n_friends_1_base,
    inclass_n_friends_2 = inclass_n_friends_2_base,
    inclass_n_friends_max = inclass_n_friends_max_base,
    inclass_n_friends_dist = inclass_n_friends_dist_base,
    
    inclass_popularity_1 = inclass_popularity_1_base,
    inclass_popularity_2 = inclass_popularity_2_base,
    inclass_popularity_max = inclass_popularity_max_base,
    inclass_popularity_dist = inclass_popularity_dist_base,
    
    friendship_1 = friendship_1_base,
    friendship_2 = friendship_2_base,
    mutual_friendship = mutual_friendship_base,
    oneway_friendship = oneway_friendship_base,
    oneside_friendship = oneway_friendship_base,
    none_friendship = none_friendship_base,
    friendship = friendship_base,
    
    
    f_ccei_1   = f_ccei_1_base,
    f_ccei_2   = f_ccei_2_base,
    f_ccei_g   = f_ccei_g_base,
    f_ccei_1g  = f_ccei_1g_base,
    f_ccei_2g  = f_ccei_2g_base,
    f_ccei_hg  = f_ccei_hg_base,
    f_ccei_lg  = f_ccei_lg_base,
    f_ccei_hlg = f_ccei_hlg_base,
    
    f_high_ccei = f_high_base,
    f_I_hg = f_I_hg_base,
    f_I_lg = f_I_lg_base,
    
    
    min_mpi_1   = min_mpi_1_base,
    min_mpi_2   = min_mpi_2_base,
    min_mpi_g   = min_mpi_g_base,
    min_mpi_1g  = min_mpi_1g_base,
    min_mpi_2g  = min_mpi_2g_base,
    min_mpi_hg  = min_mpi_hg_base,
    min_mpi_lg  = min_mpi_lg_base,
    min_mpi_hlg = min_mpi_hlg_base,
    
    min_mpi_high = min_mpi_high_base,
    I_min_mpi_hg = I_min_mpi_hg_base,
    I_min_mpi_lg = I_min_mpi_lg_base,
    
    
    max_mpi_1   = max_mpi_1_base,
    max_mpi_2   = max_mpi_2_base,
    max_mpi_g   = max_mpi_g_base,
    max_mpi_1g  = max_mpi_1g_base,
    max_mpi_2g  = max_mpi_2g_base,
    max_mpi_hg  = max_mpi_hg_base,
    max_mpi_lg  = max_mpi_lg_base,
    max_mpi_hlg = max_mpi_hlg_base,
    
    max_mpi_high = max_mpi_high_base,
    I_max_mpi_hg = I_max_mpi_hg_base,
    I_max_mpi_lg = I_max_mpi_lg_base,
    
    #########################################################
    # MPI: rev max = 1 - max MPI
    #########################################################
    rev_max_mpi_1   = rev_max_mpi_1_base,
    rev_max_mpi_2   = rev_max_mpi_2_base,
    rev_max_mpi_g   = rev_max_mpi_g_base,
    rev_max_mpi_1g  = rev_max_mpi_1g_base,
    rev_max_mpi_2g  = rev_max_mpi_2g_base,
    rev_max_mpi_hg  = rev_max_mpi_hg_base,
    rev_max_mpi_lg  = rev_max_mpi_lg_base,
    rev_max_mpi_hlg = rev_max_mpi_hlg_base,
    
    rev_max_mpi_high = rev_max_mpi_high_base,
    I_rev_max_mpi_hg = I_rev_max_mpi_hg_base,
    I_rev_max_mpi_lg = I_rev_max_mpi_lg_base
  )




panel_pbl_end <- panel_final %>%
  transmute(
    group_id = as.character(group_id),
    endline = 1L,
    
    
    id_1 = as.character(id_mover_end),
    id_2 = as.character(id_nonmover_end),
    
    
    ccei_1   = ccei_1_end,
    ccei_2   = ccei_2_end,
    ccei_g   = ccei_g_end,
    ccei_1g  = ccei_1g_end,
    ccei_2g  = ccei_2g_end,
    ccei_hg  = ccei_hg_end,
    ccei_lg  = ccei_lg_end,
    ccei_hlg = ccei_hlg_end,
    
    high_ccei = high_end,
    I_hg = I_hg_end,
    I_lg = I_lg_end,
    
    
    RA_g = RA_g_end,
    RA_1 = RA_1_end,
    RA_2 = RA_2_end,
    
    
    mathscore_1 = mathscore_1_end,
    mathscore_2 = mathscore_2_end,
    
    
    outgoing_1 = outgoing_1_base,
    opened_1 = opened_1_base,
    agreeable_1 = agreeable_1_base,
    conscientious_1 = conscientious_1_base,
    stable_1 = stable_1_base,
    
    outgoing_2 = outgoing_2_base,
    opened_2 = opened_2_base,
    agreeable_2 = agreeable_2_base,
    conscientious_2 = conscientious_2_base,
    stable_2 = stable_2_base,
    
    
    male_1 = male_1_base,
    male_2 = male_2_base,
    
    height_1 = height_1_base,
    height_2 = height_2_base,
    weight_1 = weight_1_base,
    weight_2 = weight_2_base,
    
    
    class_sleep_1 = class_sleep_1_base,
    class_participate_1 = class_participate_1_base,
    teacher_prepare_1 = teacher_prepare_1_base,
    teacher_induce_1 = teacher_induce_1_base,
    class_study_1 = class_study_1_base,
    class_dislike_1 = class_dislike_1_base,
    class_practical_1 = class_practical_1_base,
    pbl_korean_1 = pbl_korean_1_base,
    pbl_eng_1 = pbl_eng_1_base,
    pbl_math_1 = pbl_math_1_base,
    pbl_science_1 = pbl_science_1_base,
    pbl_socialsci_1 = pbl_socialsci_1_base,
    
    class_sleep_2 = class_sleep_2_base,
    class_participate_2 = class_participate_2_base,
    teacher_prepare_2 = teacher_prepare_2_base,
    teacher_induce_2 = teacher_induce_2_base,
    class_study_2 = class_study_2_base,
    class_dislike_2 = class_dislike_2_base,
    class_practical_2 = class_practical_2_base,
    pbl_korean_2 = pbl_korean_2_base,
    pbl_eng_2 = pbl_eng_2_base,
    pbl_math_2 = pbl_math_2_base,
    pbl_science_2 = pbl_science_2_base,
    pbl_socialsci_2 = pbl_socialsci_2_base,
    
    
    RAT_score_generous_1 = RAT_score_generous_1_end,
    RAT_score_generous_2 = RAT_score_generous_2_end,
    RAT_score_strict_1 = RAT_score_strict_1_end,
    RAT_score_strict_2 = RAT_score_strict_2_end,
    
    RAT_score_generous_max = RAT_score_generous_max_end,
    RAT_score_generous_dist = RAT_score_generous_dist_end,
    RAT_score_strict_max = RAT_score_strict_max_end,
    RAT_score_strict_dist = RAT_score_strict_dist_end,
    
    
    class_1 = class_1_end,
    class_2 = class_2_end,
    class = class_end,
    
    inclass_n_friends_1 = inclass_n_friends_1_end,
    inclass_n_friends_2 = inclass_n_friends_2_end,
    inclass_n_friends_max = inclass_n_friends_max_end,
    inclass_n_friends_dist = inclass_n_friends_dist_end,
    
    inclass_popularity_1 = inclass_popularity_1_end,
    inclass_popularity_2 = inclass_popularity_2_end,
    inclass_popularity_max = inclass_popularity_max_end,
    inclass_popularity_dist = inclass_popularity_dist_end,
    
    friendship_1 = friendship_1_end,
    friendship_2 = friendship_2_end,
    mutual_friendship = mutual_friendship_end,
    oneway_friendship = oneway_friendship_end,
    oneside_friendship = oneway_friendship_end,
    none_friendship = none_friendship_end,
    friendship = friendship_end,
    
    
    f_ccei_1   = f_ccei_1_end,
    f_ccei_2   = f_ccei_2_end,
    f_ccei_g   = f_ccei_g_end,
    f_ccei_1g  = f_ccei_1g_end,
    f_ccei_2g  = f_ccei_2g_end,
    f_ccei_hg  = f_ccei_hg_end,
    f_ccei_lg  = f_ccei_lg_end,
    f_ccei_hlg = f_ccei_hlg_end,
    
    f_high_ccei = f_high_end,
    f_I_hg = f_I_hg_end,
    f_I_lg = f_I_lg_end,
    
    
    min_mpi_1   = min_mpi_1_end,
    min_mpi_2   = min_mpi_2_end,
    min_mpi_g   = min_mpi_g_end,
    min_mpi_1g  = min_mpi_1g_end,
    min_mpi_2g  = min_mpi_2g_end,
    min_mpi_hg  = min_mpi_hg_end,
    min_mpi_lg  = min_mpi_lg_end,
    min_mpi_hlg = min_mpi_hlg_end,
    
    min_mpi_high = min_mpi_high_end,
    I_min_mpi_hg = I_min_mpi_hg_end,
    I_min_mpi_lg = I_min_mpi_lg_end,
    
    
    max_mpi_1   = max_mpi_1_end,
    max_mpi_2   = max_mpi_2_end,
    max_mpi_g   = max_mpi_g_end,
    max_mpi_1g  = max_mpi_1g_end,
    max_mpi_2g  = max_mpi_2g_end,
    max_mpi_hg  = max_mpi_hg_end,
    max_mpi_lg  = max_mpi_lg_end,
    max_mpi_hlg = max_mpi_hlg_end,
    
    max_mpi_high = max_mpi_high_end,
    I_max_mpi_hg = I_max_mpi_hg_end,
    I_max_mpi_lg = I_max_mpi_lg_end,
    
    
    rev_max_mpi_1   = rev_max_mpi_1_end,
    rev_max_mpi_2   = rev_max_mpi_2_end,
    rev_max_mpi_g   = rev_max_mpi_g_end,
    rev_max_mpi_1g  = rev_max_mpi_1g_end,
    rev_max_mpi_2g  = rev_max_mpi_2g_end,
    rev_max_mpi_hg  = rev_max_mpi_hg_end,
    rev_max_mpi_lg  = rev_max_mpi_lg_end,
    rev_max_mpi_hlg = rev_max_mpi_hlg_end,
    
    rev_max_mpi_high = rev_max_mpi_high_end,
    I_rev_max_mpi_hg = I_rev_max_mpi_hg_end,
    I_rev_max_mpi_lg = I_rev_max_mpi_lg_end
  )




panel_pbl <- bind_rows(
  panel_pbl_base,
  panel_pbl_end
) %>%
  arrange(group_id, endline)


#############################################################
# pair-level variables
#############################################################

panel_pbl <- panel_pbl %>%
  mutate(
    
    ccei_ind_max  = pmax(ccei_1, ccei_2, na.rm = TRUE),
    ccei_ind_min  = pmin(ccei_1, ccei_2, na.rm = TRUE),
    ccei_ind_dist = abs(ccei_1 - ccei_2),
    
    ccei_ig_max  = pmax(ccei_1g, ccei_2g, na.rm = TRUE),
    ccei_ig_min  = pmin(ccei_1g, ccei_2g, na.rm = TRUE),
    ccei_ig_dist = abs(ccei_1g - ccei_2g),
    
    
    f_ccei_ind_max  = pmax(f_ccei_1, f_ccei_2, na.rm = TRUE),
    f_ccei_ind_min  = pmin(f_ccei_1, f_ccei_2, na.rm = TRUE),
    f_ccei_ind_dist = abs(f_ccei_1 - f_ccei_2),
    
    f_ccei_ig_max  = pmax(f_ccei_1g, f_ccei_2g, na.rm = TRUE),
    f_ccei_ig_min  = pmin(f_ccei_1g, f_ccei_2g, na.rm = TRUE),
    f_ccei_ig_dist = abs(f_ccei_1g - f_ccei_2g),
    
    
    RA_ind_max  = pmax(RA_1, RA_2, na.rm = TRUE),
    RA_ind_min  = pmin(RA_1, RA_2, na.rm = TRUE),
    RA_ind_dist = abs(RA_1 - RA_2),
    
    
    mathscore_max  = pmax(mathscore_1, mathscore_2, na.rm = TRUE),
    mathscore_min  = pmin(mathscore_1, mathscore_2, na.rm = TRUE),
    mathscore_dist = abs(mathscore_1 - mathscore_2),
    
    
    outgoing_max  = pmax(outgoing_1, outgoing_2, na.rm = TRUE),
    outgoing_min  = pmin(outgoing_1, outgoing_2, na.rm = TRUE),
    outgoing_dist = abs(outgoing_1 - outgoing_2),
    
    opened_max  = pmax(opened_1, opened_2, na.rm = TRUE),
    opened_min  = pmin(opened_1, opened_2, na.rm = TRUE),
    opened_dist = abs(opened_1 - opened_2),
    
    agreeable_max  = pmax(agreeable_1, agreeable_2, na.rm = TRUE),
    agreeable_min  = pmin(agreeable_1, agreeable_2, na.rm = TRUE),
    agreeable_dist = abs(agreeable_1 - agreeable_2),
    
    conscientious_max  = pmax(conscientious_1, conscientious_2, na.rm = TRUE),
    conscientious_min  = pmin(conscientious_1, conscientious_2, na.rm = TRUE),
    conscientious_dist = abs(conscientious_1 - conscientious_2),
    
    stable_max  = pmax(stable_1, stable_2, na.rm = TRUE),
    stable_min  = pmin(stable_1, stable_2, na.rm = TRUE),
    stable_dist = abs(stable_1 - stable_2),
    
    
    male_max = pmax(male_1, male_2, na.rm = TRUE),
    male_min = pmin(male_1, male_2, na.rm = TRUE),
    male_dist = abs(male_1 - male_2),
    malepair = male_1 + male_2,
    
    height_max = pmax(height_1, height_2, na.rm = TRUE),
    height_min = pmin(height_1, height_2, na.rm = TRUE),
    height_dist = abs(height_1 - height_2),
    
    weight_max = pmax(weight_1, weight_2, na.rm = TRUE),
    weight_min = pmin(weight_1, weight_2, na.rm = TRUE),
    weight_dist = abs(weight_1 - weight_2),
    
    
    class_sleep_max = pmax(class_sleep_1, class_sleep_2, na.rm = TRUE),
    class_sleep_min = pmin(class_sleep_1, class_sleep_2, na.rm = TRUE),
    class_sleep_dist = abs(class_sleep_1 - class_sleep_2),
    
    class_participate_max = pmax(class_participate_1, class_participate_2, na.rm = TRUE),
    class_participate_min = pmin(class_participate_1, class_participate_2, na.rm = TRUE),
    class_participate_dist = abs(class_participate_1 - class_participate_2),
    
    teacher_prepare_max = pmax(teacher_prepare_1, teacher_prepare_2, na.rm = TRUE),
    teacher_prepare_min = pmin(teacher_prepare_1, teacher_prepare_2, na.rm = TRUE),
    teacher_prepare_dist = abs(teacher_prepare_1 - teacher_prepare_2),
    
    teacher_induce_max = pmax(teacher_induce_1, teacher_induce_2, na.rm = TRUE),
    teacher_induce_min = pmin(teacher_induce_1, teacher_induce_2, na.rm = TRUE),
    teacher_induce_dist = abs(teacher_induce_1 - teacher_induce_2),
    
    class_study_max = pmax(class_study_1, class_study_2, na.rm = TRUE),
    class_study_min = pmin(class_study_1, class_study_2, na.rm = TRUE),
    class_study_dist = abs(class_study_1 - class_study_2),
    
    class_dislike_max = pmax(class_dislike_1, class_dislike_2, na.rm = TRUE),
    class_dislike_min = pmin(class_dislike_1, class_dislike_2, na.rm = TRUE),
    class_dislike_dist = abs(class_dislike_1 - class_dislike_2),
    
    class_practical_max = pmax(class_practical_1, class_practical_2, na.rm = TRUE),
    class_practical_min = pmin(class_practical_1, class_practical_2, na.rm = TRUE),
    class_practical_dist = abs(class_practical_1 - class_practical_2),
    
    pbl_korean_max = pmax(pbl_korean_1, pbl_korean_2, na.rm = TRUE),
    pbl_korean_min = pmin(pbl_korean_1, pbl_korean_2, na.rm = TRUE),
    pbl_korean_dist = abs(pbl_korean_1 - pbl_korean_2),
    
    pbl_eng_max = pmax(pbl_eng_1, pbl_eng_2, na.rm = TRUE),
    pbl_eng_min = pmin(pbl_eng_1, pbl_eng_2, na.rm = TRUE),
    pbl_eng_dist = abs(pbl_eng_1 - pbl_eng_2),
    
    pbl_math_max = pmax(pbl_math_1, pbl_math_2, na.rm = TRUE),
    pbl_math_min = pmin(pbl_math_1, pbl_math_2, na.rm = TRUE),
    pbl_math_dist = abs(pbl_math_1 - pbl_math_2),
    
    pbl_science_max = pmax(pbl_science_1, pbl_science_2, na.rm = TRUE),
    pbl_science_min = pmin(pbl_science_1, pbl_science_2, na.rm = TRUE),
    pbl_science_dist = abs(pbl_science_1 - pbl_science_2),
    
    pbl_socialsci_max = pmax(pbl_socialsci_1, pbl_socialsci_2, na.rm = TRUE),
    pbl_socialsci_min = pmin(pbl_socialsci_1, pbl_socialsci_2, na.rm = TRUE),
    pbl_socialsci_dist = abs(pbl_socialsci_1 - pbl_socialsci_2),
    
    
    min_mpi_ind_lowest = pmin(min_mpi_1, min_mpi_2, na.rm = TRUE),
    min_mpi_ind_highest = pmax(min_mpi_1, min_mpi_2, na.rm = TRUE),
    min_mpi_ind_dist = abs(min_mpi_1 - min_mpi_2),
    
    min_mpi_ig_lowest = pmin(min_mpi_1g, min_mpi_2g, na.rm = TRUE),
    min_mpi_ig_highest = pmax(min_mpi_1g, min_mpi_2g, na.rm = TRUE),
    min_mpi_ig_dist = abs(min_mpi_1g - min_mpi_2g),
    
    
    max_mpi_ind_lowest = pmin(max_mpi_1, max_mpi_2, na.rm = TRUE),
    max_mpi_ind_highest = pmax(max_mpi_1, max_mpi_2, na.rm = TRUE),
    max_mpi_ind_dist = abs(max_mpi_1 - max_mpi_2),
    
    max_mpi_ig_lowest = pmin(max_mpi_1g, max_mpi_2g, na.rm = TRUE),
    max_mpi_ig_highest = pmax(max_mpi_1g, max_mpi_2g, na.rm = TRUE),
    max_mpi_ig_dist = abs(max_mpi_1g - max_mpi_2g),
    
    
    rev_max_mpi_ind_max  = pmax(rev_max_mpi_1, rev_max_mpi_2, na.rm = TRUE),
    rev_max_mpi_ind_min  = pmin(rev_max_mpi_1, rev_max_mpi_2, na.rm = TRUE),
    rev_max_mpi_ind_dist = abs(rev_max_mpi_1 - rev_max_mpi_2),
    
    rev_max_mpi_ig_max  = pmax(rev_max_mpi_1g, rev_max_mpi_2g, na.rm = TRUE),
    rev_max_mpi_ig_min  = pmin(rev_max_mpi_1g, rev_max_mpi_2g, na.rm = TRUE),
    rev_max_mpi_ig_dist = abs(rev_max_mpi_1g - rev_max_mpi_2g),
    
    end_ccei_ind_max  = endline * ccei_ind_max,
    end_ccei_ind_dist = endline * ccei_ind_dist,
    
    end_f_ccei_ind_max  = endline * f_ccei_ind_max,
    end_f_ccei_ind_dist = endline * f_ccei_ind_dist,
    
    end_rev_max_mpi_ind_max  = endline * rev_max_mpi_ind_max,
    end_rev_max_mpi_ind_dist = endline * rev_max_mpi_ind_dist,
    
    end_mathscore_max = endline * mathscore_max,
    end_mathscore_dist = endline * mathscore_dist,
    
    end_height_max = endline * height_max,
    end_height_dist = endline * height_dist,
    
    end_weight_max = endline * weight_max,
    end_weight_dist = endline * weight_dist,
    
    end_inclass_n_friends_max = endline * inclass_n_friends_max,
    end_inclass_n_friends_dist = endline * inclass_n_friends_dist,
    
    end_inclass_popularity_max = endline * inclass_popularity_max,
    end_inclass_popularity_dist = endline * inclass_popularity_dist,
    
    end_RAT_score_generous_max = endline * RAT_score_generous_max,
    end_RAT_score_generous_dist = endline * RAT_score_generous_dist,
    end_RAT_score_strict_max = endline * RAT_score_strict_max,
    end_RAT_score_strict_dist = endline * RAT_score_strict_dist
  )




panel_pbl <- panel_pbl %>%
  mutate(
    time = endline,
    
    ccei_max = ccei_ind_max,
    ccei_dist = ccei_ind_dist,
    end_max = end_ccei_ind_max,
    end_dist = end_ccei_ind_dist,
    
    f_ccei_max = f_ccei_ind_max,
    f_ccei_dist = f_ccei_ind_dist,
    
    rev_max_mpi_max = rev_max_mpi_ind_max,
    rev_max_mpi_dist = rev_max_mpi_ind_dist
  )

panel_pbl <- panel_pbl %>%
  mutate(
    class = as.numeric(factor(as.character(class)))
  )


write_dta(panel_pbl, "data/panel_pbl.dta")

#############################################################