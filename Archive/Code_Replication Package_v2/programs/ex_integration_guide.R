## ===========================================================================
## HOW TO ADD THE CROSS-PARTITION INDEX (Ihat) TO 01_calculate_ccei.R
##
## 1. At the top, alongside the other source() calls (~line 254), add:
##        source("programs/ex_cross.R")
##
## 2. Inside compute_ccei_wave_measures(), immediately AFTER the block that
##    computes I_hg / I_lg (currently lines ~642-651, ending with
##    `panel[[I_lg_col]] <- row_mean_na(cbind(Ilg1, Ilg2))`), paste the block
##    below. It reuses the subsets the function already built:
##        data_group  : group-side rows (mover's rounds 19-36)   [line ~627]
##        data_high   : high member's rows (raw_high == 1)        [line ~560]
##        data_low    : low  member's rows (raw_high == 0)        [line ~587]
##        data_indiv  : both members' individual rows (<=18)      [line ~626]
##    All carry group_id and round_number.
##
## 3. The two new columns Ihat_hg_<suffix>, Ihat_lg_<suffix> are added to panel,
##    parallel to I_hg / I_lg. (Optionally mirror this in compute_fgarp_wave_measures.)
## ===========================================================================

  ## ---- cross-partition index Ihat (mirrors I_hg / I_lg above) ----
  Ihat_hg_col <- paste0("Ihat_hg_", suffix)
  Ihat_lg_col <- paste0("Ihat_lg_", suffix)
  panel[[Ihat_hg_col]] <- NA_real_
  panel[[Ihat_lg_col]] <- NA_real_

  for (i in seq_len(nrow(panel))) {
    g <- panel$group_id[i]

    grp   <- data_group[data_group$group_id == g, ]                              # group side
    hi    <- data_high [data_high$group_id  == g & data_high$round_number <= 18, ] # high indiv
    lo    <- data_low  [data_low$group_id   == g & data_low$round_number  <= 18, ] # low  indiv
    bothi <- data_indiv[data_indiv$group_id == g, ]                              # both indiv

    ex_h  <- compute_ex_from_subsets(hi,    grp)   # e^x({high})
    ex_l  <- compute_ex_from_subsets(lo,    grp)   # e^x({low})
    ex_hl <- compute_ex_from_subsets(bothi, grp)   # e^x({high,low})

    panel[[Ihat_hg_col]][i] <- ihat_from_ex(ex_h, ex_l, ex_hl)
    panel[[Ihat_lg_col]][i] <- ihat_from_ex(ex_l, ex_h, ex_hl)   # = 1 - Ihat_hg when defined
  }
  ## ---- end cross-partition block ----
