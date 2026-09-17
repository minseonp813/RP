## ---------------------------------------------------------------------------
## Cross-consistency threshold  e^x(S)  and the cross-partition index  Ihat.
##
## Mirrors the existing GARP/CCEI code (garp.R, warshall.R, ccei_garp.R):
##   - cross_garp() replaces garp()'s "is there a violation?" test with
##     "is there a violation whose supporting cycle uses BOTH sides?"
##   - ex_cross()   replaces ccei_garp()'s bisection wrapper, unchanged in shape.
##
## side: length-n character vector labelling each observation of the MERGED data
##       "I" (individual side, D_S) or "G" (group side, D_g).
##
## Verified against the existing code by the identity (Lemma 1 of the note)
##       ccei(D_Sg) == min( ccei(D_S), ccei(D_g), e^x(S) ),
## which held with residual 0 on the checked groups (see T4d/T5 in ex_cross_tests.R).
##
## Requires: warshall() from programs/warshall.R.
## ---------------------------------------------------------------------------

## cross-GARP at efficiency e: returns 1 if D_Sg has NO cross e-violation, else 0.
## SEMANTICS: like the existing garp.R/ccei_garp.R, a "violation" is a directed CLOSED WALK
## (nodes may repeat) with a strict step -- captured here by transitive closure + strong
## components. The cross condition adds: that component must meet BOTH sides. This is the
## reading under which Lemma 1 (ccei(D_Sg)=min{ccei_S,ccei_g,e^x}) holds, verified residual 0.
## It differs from a strict SIMPLE-cycle reading only at exact indifference ties (weak-but-not-
## strict edges, e = an exact expenditure ratio); on tie-free data the two coincide exactly.
cross_garp <- function(p, x, side, e) {
  n <- dim(p)[2]
  R0 <- matrix(0, n, n)
  P0 <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      budget_i <- e * sum(p[, i] * x[, i])
      R0[i, j] <- budget_i >= sum(p[, i] * x[, j])
      P0[i, j] <- budget_i >  sum(p[, i] * x[, j])
    }
  }
  R <- warshall(R0)
  result <- 1
  for (a in 1:n) {
    for (b in 1:n) {
      ## strict edge a->b together with a path b->a puts a,b in one SCC with a
      ## strict internal edge; if that SCC meets both sides, a cross violation exists.
      if (P0[a, b] == 1 && R[b, a] == 1) {
        comp <- union(which(R[a, ] == 1 & R[, a] == 1), c(a, b))
        if (any(side[comp] == "I") && any(side[comp] == "G")) {
          result <- 0
          break
        }
      }
    }
    if (result == 0) break
  }
  result
}

## e^x(S) = sup{ e in (0,1] : D_Sg has no cross e-violation } (bisection, tol 1e-6).
ex_cross <- function(p, x, side) {
  if (cross_garp(p, x, side, 1) == 1) return(1)     # no cross violation at all -> e^x = 1
  eL <- 0
  eH <- 1
  estar <- 0
  while (eH - eL > 1e-6) {
    e <- (eL + eH) / 2
    if (cross_garp(p, x, side, e) == 1) {
      estar <- e
      eL <- e
    } else {
      eH <- e
    }
  }
  ## Non-attained-sup guard: the RP relations change only at candidate ratios
  ## r_ij = (p_i . x_j)/(p_i . x_i). If the ONLY cross violation is exactly at the closed
  ## endpoint e=1 (an exact budget tie), then no r<1 induces it, so e^x = sup = 1 even
  ## though bisection converged just below 1. Detect: estar climbed above every ratio < 1.
  n <- ncol(p); hi <- 0
  for (i in 1:n) {
    bi <- sum(p[, i] * x[, i])
    for (j in 1:n) if (j != i) {
      r <- sum(p[, i] * x[, j]) / bi
      if (r < 1 - 1e-12 && r > hi) hi <- r
    }
  }
  if (estar > hi + 1e-9) return(1)
  estar
}

## Convenience: e^x from an individual-side data.frame subset and a group-side subset.
## Subsets are round-level rows with columns coord_x, coord_y, intercept_x, intercept_y
## (same shape the existing compute_ccei_from_subset() consumes). Returns NA if a side is empty.
compute_ex_from_subsets <- function(sub_ind, sub_grp) {
  keep <- function(s) s[
    !is.na(s$coord_x) & !is.na(s$coord_y) &
      !is.na(s$intercept_x) & !is.na(s$intercept_y) &
      s$intercept_x != 0 & s$intercept_y != 0, , drop = FALSE]
  si <- keep(sub_ind); sg <- keep(sub_grp)
  if (nrow(si) < 1 || nrow(sg) < 1) return(NA_real_)
  p <- rbind(c(1 / si$intercept_x, 1 / sg$intercept_x),
             c(1 / si$intercept_y, 1 / sg$intercept_y))
  x <- rbind(c(si$coord_x, sg$coord_x),
             c(si$coord_y, sg$coord_y))
  side <- c(rep("I", nrow(si)), rep("G", nrow(sg)))
  ex_cross(p, x, side)
}

## Total cross-partition cost c_Ng = 1 - e^x(N).
## Kept here so the full pipeline and the temporary fast updater use exactly
## the same conversion from the cross threshold to the cost.
cNg_from_ex <- function(ex_Ng) {
  ifelse(is.na(ex_Ng), NA_real_, 1 - ex_Ng)
}

## Cross-partition index for the "high" member, given the three cross thresholds.
## Ihat_hg = 1/2 + (chat_h - chat_l) / (2 chat_hl),  chat(S) = 1 - e^x(S).
## Returns NA when undefined (chat_hl == 0, i.e. no revealed member-group disagreement).
## GUARD: e^x is measured by bisection at tolerance 1e-6, so a chat_hl at that resolution is
## indistinguishable from 0 (no disagreement). Guarding at 1e-9 (below the bisection floor
## ~2^-20) let a should-be-NA index return a huge finite value; guard at the tolerance instead.
ihat_from_ex <- function(ex_h, ex_l, ex_hl) {
  chat_h <- 1 - ex_h; chat_l <- 1 - ex_l; chat_hl <- 1 - ex_hl
  if (is.na(chat_hl) || chat_hl <= 1e-6) return(NA_real_)
  0.5 + (chat_h - chat_l) / (2 * chat_hl)
}
