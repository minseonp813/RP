## =====================================================================================
##  programs/rp_cross_costs.R
##
##  Shared machinery for the revealed-preference distance index of Section 3 of the paper,
##  under the three inconsistency measures used there and in Appendix C:
##
##      CCEI     c_Sg      = 1 - e^x_Sg,   e^x = sup{ e : no cross e-violation }
##      MaxMPI   c^MP_Sg   = max money-pump index over cross violations   (appendix, def:app_mp)
##      HM       c^HM_Sg   = min # observations to delete to kill all cross violations (def:app_hm)
##
##  and the Shapley-normalised index built from any of them,
##
##      I_ig = [ (1/2) c_ig + (1/2) ( c_Ng - c_jg ) ] / c_Ng          (paper, def:index).
##
##  References to the paper are by LaTeX label (def:cost, def:index, def:app_mp, def:app_hm,
##  prop:app_general, ...) because the printed numbering changes between drafts.
##
##  PLATFORM.  .R-library holds macOS arm64 binaries built under R 4.6 (haven, ggplot2, igraph).
##  On any other platform or R version, delete or rename that folder and install those three
##  packages yourself; rstudioapi is optional.  Run every script with the package folder as the
##  working directory (Rscript from that folder, or Source in RStudio, which sets it).
##
##  This file defines functions only.  The three driver scripts
##      20_table1_figure2_ccei.R, 21_table1_figure2_maxmpi.R, 22_table1_figure2_hm.R
##  source it and each produces the Table 1 rows and the Figure 2 panels for one measure.
##
##  ---------------------------------------------------------------------------------
##  WHY THE COMPARISONS ARE DONE IN INTEGER ARITHMETIC
##  ---------------------------------------------------------------------------------
##  With p = 1/intercept, the revealed-preference comparison p_u . x_v <= p_u . x_u is
##
##      cx_v * iy_u + cy_v * ix_u   <=   cx_u * iy_u + cy_u * ix_u ,
##
##  i.e. a comparison of two integers, because the choice coordinates and the budget
##  intercepts are recorded in whole Korean won.  Exact expenditure ties DO occur in this
##  data set (a chosen bundle lying exactly on another round's budget line), and in double
##  precision a handful of them evaluate the wrong way.  Every relation below is therefore
##  computed from the integer form.  Only the money-pump VALUES are floating point.
##
##  Author: replication package.  Requires: igraph (bundled in .R-library).
## =====================================================================================

suppressPackageStartupMessages(library(igraph))


## -------------------------------------------------------------------------------------
## 1.  From a block of round-level rows to the revealed-preference primitives
## -------------------------------------------------------------------------------------

##' Build the integer expenditure matrix of a set of observations.
##'
##' @param d data.frame with columns coord_x, coord_y, intercept_x, intercept_y
##' @return list with
##'   E   n x n integer matrix, E[u, v] = numerator of p_u . x_v scaled by ix_u * iy_u
##'   den n-vector, den[u] = ix_u * iy_u, so that p_u . x_v = E[u, v] / den[u]
##'   m   n-vector of own expenditures p_u . x_u (numeric; equals 1 up to KRW rounding)
##' The point of E is that the comparison p_u.x_v <= p_u.x_u is exactly E[u,v] <= E[u,u].
rp_expenditure <- function(d) {
  cx <- as.numeric(d$coord_x);     cy <- as.numeric(d$coord_y)
  ix <- as.numeric(d$intercept_x); iy <- as.numeric(d$intercept_y)
  n  <- length(cx)
  ## E[u, v] = cx_v * iy_u + cy_v * ix_u  (outer products keep this vectorised)
  E   <- outer(iy, cx) + outer(ix, cy)
  den <- ix * iy
  list(E = E, den = den, m = diag(E) / den, n = n)
}

##' Weak and strict revealed-preference relations at efficiency level e = num / den.
##'
##' Row u, column v is TRUE when p_u . x_v <= e * (p_u . x_u), i.e. E[u,v]*den <= E[u,u]*num.
##' Self-comparisons are removed from the strict matrix, which is what Definition 1 needs.
rp_relations <- function(EX, e_num = 1, e_den = 1) {
  thr <- EX$E[cbind(1:EX$n, 1:EX$n)] * e_num          # e * (p_u . x_u), scaled
  lhs <- EX$E * e_den
  W <- lhs <= thr                                      # weak   R^0_e
  S <- lhs <  thr                                      # strict P^0_e
  diag(S) <- FALSE
  list(W = W, S = S)
}

##' Strongly connected components of a boolean adjacency matrix (diagonal ignored).
##' Returns a list of integer vectors of node indices.
rp_scc <- function(W) {
  A <- W; diag(A) <- FALSE
  g <- graph_from_adjacency_matrix(A, mode = "directed")
  memb <- components(g, mode = "strong")$membership
  split(seq_along(memb), memb)
}

##' Does the sub-dataset on `keep` admit a CROSS violation (paper, def:cross)?
##'
##' Reading Definition 1 literally, a violation is a closed walk with one strict step, so a
##' cross violation exists exactly when some strongly connected component of the retained
##' graph contains observations of both sides AND a strict comparison between two of its
##' own members.  That is what this function tests.
##'
##' @param side character vector, "I" (individual side) or "G" (group side), one per observation
rp_has_cross <- function(REL, side, keep = NULL) {
  idx <- if (is.null(keep)) seq_along(side) else sort(keep)
  if (length(idx) < 2L) return(FALSE)
  W <- REL$W[idx, idx, drop = FALSE]; S <- REL$S[idx, idx, drop = FALSE]
  for (comp in rp_scc(W)) {
    if (length(comp) < 2L) next
    sd <- side[idx[comp]]
    if (!(any(sd == "I") && any(sd == "G"))) next
    if (any(S[comp, comp, drop = FALSE])) return(TRUE)
  }
  FALSE
}

##' Support of a small cross violation inside `keep`, or NULL if there is none.
##' Used to generate cutting planes for the Houtman-Maks program.  We look for the shortest
##' cycle that closes a side-switching comparison, which is a cross violation by construction.
rp_cross_support <- function(REL, side, keep = NULL) {
  idx <- if (is.null(keep)) seq_along(side) else sort(keep)
  if (length(idx) < 2L) return(NULL)
  W <- REL$W[idx, idx, drop = FALSE]; S <- REL$S[idx, idx, drop = FALSE]
  best <- NULL
  for (comp in rp_scc(W)) {
    if (length(comp) < 2L) next
    sd <- side[idx[comp]]
    if (!(any(sd == "I") && any(sd == "G"))) next
    if (!any(S[comp, comp, drop = FALSE])) next
    sub <- W[comp, comp, drop = FALSE]; diag(sub) <- FALSE
    g   <- graph_from_adjacency_matrix(sub, mode = "directed")
    ## (a) shortest cycle through a side-switching comparison
    for (a in seq_along(comp)) for (b in seq_along(comp)) {
      if (a == b || !sub[a, b] || sd[a] == sd[b]) next
      pth <- suppressWarnings(shortest_paths(g, from = b, to = a, output = "vpath")$vpath[[1]])
      if (length(pth) == 0L) next
      cyc <- unique(c(a, as.integer(pth)))
      steps <- cbind(cyc, c(cyc[-1], cyc[1]))
      if (!any(S[comp, comp, drop = FALSE][steps])) next     # needs one strict step
      if (is.null(best) || length(cyc) < length(best)) best <- idx[comp[cyc]]
    }
    ## (b) exact-tie fallback: a strict cycle plus a detour to the other side
    if (is.null(best)) {
      for (a in seq_along(comp)) for (b in seq_along(comp)) {
        if (!S[comp, comp, drop = FALSE][a, b]) next
        pth <- suppressWarnings(shortest_paths(g, from = b, to = a, output = "vpath")$vpath[[1]])
        if (length(pth) == 0L) next
        supp <- unique(c(a, as.integer(pth)))
        if (any(sd[supp] == "I") && any(sd[supp] == "G")) {
          if (is.null(best) || length(supp) < length(best)) best <- idx[comp[supp]]
          next
        }
        miss <- if (all(sd[supp] != "I")) "I" else "G"
        for (w in which(sd == miss)) {
          p1 <- suppressWarnings(shortest_paths(g, from = a, to = w, output = "vpath")$vpath[[1]])
          p2 <- suppressWarnings(shortest_paths(g, from = w, to = a, output = "vpath")$vpath[[1]])
          if (length(p1) == 0L || length(p2) == 0L) next
          cand <- unique(c(supp, as.integer(p1), as.integer(p2)))
          if (is.null(best) || length(cand) < length(best)) best <- idx[comp[cand]]
        }
      }
    }
  }
  best
}


## -------------------------------------------------------------------------------------
## 2.  CCEI-based cross-consistency cost      c_Sg = 1 - e^x_Sg
## -------------------------------------------------------------------------------------

##' Exact cross-consistency cost.
##'
##' The revealed-preference relations change only at the finitely many expenditure ratios
##' r_uv = (p_u . x_v) / (p_u . x_u), so the supremum in Definition 3 is found EXACTLY by a
##' binary search over that sorted grid: no bisection tolerance is involved.
##' Passing side = NULL gives 1 - CCEI of the dataset itself (used for individual and group
##' rationality in Table 1).
rp_cost_ccei <- function(EX, side = NULL) {
  ## "is there a (cross) violation at e = num/den?", evaluated by exact integer comparison
  viol <- function(num, den, strict_as_weak = FALSE) {
    REL <- rp_relations(EX, num, den)
    if (strict_as_weak) { REL$S <- REL$W; diag(REL$S) <- FALSE }
    if (is.null(side)) {
      for (comp in rp_scc(REL$W))
        if (length(comp) > 1L && any(REL$S[comp, comp, drop = FALSE])) return(TRUE)
      return(FALSE)
    }
    rp_has_cross(REL, side)
  }
  if (!viol(1, 1)) return(0)
  ## Candidate levels: the distinct expenditure ratios r_uv = E[u,v] / E[u,u] in (0, 1), kept as
  ## exact (numerator, denominator) pairs so that no rounding enters the comparisons.
  dg  <- EX$E[cbind(1:EX$n, 1:EX$n)]
  num <- as.vector(EX$E); den <- rep(dg, times = EX$n)
  val <- num / den
  ok  <- val > 0 & val < 1
  num <- num[ok]; den <- den[ok]; val <- val[ok]
  ## Sorting and de-duplication use the double num/den, which is safe here and only here.
  ## Equal rationals give bitwise-equal doubles (IEEE division is correctly rounded), so the
  ## de-duplication is exact.  Distinct ratios differ by at least 1/(den_i * den_j), so with
  ## denominators bounded as below the gap exceeds the rounding error by orders of magnitude and
  ## the order cannot be wrong.  Every SUBSEQUENT comparison uses the exact (num, den) pair.
  ## Distinct ratios are at least 1/(den_i * den_j) >= 1/max(den)^2 apart, while the division
  ## num/den is correctly rounded to within about 1.1e-16.  In this data the intercepts are at
  ## most 3,000 KRW, so max(den) <= 9e6 and the gap is at least 1.2e-14 -- roughly seventy times
  ## the rounding error.  The guard fails loudly on data that would break the argument.
  stopifnot(1 / (max(den) * max(den)) > 8 * .Machine$double.eps)
  o   <- order(val); num <- num[o]; den <- den[o]; val <- val[o]
  keep <- !duplicated(val); num <- num[keep]; den <- den[keep]; val <- val[keep]
  ## "no violation at level k" is non-increasing in k, so binary search finds the switch point.
  lo <- 0L; hi <- length(val) + 1L
  while (hi - lo > 1L) {
    mid <- (lo + hi) %/% 2L
    if (!viol(num[mid], den[mid])) lo <- mid else hi <- mid
  }
  attained <- lo > 0L && viol(num[lo], den[lo], strict_as_weak = TRUE)
  estar <- if (attained) val[lo] else if (hi == length(val) + 1L) 1 else val[hi]
  1 - estar
}


## -------------------------------------------------------------------------------------
## 3.  Houtman-Maks cross cost      c^HM_Sg = min # deletions
## -------------------------------------------------------------------------------------

##' Minimum-cardinality hitting set of `cuts` over items 1..n, by branch and bound.
##' `cuts` is a list of integer vectors.  Exact; the instances here have optima below 20.
rp_min_hitting_set <- function(cuts, n) {
  if (length(cuts) == 0L) return(integer(0))
  cuts <- lapply(cuts, function(c) sort(unique(as.integer(c))))
  ## Drop any cut that contains another cut: hitting the smaller one hits it too.
  ord  <- order(vapply(cuts, length, integer(1)))
  cuts <- cuts[ord]; keep <- rep(TRUE, length(cuts))
  for (a in seq_along(cuts)) if (keep[a])
    for (b in seq_along(cuts)) if (b > a && keep[b] && all(cuts[[a]] %in% cuts[[b]])) keep[b] <- FALSE
  cuts <- cuts[keep]

  ## Greedy upper bound: repeatedly delete the observation lying on the most surviving cuts.
  rem <- cuts; greedy <- integer(0)
  while (length(rem) > 0L) {
    tab <- table(unlist(rem))
    k <- as.integer(names(tab)[which.max(tab)])
    greedy <- c(greedy, k)
    rem <- rem[!vapply(rem, function(c) k %in% c, logical(1))]
  }
  best <- greedy; best_size <- length(greedy)

  ## Lower bound: a family of pairwise DISJOINT cuts needs one deletion each, so the size of a
  ## greedy disjoint sub-family is a valid lower bound on what any completion still costs.
  lb_disjoint <- function(remaining) {
    if (length(remaining) == 0L) return(0L)
    ordr <- order(vapply(remaining, length, integer(1)))
    used <- integer(0); cnt <- 0L
    for (i in ordr) {
      c <- remaining[[i]]
      if (!any(c %in% used)) { cnt <- cnt + 1L; used <- c(used, c) }
    }
    cnt
  }

  rec <- function(chosen, remaining) {
    if (length(remaining) == 0L) {
      if (length(chosen) < best_size) { best <<- chosen; best_size <<- length(chosen) }
      return(invisible(NULL))
    }
    if (length(chosen) + lb_disjoint(remaining) >= best_size) return(invisible(NULL))
    cut <- remaining[[which.min(vapply(remaining, length, integer(1)))]]
    for (k in cut) rec(c(chosen, k), remaining[!vapply(remaining, function(c) k %in% c, logical(1))])
    invisible(NULL)
  }
  rec(integer(0), cuts)
  best
}

##' Exact cross Houtman-Maks cost by constraint generation.
##' Repeatedly: solve the hitting-set problem over the cuts found so far, look for a cross
##' violation in what survives, and add its support as a new cut.  Terminates because there
##' are finitely many supports.  side = NULL gives the plain Houtman-Maks COUNT of a dataset
##' (the number of observations to delete to restore GARP), not the fraction retained.
rp_cost_hm <- function(EX, side = NULL, max_iter = 500L) {
  REL <- rp_relations(EX, 1, 1)
  n   <- EX$n
  sd  <- if (is.null(side)) rep("I", n) else side
  find_supp <- function(keep) {
    if (is.null(side)) {                                   # plain HM: any violation
      idx <- sort(keep); if (length(idx) < 2L) return(NULL)
      W <- REL$W[idx, idx, drop = FALSE]; S <- REL$S[idx, idx, drop = FALSE]
      best <- NULL
      for (comp in rp_scc(W)) {
        if (length(comp) < 2L) next
        sub <- W[comp, comp, drop = FALSE]; diag(sub) <- FALSE
        g <- graph_from_adjacency_matrix(sub, mode = "directed")
        for (a in seq_along(comp)) for (b in seq_along(comp)) {
          if (!S[comp, comp, drop = FALSE][a, b]) next
          pth <- suppressWarnings(shortest_paths(g, from = b, to = a, output = "vpath")$vpath[[1]])
          if (length(pth) == 0L) next
          cand <- idx[comp[unique(c(a, as.integer(pth)))]]
          if (is.null(best) || length(cand) < length(best)) best <- cand
        }
      }
      best
    } else rp_cross_support(REL, side, keep)
  }
  ## Seed with every two-observation violation, which is cheap to list and already supplies many
  ## pairwise-disjoint cuts, so the branch-and-bound below starts with a tight lower bound.
  cuts <- list()
  two <- which(REL$W & t(REL$W) & (REL$S | t(REL$S)), arr.ind = TRUE)
  if (nrow(two) > 0L) {
    two <- two[two[, 1] < two[, 2], , drop = FALSE]
    if (!is.null(side) && nrow(two) > 0L)
      two <- two[sd[two[, 1]] != sd[two[, 2]], , drop = FALSE]      # cross only
    if (nrow(two) > 0L) cuts <- split(two, row(two))
  }
  supp <- find_supp(seq_len(n))
  if (is.null(supp)) return(0L)
  if (length(cuts) == 0L) cuts <- list(supp) else cuts[[length(cuts) + 1L]] <- supp
  for (it in seq_len(max_iter)) {
    R <- rp_min_hitting_set(cuts, n)
    s <- find_supp(setdiff(seq_len(n), R))
    if (is.null(s)) return(length(R))
    cuts[[length(cuts) + 1L]] <- s
  }
  stop("rp_cost_hm: constraint generation did not converge")
}


## -------------------------------------------------------------------------------------
## 4.  Money-pump cross cost      c^MP_Sg = max MPI over cross violations
## -------------------------------------------------------------------------------------

##' Money-pump index of a cycle given as a vector of observation indices (appendix, def:app_mp).
##'
##' MPI(V) = sum_a (p^a.x^a - p^a.x^{a+1}) / sum_a p^a.x^a.  The paper normalises income to one,
##' p^a.x^a = 1 for every observation, under which this is the UNWEIGHTED mean of the per-step
##' shares  sigma_a = 1 - p^a.x^{a+1} / p^a.x^a  (appendix, eqn:app_pumpshare).  The shares are
##' ratios of two expenditures at the same prices, so they do not depend on how prices are
##' scaled; the normalisation only fixes the weights, and this function applies it exactly.
##' (Taking p = 1/intercept instead gives p.x within 0.2 per cent of 1 because the recorded
##' choices are integer KRW; that would weight the shares by those expenditures and differ from
##' the paper's definition in the fourth decimal.  We do not do that.)
rp_mpi <- function(EX, cyc) {
  nxt <- c(cyc[-1], cyc[1])
  mean(1 - EX$E[cbind(cyc, nxt)] / EX$E[cbind(cyc, cyc)])
}

##' Unconstrained maximum ratio cycle inside `nodes` (Dinkelbach with Bellman-Ford).
##'
##' @param banned,forced  optional character vectors of "u|v" edge keys, see the body
##' @return list(lambda, cycle, tight): lambda is the largest MPI over all cycles of the (sub)graph
##'   and cycle attains it; `tight` is TRUE only when the loop PROVED that bound (see the body).
##'   The value bounds every cross cycle in the component from above, which is what
##'   rp_max_cross_cycle prunes on.
rp_max_ratio_cycle <- function(EX, REL, nodes, banned = NULL, forced = NULL,
                               eps = 1e-12, max_iter = 200L) {
  ## Maximum ratio cycle sum(w_e) / sum(b_u) by Dinkelbach iteration with Bellman-Ford, where
  ## b_u is the expenditure at u and w_{uv} the expenditure released by the step u -> v; the
  ## ratio of a cycle is exactly its money-pump index.
  ##
  ## `banned` and `forced` are character vectors of "u|v" keys in GLOBAL observation numbers,
  ## used by the branch and bound below: a banned edge is deleted, and a forced edge (a,b) is
  ## imposed by deleting every other edge out of a and every other edge into b.  That is a
  ## RELAXATION of "the cycle uses (a,b)", not an encoding of it: a cycle avoiding both a and b
  ## survives untouched.  It is sound for the search below because the relaxed maximum still
  ## bounds the intended subproblem from above, and any cycle it reports is a genuine cycle of
  ## the original graph.  No simple cycle through (a,b) is ever lost, since such a cycle uses no
  ## other out-edge of a and no other in-edge of b.  NULL is returned if forcing is infeasible.
  ##
  ## The Bellman-Ford relaxation is one matrix operation per round: R loops over the ~10^3 edges
  ## of a 50-observation dataset are the difference between minutes and hours.
  ns <- sort(nodes); n <- length(ns)
  if (n < 2L) return(list(lambda = 0, cycle = NULL, tight = TRUE))
  Wl <- REL$W[ns, ns, drop = FALSE]; diag(Wl) <- FALSE
  if (length(banned) || length(forced)) kk <- outer(ns, ns, function(a, b) paste0(a, "|", b))
  if (length(banned)) Wl[kk %in% banned] <- FALSE
  if (length(forced)) {
    for (f in forced) {
      ij <- which(kk == f, arr.ind = TRUE)
      if (!nrow(ij)) return(list(lambda = 0, cycle = NULL, tight = TRUE))
      a <- ij[1, 1]; b <- ij[1, 2]
      if (!Wl[a, b]) return(list(lambda = 0, cycle = NULL, tight = TRUE))  # conflicting demands
      Wl[a, ] <- FALSE; Wl[, b] <- FALSE; Wl[a, b] <- TRUE
    }
  }
  if (!any(Wl)) return(list(lambda = 0, cycle = NULL, tight = TRUE))
  ## Under p.x = 1 every observation's expenditure is 1, so the "time" b_u of a step is 1 and
  ## the "cost" w[u, v] is the share sigma released by u -> v (see rp_mpi).
  mm <- rep(1, n)                                              # b_u = 1
  ee <- EX$E[ns, ns, drop = FALSE] / EX$E[cbind(ns, ns)]       # ee[u, v] = p^u . x^v / (p^u . x^u)
  Wm <- 1 - ee                                                 # w[u, v] = sigma_uv
  Bm <- matrix(1, n, n)
  ## `tight` records whether the loop ended by PROVING that no cycle beats lam, which happens
  ## when Bellman-Ford finds no negative cycle.  The other exits stop while a better cycle is
  ## still known to EXIST, and then lam is the ratio of some cycle of the subgraph -- a LOWER
  ## bound on its maximum, not a proven upper bound.  Pruning on such a value is not a
  ## small approximation: it can discard an arbitrarily better cross cycle.  The caller must
  ## therefore consult `tight` before using lam as a bound, or before treating a cross maximizer
  ## as optimal.
  lam <- 0; cyc <- NULL; tight <- FALSE
  for (it in seq_len(max_iter)) {
    Cm <- matrix(Inf, n, n)
    Cm[Wl] <- (lam * Bm - Wm)[Wl]                              # negative cycle  <=>  ratio > lam
    dist <- rep(0, n); pred <- rep(NA_integer_, n); upd <- 0L
    for (r in seq_len(n)) {
      M    <- Cm + dist                                        # M[u, v] = dist[u] + cost[u, v]
      idx  <- max.col(t(-M), ties.method = "first")            # argmin over u, for each v
      mins <- M[cbind(idx, seq_len(n))]
      imp  <- which(mins < dist - 1e-15)
      if (!length(imp)) { upd <- 0L; break }
      dist[imp] <- mins[imp]; pred[imp] <- idx[imp]; upd <- imp[1L]
    }
    if (upd == 0L) { tight <- TRUE; break }                    # no negative cycle: lam is optimal
    x <- upd
    for (r in seq_len(n)) { if (is.na(pred[x])) break; x <- pred[x] }
    if (is.na(pred[x])) break
    cur <- x; seen <- integer(0)
    repeat {
      seen <- c(seen, cur); cur <- pred[cur]
      if (is.na(cur) || cur == x || length(seen) > n) break
    }
    if (is.na(cur) || cur != x) break
    cyc_local <- rev(seen)
    new <- rp_mpi(EX, ns[cyc_local])
    cyc <- ns[cyc_local]
    ## Exact arithmetic guarantees new > lam here.  When rounding makes new <= lam the
    ## iteration has stalled on cycles whose ratios tie to machine precision.  We do NOT stop
    ## unproven, and we do NOT stop on a tolerance either (a far better cycle may still be
    ## ahead).  Instead we nudge lam up by `eps` and let Bellman-Ford decide: if no cycle beats
    ## lam + eps the loop exits through the proven branch above with a bound valid to within
    ## eps, which is the slack the callers use when pruning; if some cycle does beat lam + eps
    ## the next extraction makes genuine progress.  Reported VALUES are always rp_mpi() of an
    ## actual cycle, so only pruning decisions, not results, carry the eps.
    if (new <= lam) { lam <- lam + eps; next }
    lam <- new
  }
  list(lambda = lam, cycle = cyc, tight = tight)
}

##' Exact maximum MPI over CROSS simple cycles inside `nodes`, by branch and bound over
##' maximum-ratio-cycle relaxations (Lawler's partition).  See the comment in the body for the
##' correctness argument.  Returns the value, with attributes "exhausted" (the search closed
##' rather than hitting `budget`) and "cycle" (a maximizing cross cycle).
rp_max_cross_cycle <- function(EX, REL, nodes, side, lb = 0, budget = 200000L) {
  ## Largest money-pump index over the CROSS cycles of `nodes`, exactly.
  ##
  ## The unconstrained maximum-ratio cycle is polynomial (rp_max_ratio_cycle); the maximum over
  ## cycles that must meet both sides is not, so we branch.  In a subproblem let C be the
  ## unconstrained maximum-ratio cycle.  Its ratio bounds every cycle of the subproblem from
  ## above, cross cycles included, so a subproblem whose bound is below the incumbent is dropped.
  ## If C is cross it attains that bound and the subproblem is finished.  Otherwise every cross
  ## cycle of the subproblem differs from C, hence omits one of its edges e_1, ..., e_L, because
  ## a simple cycle containing all edges of another simple cycle equals it (in a simple cycle
  ## every vertex has out-degree one, so containing all of C's edges forces the same successor at
  ## every vertex of C, hence the same cycle).  We split on the FIRST omitted edge, in Lawler's
  ## fashion: child i bans e_i and forces e_1, ..., e_{i-1}.  Every cross cycle of the subproblem
  ## then survives in exactly the child indexed by its first omitted edge, so the split is
  ## exhaustive; the forcing is what keeps the tree small, since plain edge deletion re-solves the
  ## same subgraph many times over.  (Children can still overlap, in cycles that avoid the forced
  ## edges' endpoints altogether; that costs work, not correctness.)
  ##
  ## The search is depth-first so that the incumbent rises early.  `budget` caps the number of
  ## subproblems and the result carries attr "exhausted", so a truncated search is never silent.
  ns <- sort(nodes); n <- length(ns)
  ## The "cycle" attribute is NULL whenever the value is inherited from `lb` rather than found
  ## here, so any consumer must handle NULL.
  if (n < 2L) return(structure(lb, exhausted = TRUE, cycle = NULL))

  ## Warm start: the best cross two-cycle.  A good incumbent is what makes the pruning bite.
  best <- lb
  Wl <- REL$W[ns, ns, drop = FALSE]; diag(Wl) <- FALSE
  mm <- rep(1, n)                                              # p.x = 1, as in rp_mpi
  ee <- EX$E[ns, ns, drop = FALSE] / EX$E[cbind(ns, ns)]
  Wm <- 1 - ee
  sd <- side[ns]
  bestcyc <- NULL
  two <- which(Wl & t(Wl) & outer(sd, sd, "!="), arr.ind = TRUE)
  two <- two[two[, 1] < two[, 2], , drop = FALSE]
  if (nrow(two)) {
    r2 <- (Wm[two] + Wm[two[, c(2, 1), drop = FALSE]]) / (mm[two[, 1]] + mm[two[, 2]])
    if (max(r2) > best) {
      best <- max(r2)
      bestcyc <- ns[two[which.max(r2), ]]
    }
  }

  stack <- list(list(ban = character(0), force = character(0))); used <- 0L
  while (length(stack) && used < budget) {
    node  <- stack[[length(stack)]]; stack[[length(stack)]] <- NULL; used <- used + 1L
    mr <- rp_max_ratio_cycle(EX, REL, ns, banned = node$ban, forced = node$force)
    ## An empty subproblem is a legitimate reason to drop it, but only when that emptiness was
    ## established rather than assumed.  The extraction failures inside rp_max_ratio_cycle are
    ## unreachable -- a node improved in round r has a predecessor improved in round r-1, so the
    ## n-step predecessor walk lands inside a cycle by pigeonhole -- and this guard says so.
    if (is.null(mr$cycle)) {
      if (!isTRUE(mr$tight)) stop("rp_max_cross_cycle: empty subproblem on an unproven bound")
      next
    }
    ## Prune only on a PROVEN bound.  If Dinkelbach stopped on its tolerance, lam bounds the
    ## subproblem only heuristically, so we keep the subproblem rather than risk dropping a
    ## better cross cycle.  This costs a handful of extra solves and never changes the answer.
    if (mr$tight && mr$lambda <= best + 1e-12) next
    cs <- side[mr$cycle]
    if (any(cs == "I") && any(cs == "G")) {
      v <- rp_mpi(EX, mr$cycle)                                  # a genuine cross cycle either way
      if (v > best) { best <- v; bestcyc <- mr$cycle }
      ## Closing the subproblem here is licensed only when lam is a proven maximum over it.
      if (mr$tight) next
    }
    L  <- length(mr$cycle)
    ek <- vapply(seq_len(L), function(i)
                 paste0(mr$cycle[i], "|", mr$cycle[if (i == L) 1L else i + 1L]), character(1))
    ## push in reverse so that child 1 (ban e_1, force nothing new) is explored first
    for (i in rev(seq_len(L)))
      stack[[length(stack) + 1L]] <- list(ban   = c(node$ban, ek[i]),
                                          force = c(node$force, ek[seq_len(i - 1L)]))
  }
  structure(best, exhausted = length(stack) == 0L, cycle = bestcyc)
}

##' Cross money-pump cost (appendix, def:app_mp), exact.
##'
##' Inside each strongly connected component that meets both sides,
##' take the unconstrained maximum-ratio cycle.  If that cycle is itself cross AND the
##' Dinkelbach loop proved it optimal, it is the largest money pump in the component and the
##' value is certified.  Otherwise fall back to the branch and bound above, warm-started at the
##' best cross cycle found so far.
##' Returns list(value, certified, exhausted): `certified` records that no branching was needed,
##' `exhausted` that the branch and bound closed rather than hitting its budget.
rp_cost_mpi <- function(EX, side) {
  REL <- rp_restrict_relations(EX)
  best <- 0; certified <- TRUE; exhausted <- TRUE
  for (comp in rp_scc(REL$W)) {
    if (length(comp) < 2L) next
    sd <- side[comp]
    if (!(any(sd == "I") && any(sd == "G"))) next               # no cross violation lives here
    mr <- rp_max_ratio_cycle(EX, REL, comp)
    ## Skipping a component needs the same licence as pruning does: `lambda <= 0` rules out a
    ## cross violation here only if lambda was proved to bound the component.
    if (is.null(mr$cycle)) {
      if (!isTRUE(mr$tight)) stop("rp_cost_mpi: empty component on an unproven bound")
      next
    }
    if (mr$lambda <= 0 && isTRUE(mr$tight)) next
    if (mr$lambda <= 0) { certified <- FALSE
      z <- rp_max_cross_cycle(EX, REL, comp, side, lb = best)
      best <- max(best, as.numeric(z)); exhausted <- exhausted && isTRUE(attr(z, "exhausted"))
      next }
    cs <- side[mr$cycle]
    if (isTRUE(mr$tight) && any(cs == "I") && any(cs == "G")) {
      best <- max(best, rp_mpi(EX, mr$cycle))                    # certificate: optimum is cross
    } else {
      certified <- FALSE
      z <- rp_max_cross_cycle(EX, REL, comp, side, lb = best)
      best <- max(best, as.numeric(z))
      exhausted <- exhausted && isTRUE(attr(z, "exhausted"))
    }
  }
  list(value = best, certified = certified, exhausted = exhausted)
}

##' Unconstrained maximum MPI of a dataset (individual or group rationality, Table 1).
rp_maxmpi <- function(EX) {
  REL <- rp_restrict_relations(EX)
  mr <- rp_max_ratio_cycle(EX, REL, seq_len(EX$n))
  ## The individual and group MaxMPI scores are used for the ranking and the tie-break, so an
  ## unproven maximum is not acceptable here either (it never happens on this data).
  stopifnot(isTRUE(mr$tight))
  if (is.null(mr$cycle) || mr$lambda <= 0) 0 else rp_mpi(EX, mr$cycle)
}

##' Relations at e = 1, computed once and reused.
rp_restrict_relations <- function(EX) rp_relations(EX, 1, 1)


## -------------------------------------------------------------------------------------
## 5.  The index, the rationality ranking, and the two Table 1 / Figure 2 helpers
## -------------------------------------------------------------------------------------

##' Shapley-normalised revealed-preference distance of the FIRST member (paper, def:index).
##' Returns NA when the total cost is zero, in which case there is no cost to attribute.
rp_index <- function(c_i, c_j, c_N, zero_tol = 1e-9) {
  ifelse(is.na(c_N) | c_N <= zero_tol, NA_real_, 0.5 + (c_i - c_j) / (2 * c_N))
}

##' Which member is the more rational one?
##'
##' Convention of 01_calculate_ccei.R, applied to whichever score is passed in: the mover is
##' the more rational member unless the non-mover is STRICTLY more rational; ties on the
##' individual score are broken by the score of that member's individual-plus-group dataset;
##' any remaining tie is resolved in favour of the mover.  `higher_is_better` is TRUE for the
##' CCEI and FALSE for MaxMPI and Houtman-Maks, where a smaller score means more rational.
##' This flag is used ONLY here: the costs, the index and Table 1 do not depend on it; it decides
##' the Lower/Higher split of Figure 2 and the `high_mover` column.
##'
##' Rationale for the secondary key: when two members' own choices are equally consistent, the
##' one whose choices sit more consistently alongside the group's choices (the score of D^{ig})
##' is ranked higher, since the ranking is used to explain the group's choices.  The residual
##' tie goes to the mover by convention.  Ties are frequent -- 199 of 1,304 pair-waves for the
##' CCEI and MaxMPI (all at the value signalling GARP consistency), 343 for Houtman-Maks, whose
##' keys are integers -- so each driver script prints how much the convention matters.
##' Scores are never NA in this data; if one were, the mover would be ranked higher silently.
rp_higher <- function(s1, s2, s1g, s2g, higher_is_better = TRUE) {
  strictly_better <- function(a, b) if (higher_is_better) b > a else b < a
  out <- rep(1L, length(s1))
  swap <- !is.na(s1) & !is.na(s2) & strictly_better(s1, s2)
  tie  <- !is.na(s1) & !is.na(s2) & (s1 == s2) &
          !is.na(s1g) & !is.na(s2g) & strictly_better(s1g, s2g)
  out[swap | tie] <- 0L
  out
}

##' Stata's `summarize, detail` percentile (Hyndman-Fan type 2), so that the numbers printed
##' here line up with the ones the paper reports.
rp_pctile <- function(x, p) {
  x <- sort(x[!is.na(x)]); n <- length(x)
  if (n == 0L) return(NA_real_)
  i <- n * p
  if (abs(i - round(i)) < 1e-9) { i <- round(i); if (i < n) (x[i] + x[i + 1]) / 2 else x[n] }
  else x[ceiling(i)]
}

##' One Table 1 row: mean, SD, p10, p50, p90, N.
rp_summary_row <- function(label, x) {
  x <- x[!is.na(x)]
  data.frame(measure = label, mean = mean(x), sd = stats::sd(x),
             p10 = rp_pctile(x, .1), p50 = rp_pctile(x, .5), p90 = rp_pctile(x, .9),
             N = length(x), stringsAsFactors = FALSE)
}


## -------------------------------------------------------------------------------------
## 6.  Data assembly shared by the three driver scripts
## -------------------------------------------------------------------------------------

##' The nine pairs excluded in 01_calculate_ccei.R (budget-violating groups and groups that the
##' earlier pipeline dropped).  Kept here so that the three driver scripts use one definition.
## Groups excluded by the paper.  Worth noting for anyone changing this list: three of them
## (21204152120413, 21204162120405, 21204212120411) contain collective choices of exactly
## (0, 0) -- twenty rows in the endline wave -- at which the expenditure p . x is zero, so the
## per-step pump share and hence the money-pump index are undefined.  Dropping these groups is
## what keeps every expenditure in the analysed sample strictly positive.  The baseline wave has
## no zero bundle, and no observation in either wave has a zero intercept.
RP_DROP_GROUPS <- c("11106161110601", "21204152120413", "21204162120405", "21204212120411",
                    "16106101610601", "24101122410102", "24102242410208", "24102252410217",
                    "26104032610401")

##' Read one wave of round-level choices and normalise the id columns to character.
##'
##' `mover` is the raw-data flag for the member who moved seats to join the partner for the
##' collective rounds 19-36 (the other member is the stayer).  It is read at round 1 and may be
##' a different person in the two waves, so `id_mover` and role == "mover" below are labels
##' within a wave, not a fixed person.  Nothing in the index depends on which member is called
##' the mover except the residual tie-break in rp_higher().
rp_load_wave <- function(path) {
  d <- haven::read_dta(path)
  for (v in c("id", "partner_id", "group_id")) d[[v]] <- trimws(as.character(d[[v]]))
  d$round_number <- as.integer(d$round_number)
  d$mover <- as.integer(d$mover)
  ## The revealed-preference comparisons divide by p.x, so intercepts must be positive.  The
  ## sample used below also has no zero bundle (the groups that contain one are in
  ## RP_DROP_GROUPS); rp_compute_measure() checks that as well.
  stopifnot(all(as.numeric(d$intercept_x) > 0 & as.numeric(d$intercept_y) > 0))
  d
}

##' The balanced panel of pairs: those present in both waves, less RP_DROP_GROUPS.
##' Rounds 1-18 are the individual choices, rounds 19-36 the collective ones; the collective rows
##' are identical for the two members, so we always read them off the mover.
rp_pair_table <- function(base, end) {
  pt <- function(d) {
    r <- d[d$round_number == 1 & d$mover == 1, c("group_id", "id", "partner_id")]
    names(r) <- c("group_id", "id_mover", "id_nonmover")
    r[!duplicated(r$group_id), ]
  }
  p <- merge(pt(base), pt(end), by = "group_id", suffixes = c("_base", "_end"))
  p <- p[!p$group_id %in% RP_DROP_GROUPS, ]
  p[order(p$group_id), ]
}

##' Average demand share of the more expensive security (Section 3.3): the risk-aversion measure.
rp_risk_aversion <- function(d) {
  xe  <- ifelse(d$intercept_x < d$intercept_y, d$coord_x, d$coord_y)
  tot <- d$coord_x + d$coord_y
  mean(ifelse(tot == 0, NA_real_, xe / tot), na.rm = TRUE)
}

##' Compute, for every pair-wave, the three cross costs of ONE measure plus the individual and
##' group rationality scores of that measure and the risk-aversion measures.
##'
##' @param measure "ccei", "maxmpi", or "hm"
##' @param progress print a line every `progress` pair-waves (0 to silence)
##' @return data.frame, one row per pair-wave, with
##'   c_mover, c_nonmover, c_N        cross costs of D^{ig}, D^{jg}, D^{Ng}
##'   score_1, score_2, score_g       rationality of member 1, member 2 and the group
##'   score_1g, score_2g              rationality of each member's individual-plus-group dataset
##'                                   (used only to break ties in the "more rational" ranking)
##'   ra_1, ra_2, ra_g                risk aversion
rp_compute_measure <- function(measure, base, end, pairs, progress = 100L) {
  stopifnot(measure %in% c("ccei", "maxmpi", "hm"))
  ## Each cost function returns the value plus two flags that only the money pump uses:
  ##   cert  the unconstrained maximum-ratio cycle was already cross, so no branching was needed
  ##   exh   the branch and bound closed rather than hitting its budget
  ## They are carried into the output so that a truncated search can never pass unnoticed.
  cost  <- switch(measure,
                  ccei   = function(EX, sd) list(v = rp_cost_ccei(EX, sd), cert = NA, exh = NA),
                  maxmpi = function(EX, sd) { r <- rp_cost_mpi(EX, sd)
                                              list(v = r$value, cert = r$certified, exh = r$exhausted) },
                  hm     = function(EX, sd) list(v = rp_cost_hm(EX, sd), cert = NA, exh = NA))
  score <- switch(measure,                                   # larger = more rational only for CCEI
                  ccei   = function(EX) 1 - rp_cost_ccei(EX, NULL),
                  maxmpi = function(EX) rp_maxmpi(EX),
                  hm     = function(EX) rp_cost_hm(EX, NULL))
  out <- NULL; t0 <- Sys.time()
  for (w in c("base", "end")) {
    raw <- if (w == "base") base else end
    idc <- if (w == "base") c("id_mover_base", "id_nonmover_base") else c("id_mover_end", "id_nonmover_end")
    by_id <- split(raw, raw$id)
    for (k in seq_len(nrow(pairs))) {
      id1 <- pairs[[idc[1]]][k]; id2 <- pairs[[idc[2]]][k]
      d1 <- by_id[[id1]]; d2 <- by_id[[id2]]
      d1 <- d1[order(d1$round_number), ]; d2 <- d2[order(d2$round_number), ]
      i1 <- d1[d1$round_number <= 18, ]; i2 <- d2[d2$round_number <= 18, ]
      gg <- d1[d1$round_number >= 19, ]
      mk <- function(a, b) list(EX = rp_expenditure(rbind(a, b)),
                                sd = c(rep("I", nrow(a)), rep("G", nrow(b))))
      stopifnot(all(rp_expenditure(rbind(i1, i2, gg))$m > 0))      # no zero bundle in the sample
      M <- mk(i1, gg); Nm <- mk(i2, gg); B <- mk(rbind(i1, i2), gg)
      cm <- cost(M$EX, M$sd); cn <- cost(Nm$EX, Nm$sd); cb <- cost(B$EX, B$sd)
      out <- rbind(out, data.frame(
        group_id = pairs$group_id[k], wave = w, id_mover = id1, id_nonmover = id2,
        c_mover = cm$v, c_nonmover = cn$v, c_N = cb$v,
        certified = all(c(cm$cert, cn$cert, cb$cert)),
        exhausted = all(c(cm$exh, cn$exh, cb$exh)),
        score_1 = score(rp_expenditure(i1)), score_2 = score(rp_expenditure(i2)),
        score_g = score(rp_expenditure(gg)),
        score_1g = score(M$EX), score_2g = score(Nm$EX),
        ra_1 = rp_risk_aversion(i1), ra_2 = rp_risk_aversion(i2), ra_g = rp_risk_aversion(gg),
        stringsAsFactors = FALSE))
      if (progress > 0 && k %% progress == 0)
        message(sprintf("  %s %s: %d/%d  (%.0fs)", measure, w, k, nrow(pairs),
                        as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    }
  }
  out
}

##' Add the index and the more-rational-member flag to the output of rp_compute_measure().
rp_add_index <- function(df, higher_is_better) {
  df$I_mover  <- rp_index(df$c_mover, df$c_nonmover, df$c_N)
  df$high_mover <- rp_higher(df$score_1, df$score_2, df$score_1g, df$score_2g, higher_is_better)
  df
}

##' Reshape to one row per student-wave, which is the unit of Table 1 and Figure 2.
rp_student_long <- function(df) {
  mk <- function(role) {
    n <- if (role == "mover") 1L else 2L
    data.frame(group_id = df$group_id, wave = df$wave, role = role,
               id = if (role == "mover") df$id_mover else df$id_nonmover,
               score_i = df[[paste0("score_", n)]],
               ra_i = df[[paste0("ra_", n)]],
               I = if (role == "mover") df$I_mover else 1 - df$I_mover,
               higher = if (role == "mover") df$high_mover else 1L - df$high_mover,
               stringsAsFactors = FALSE)
  }
  rbind(mk("mover"), mk("nonmover"))
}

##' Table 1 for one measure: individual and group risk aversion, individual and group rationality,
##' the cross cost of the pair, and the revealed-preference distance, by wave.
rp_table1 <- function(df, stu, label) {
  rows <- NULL
  for (w in c("base", "end")) {
    p <- df[df$wave == w, ]; s <- stu[stu$wave == w, ]
    block <- rbind(
      rp_summary_row("Individual risk aversion", s$ra_i),
      rp_summary_row("Group risk aversion", p$ra_g),
      rp_summary_row(paste("Individual", label), s$score_i),
      rp_summary_row(paste("Group", label), p$score_g),
      rp_summary_row(paste0("Cross cost c_Ng (", label, ")"), p$c_N),
      rp_summary_row(paste0("Preference distance I_ig (", label, ")"), s$I))
    rows <- rbind(rows, cbind(wave = w, block, stringsAsFactors = FALSE))
  }
  rows[, c("wave", "measure", "mean", "sd", "p10", "p50", "p90", "N")]
}

##' Figure 2 for one measure: mean distance of the less- and more-rational member with 95 per cent
##' confidence intervals, and the two empirical CDFs.  Waves are pooled, as in the paper.
rp_figure2 <- function(stu, label, file_bar, file_cdf, width = 5, height = 4) {
  suppressPackageStartupMessages(library(ggplot2))
  d <- stu[!is.na(stu$I), ]
  d$grp <- factor(ifelse(d$higher == 1L, "Higher", "Lower"), levels = c("Lower", "Higher"))
  agg <- do.call(rbind, lapply(levels(d$grp), function(g) {
    x <- d$I[d$grp == g]
    data.frame(grp = g, m = mean(x), se = stats::sd(x) / sqrt(length(x)), stringsAsFactors = FALSE)
  }))
  agg$grp <- factor(agg$grp, levels = c("Lower", "Higher"))
  gap <- agg$m[agg$grp == "Lower"] - agg$m[agg$grp == "Higher"]
  x_lo <- d$I[d$grp == "Lower"]; x_hi <- d$I[d$grp == "Higher"]
  ## The two groups are mirror images (I_j = 1 - I_i), so this two-sample test treats the two
  ## halves of each pair as independent; the difference of means it tests equals the within-pair
  ## mean difference quoted in the paper's Figure 2 note.  Stars: ** p<.01, * p<.05, + p<.10.
  tt <- stats::t.test(x_lo, x_hi)
  stars <- if (tt$p.value < .01) "**" else if (tt$p.value < .05) "*" else if (tt$p.value < .10) "+" else ""
  p1 <- ggplot(agg, aes(grp, m, fill = grp)) +
    geom_col(width = .6, colour = "black") +
    geom_errorbar(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se), width = .15) +
    annotate("text", x = 1.5, y = .80, label = sprintf("Diff. = %.3f%s", gap, stars), size = 3.4) +
    scale_fill_manual(values = c(Lower = "#d9a0a0", Higher = "#8fb3d9"), guide = "none") +
    scale_x_discrete(labels = c(Lower = "Lower\nrationality", Higher = "Higher\nrationality")) +
    coord_cartesian(ylim = c(0, .85)) +
    labs(x = NULL, y = expression("Mean revealed preference distance " * hat(I)[ig]),
         title = label) +
    theme_minimal(base_size = 11)
  p2 <- ggplot(d, aes(I, colour = grp, linetype = grp)) +
    stat_ecdf(linewidth = .6) +
    scale_colour_manual(values = c(Lower = "red", Higher = "blue"), name = NULL) +
    scale_linetype_manual(values = c(Lower = "dashed", Higher = "solid"), name = NULL) +
    labs(x = expression(hat(I)[ig]), y = "Cumulative probability", title = label) +
    theme_minimal(base_size = 11) + theme(legend.position = c(.02, .98),
                                          legend.justification = c(0, 1))
  ggsave(file_bar, p1, width = width, height = height, dpi = 200)
  ggsave(file_cdf, p2, width = width, height = height, dpi = 200)
  ks <- suppressWarnings(stats::ks.test(x_hi, x_lo))
  data.frame(measure = label, n = nrow(d), mean_lower = mean(x_lo), mean_higher = mean(x_hi),
             diff = gap, t = unname(tt$statistic), p = tt$p.value,
             ks_D = unname(ks$statistic), ks_p = ks$p.value, stringsAsFactors = FALSE)
}
