## ---------------------------------------------------------------------------
## Cross-consistency threshold for FGARP and the corresponding Ihat index.
##
## This file is the FGARP analogue of programs/ex_cross.R. It preserves the
## weak and strict revealed-preference relations in programs/fgarp.R, while
## counting only violations supported by a strongly connected component that
## contains observations from BOTH partitions:
##   "I" = individual-side choices
##   "G" = group-side choices
##
## Interpretation: as in ex_cross.R, a cycle is read as a directed closed walk
## (nodes may repeat). This SCC convention is the proposed implementation that
## should be confirmed by the authors before substantive use.
##
## Requires: warshall() from programs/warshall.R.
## ---------------------------------------------------------------------------

## FGARP cross-consistency test at efficiency e.
## Returns 1 if there is no mixed-side FGARP violation and 0 otherwise.
cross_fgarp <- function(p, x, side, pi, e) {
  if (!is.matrix(p) || !is.matrix(x)) {
    stop("p and x must be matrices.")
  }
  if (!all(dim(p) == dim(x))) {
    stop("p and x must have identical dimensions.")
  }

  n <- ncol(p)
  if (length(side) != n) {
    stop("side must contain one label for each observation.")
  }
  if (!all(side %in% c("I", "G"))) {
    stop("side labels must be either 'I' or 'G'.")
  }
  if (!is.matrix(pi) || nrow(pi) != nrow(p) || ncol(pi) != 1) {
    stop("pi must be a one-column matrix with one row per good.")
  }

  ## Exact copy of the counterfactual-bundle construction in fgarp.R.
  y <- rbind(x[2, ], x[1, ])
  if (pi[1, 1] != pi[2, 1]) {
    if (pi[1, 1] < pi[2, 1]) {
      y[, x[1, ] <= x[2, ]] <- x[, x[1, ] <= x[2, ], drop = FALSE]
    } else {
      y[, x[2, ] <= x[1, ]] <- x[, x[2, ] <= x[1, ], drop = FALSE]
    }
  }

  R0 <- matrix(0, n, n)
  P0 <- matrix(0, n, n)

  for (i in seq_len(n)) {
    budget_i <- e * sum(p[, i] * x[, i])

    for (j in seq_len(n)) {
      cost_xj <- sum(p[, i] * x[, j])
      cost_yj <- sum(p[, i] * y[, j])

      ## Same weak FGARP relation as programs/fgarp.R.
      R0[i, j] <- budget_i >= cost_xj || budget_i >= cost_yj

      ## Same strict FGARP relation as programs/fgarp.R.
      if (pi[1, 1] == pi[2, 1]) {
        P0[i, j] <- budget_i > cost_xj || budget_i > cost_yj
      } else {
        P0[i, j] <- budget_i > cost_xj ||
          (budget_i >= cost_yj && !all(x[, j] == y[, j]))
      }
    }
  }

  R <- warshall(R0)

  for (a in seq_len(n)) {
    for (b in seq_len(n)) {
      ## A strict internal edge plus a return path identifies a violating SCC.
      if (P0[a, b] == 1 && R[b, a] == 1) {
        component <- union(
          which(R[a, ] == 1 & R[, a] == 1),
          c(a, b)
        )

        if (any(side[component] == "I") &&
            any(side[component] == "G")) {
          return(0)
        }
      }
    }
  }

  1
}

## e_F^x(S) = sup{e in (0,1] : no mixed-side FGARP e-violation}.
## Uses the same bisection tolerance as ccei_fgarp.R and ex_cross.R.
ex_cross_fgarp <- function(p, x, side, pi, tolerance = 1e-6) {
  if (cross_fgarp(p, x, side, pi, 1) == 1) return(1)

  e_low <- 0
  e_high <- 1
  e_star <- 0

  while (e_high - e_low > tolerance) {
    e <- (e_low + e_high) / 2

    if (cross_fgarp(p, x, side, pi, e) == 1) {
      e_star <- e
      e_low <- e
    } else {
      e_high <- e
    }
  }

  ## Endpoint guard analogous to ex_cross.R. Under FGARP the relation can
  ## change at expenditure ratios for either x_j or its counterfactual y_j.
  y <- rbind(x[2, ], x[1, ])
  if (pi[1, 1] != pi[2, 1]) {
    if (pi[1, 1] < pi[2, 1]) {
      y[, x[1, ] <= x[2, ]] <- x[, x[1, ] <= x[2, ], drop = FALSE]
    } else {
      y[, x[2, ] <= x[1, ]] <- x[, x[2, ] <= x[1, ], drop = FALSE]
    }
  }

  highest_ratio_below_one <- 0
  n <- ncol(p)

  for (i in seq_len(n)) {
    own_exp <- sum(p[, i] * x[, i])
    if (!is.finite(own_exp) || own_exp <= 0) next

    for (j in seq_len(n)) {
      if (j == i) next

      candidate_ratios <- c(
        sum(p[, i] * x[, j]) / own_exp,
        sum(p[, i] * y[, j]) / own_exp
      )

      below_one <- candidate_ratios[
        is.finite(candidate_ratios) & candidate_ratios < 1 - 1e-12
      ]

      if (length(below_one) > 0) {
        highest_ratio_below_one <- max(
          highest_ratio_below_one,
          below_one
        )
      }
    }
  }

  if (e_star > highest_ratio_below_one + 1e-9) return(1)
  e_star
}

## Convenience wrapper for the round-level data used by 01_calculate_ccei.R.
compute_f_ex_from_subsets <- function(
    sub_ind,
    sub_grp,
    pi = matrix(1 / 2, 2, 1)) {

  keep_valid_rows <- function(s) {
    s[
      !is.na(s$coord_x) &
        !is.na(s$coord_y) &
        !is.na(s$intercept_x) &
        !is.na(s$intercept_y) &
        s$intercept_x != 0 &
        s$intercept_y != 0,
      ,
      drop = FALSE
    ]
  }

  individual <- keep_valid_rows(sub_ind)
  group <- keep_valid_rows(sub_grp)

  if (nrow(individual) < 1 || nrow(group) < 1) {
    return(NA_real_)
  }

  p <- rbind(
    c(1 / individual$intercept_x, 1 / group$intercept_x),
    c(1 / individual$intercept_y, 1 / group$intercept_y)
  )

  x <- rbind(
    c(individual$coord_x, group$coord_x),
    c(individual$coord_y, group$coord_y)
  )

  side <- c(
    rep("I", nrow(individual)),
    rep("G", nrow(group))
  )

  ex_cross_fgarp(p, x, side, pi)
}

## FGARP cross-partition distance allocation.
## f_chat(S) = 1 - e_F^x(S).
f_ihat_from_ex <- function(ex_high, ex_low, ex_both, tolerance = 1e-6) {
  if (anyNA(c(ex_high, ex_low, ex_both))) return(NA_real_)

  cost_high <- 1 - ex_high
  cost_low <- 1 - ex_low
  cost_both <- 1 - ex_both

  if (!is.finite(cost_both) || cost_both <= tolerance) {
    return(NA_real_)
  }

  0.5 + (cost_high - cost_low) / (2 * cost_both)
}
