## ---------------------------------------------------------------------------
## Cross-partition maximum MPI and the corresponding RevMaxMPI Ihat index.
##
## The installed revpref package computes maximum MPI as the negative of the
## minimum cost-to-time ratio over cycles in its revealed-preference graph.
## This file leaves that optimization algorithm unchanged. It only removes
## graph components that do not contain observations from BOTH partitions,
## then calls revpref's original minimum_cost_time() implementation.
##
## side labels:
##   "I" = individual-side choices
##   "G" = group-side choices
##
## Proposed interpretation (chosen for its consistency with ex_cross.R):
## a cross cycle is a directed closed walk supported by a strongly connected
## component containing both I and G observations. A within-side cycle inside
## such a mixed SCC is therefore eligible under the closed-walk convention.
## This convention should be confirmed by the authors before substantive use.
##
## Requires:
##   - revpref package
##   - warshall() from programs/warshall.R
##
## Reference source retained in this replication package:
##   programs/revpref-master/R/mpi.R
##   programs/revpref-master/R/utils.R
## ---------------------------------------------------------------------------

## Retrieve the exact optimization helpers used by the installed revpref
## package. Using getFromNamespace() avoids altering or masking package code.
revpref_mpi_helper <- function(name) {
  if (!requireNamespace("revpref", quietly = TRUE)) {
    stop("Package 'revpref' is required for cross-MaxMPI calculation.")
  }

  getFromNamespace(name, "revpref")
}

## Convert revpref's graph-list representation to a binary adjacency matrix.
revpref_graph_adjacency <- function(graph) {
  n <- length(graph)
  adjacency <- matrix(0, n, n)

  for (i in seq_len(n)) {
    if (length(graph[[i]]) == 0) next

    for (edge in graph[[i]]) {
      j <- as.integer(edge[1])
      adjacency[i, j] <- 1
    }
  }

  adjacency
}

## Identify nodes belonging to an SCC that contains both I and G observations.
mixed_side_scc_nodes <- function(graph, side) {
  n <- length(graph)

  if (length(side) != n) {
    stop("side must contain one label for each graph node.")
  }
  if (!all(side %in% c("I", "G"))) {
    stop("side labels must be either 'I' or 'G'.")
  }
  if (n == 0) return(integer(0))

  adjacency <- revpref_graph_adjacency(graph)
  reachability <- warshall(adjacency)
  visited <- rep(FALSE, n)
  eligible <- rep(FALSE, n)

  for (i in seq_len(n)) {
    if (visited[i]) next

    component <- union(
      i,
      which(reachability[i, ] == 1 & reachability[, i] == 1)
    )
    visited[component] <- TRUE

    if (any(side[component] == "I") &&
        any(side[component] == "G")) {
      eligible[component] <- TRUE
    }
  }

  which(eligible)
}

## Keep only edges whose two endpoints belong to mixed-side SCCs.
restrict_graph_to_nodes <- function(graph, keep_nodes) {
  n <- length(graph)
  keep <- rep(FALSE, n)
  keep[keep_nodes] <- TRUE
  restricted <- vector("list", n)

  for (i in seq_len(n)) {
    restricted[[i]] <- list()
    if (!keep[i] || length(graph[[i]]) == 0) next

    retained_edges <- Filter(
      function(edge) keep[as.integer(edge[1])],
      graph[[i]]
    )

    restricted[[i]] <- retained_edges
  }

  restricted
}

## Maximum MPI over mixed-side SCCs.
## p and q follow revpref::mpi(): rows are observations, columns are goods.
cross_max_mpi <- function(p, q, side) {
  if (!is.matrix(p) || !is.matrix(q)) {
    stop("p and q must be matrices.")
  }
  if (!all(dim(p) == dim(q))) {
    stop("p and q must have identical dimensions.")
  }
  if (length(side) != nrow(p)) {
    stop("side must contain one label for each observation.")
  }
  if (!all(side %in% c("I", "G"))) {
    stop("side labels must be either 'I' or 'G'.")
  }

  ## Match revpref::mpi(): discard observations with an all-zero bundle.
  keep <- apply(q, 1, sum) != 0
  p_clean <- p[keep, , drop = FALSE]
  q_clean <- q[keep, , drop = FALSE]
  side_clean <- side[keep]

  if (nrow(p_clean) < 2 ||
      !any(side_clean == "I") ||
      !any(side_clean == "G")) {
    return(0)
  }

  graph_list <- revpref_mpi_helper("graph_list")
  cycle_detection <- revpref_mpi_helper("cycle_detection_topo")
  minimum_cost_time <- revpref_mpi_helper("minimum_cost_time")

  ## bound = 1 is the graph used for maximum MPI in revpref::mpi().
  full_graph <- graph_list(p_clean, q_clean, 1)
  eligible_nodes <- mixed_side_scc_nodes(full_graph, side_clean)

  if (length(eligible_nodes) == 0) return(0)

  cross_graph <- restrict_graph_to_nodes(full_graph, eligible_nodes)
  if (cycle_detection(cross_graph) == 0) return(0)

  value <- -minimum_cost_time(
    cross_graph,
    -100000000,
    100000000
  )

  round(as.numeric(value), 10)
}

## Convenience wrapper for the round-level data used by 01_calculate_ccei.R.
compute_cross_max_mpi_from_subsets <- function(sub_ind, sub_grp) {
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
    cbind(1 / individual$intercept_x, 1 / individual$intercept_y),
    cbind(1 / group$intercept_x, 1 / group$intercept_y)
  )

  q <- rbind(
    cbind(individual$coord_x, individual$coord_y),
    cbind(group$coord_x, group$coord_y)
  )

  side <- c(
    rep("I", nrow(individual)),
    rep("G", nrow(group))
  )

  cross_max_mpi(p, q, side)
}

## RevMaxMPI is the consistency-oriented transformation used in the project.
cross_rev_max_mpi <- function(cross_max_mpi_value) {
  ifelse(
    is.na(cross_max_mpi_value),
    NA_real_,
    1 - cross_max_mpi_value
  )
}

## Cross-partition distance allocation for RevMaxMPI.
## Because 1 - RevMaxMPI = MaxMPI, the allocation uses cross-MaxMPI directly.
ihat_from_cross_max_mpi <- function(
    max_mpi_high,
    max_mpi_low,
    max_mpi_both,
    tolerance = 1e-10) {

  if (anyNA(c(max_mpi_high, max_mpi_low, max_mpi_both))) {
    return(NA_real_)
  }
  if (!is.finite(max_mpi_both) || max_mpi_both <= tolerance) {
    return(NA_real_)
  }

  0.5 +
    (max_mpi_high - max_mpi_low) /
      (2 * max_mpi_both)
}
