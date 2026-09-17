#' Tests consistency with the Weak Axiom of Revealed Preference at efficiency \eqn{e}
#'
#' This function allows the user to check whether a given data set is consistent with the Weak Axiom of Revealed
#' Preference at efficiency level \eqn{e} (\eqn{e}WARP) and computes the number of \eqn{e}WARP violations.
#' We say that a data set satisfies WARP at efficiency level \eqn{e} if \eqn{q_t R^D_e q_s} and \eqn{q_t \neq q_s}
#' implies \eqn{ep_s'q_s < p_s'q_t} (see the definition of R^D_e below). The exact WARP, with \eqn{e = 1}, is a necessary and sufficient
#' condition for a data set to be rationalizable by a continuous, strictly increasing, piecewise strictly concave,
#' and skew-symmetric preference function (see Aguiar et al. (2020)). Moreover, Rose (1958) showed that for the case
#' of two goods (\eqn{N = 2}), WARP is equivalent to the Strong Axiom of Revealed Preference (SARP). In other words,
#' when there are only two consumption categories, transitivity has no empirical bite.
#'
#' @param p A \eqn{T \times N} matrix of observed prices where each row corresponds to an observation
#' and each column corresponds to a consumption category. \eqn{T} is the number of observations
#' and \eqn{N} is the number of consumption categories.
#'
#' @param q A \eqn{T \times N} matrix of observed quantities where each row corresponds to an observation
#' and each column corresponds to a consumption category.\eqn{T} is the number of observations
#' and \eqn{N} is the number of consumption categories.
#'
#' @param efficiency The efficiency level \eqn{e}, is a real number between 0 and 1, which allows for a
#' small margin of error when checking for consistency with the axiom. The default value is 1, which corresponds to the
#' test of consistency with the exact WARP.
#'
#' @return The function returns two elements. The first element (\code{passwarp}) is a binary indicator telling us
#' whether the data set is consistent with WARP at a given efficiency level \eqn{e}. It takes a value 1 if the data set
#' is \eqn{e}WARP consistent and a value 0 if the data set is \eqn{e}WARP inconsistent.
#' The second element (\code{nviol}) reports the number of \eqn{e}WARP violations. If the data set is \eqn{e}WARP
#' consistent, \code{nviol} is 0. Note that the maximum number of violations in an \eqn{e}WARP inconsistent data is
#' \eqn{T(T-1)/2}.
#'
#' @section Definitions:
#' For a given efficiency level \eqn{0 \le e \le 1}, we say that:
#'
#' \itemize{
#'
#' \item bundle \eqn{q_t} is directly revealed preferred to bundle \eqn{q_s} at efficiency level \eqn{e} (denoted as
#' \eqn{q_t R^D_e q_s}) if \eqn{ep_t'q_t \ge p_t'q_s}.
#'}
#'
#'
#' @section References:
#' \itemize{
#' \item Aguiar, Victor, Per Hjertstrand, and Roberto Serrano. "A Rationalization of the Weak Axiom of Revealed
#'  Preference." (2020).
#' \item Rose, Hugh. "Consistency of preference: the two-commodity case." The Review of Economic Studies 25,
#'  no. 2 (1958): 124-125.
#' }
#'
#'
#' @examples
#'
#' # define a price matrix
#' p = matrix(c(4,4,4,1,9,3,2,8,3,1,
#' 8,4,3,1,9,3,2,8,8,4,
#' 1,4,1,8,9,3,1,8,3,2),
#' nrow = 10, ncol = 3, byrow = TRUE)
#'
#' # define a quantity matrix
#' q = matrix(c( 1.81,0.19,10.51,17.28,2.26,4.13,12.33,2.05,2.99,6.06,
#' 5.19,0.62,11.34,10.33,0.63,4.33,8.08,2.61,4.36,1.34,
#' 9.76,1.37,36.35, 1.02,3.21,4.97,6.20,0.32,8.53,10.92),
#' nrow = 10, ncol = 3, byrow = TRUE)
#'
#' # Test consistency with WARP and compute the number of WARP violations
#' warp(p,q)
#'
#' # Test consistency with WARP and compute the number of WARP violations at e = 0.95
#' warp(p,q, efficiency = 0.95)
#'
#' @seealso \code{\link{sarp}} for the Strong Axiom of Revealed Preference and \code{\link{garp}} for
#' the Generalized Axiom of Revealed Preference.
#'
#' @export
#'
warp <- function(p,q,efficiency=1)
{
  if (nrow(p)!=nrow(q)) stop("Number of observations must be the same for both price and quanity matrices")
  if (ncol(p)!=ncol(q)) stop("Number of consumption categories must be the same for both price and quanity matrices")
  if (!(0 <= efficiency & efficiency <= 1)) stop("Efficiency index must be between 0 and 1")

  passwarp <- 1
  t <- dim(p)[1]
  DRP <- diag(t)
  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(sum(p[i,]*q[j,]) <= efficiency*sum(p[i,]*q[i,])) {DRP[i,j] = 1}
    }
  }


  nviol = 0
  for(i in 1:t)
  {
    for(j in 1:t)
    {
      if(i != j)
      {if(DRP[i,j] == 1 & DRP[j,i] == 1 & !identical(q[i,],q[j,])) # i is revealed preferred to j & j is directly revealed preferred to i
      {
        passwarp = 0
        nviol = nviol + 1
      }
      }
    }
  }
  nviol = nviol/2
  return(c(passwarp,nviol))
}
