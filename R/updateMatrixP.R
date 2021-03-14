#' @title updateMatrixP
#'
#' @description Updates the probability matrix
#'
#' @param P matrix P. Default \code{\link{getMatrixP}}
#' @param u list of factors used for the update
#'
#' @return a matrix with updated transition probabilities
#'
#' @examples
#'
#' u <- list(k = 2)
#' R <- updateMatrixP(u = u)
#' @export

updateMatrixP <- function(P = getMatrixP(), u) {
  eps <- .Machine$double.eps * 1e+10
  m <- rep(1, nrow(P))
  # Constrain u$k to [eps, 1e6]
  m[9] <- min(max(u$k, eps), 1e+06)
  Q <- P %*% diag(m)
  S <- diag(1 / rowSums(Q))
  return(S %*% Q)
}
