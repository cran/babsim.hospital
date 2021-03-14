#' @title getDecision
#'
#' @description For given n probabilities 0 <= pi <= 1 with sum(pi)=1,
#' return 0,1,2,3,..,n with probability p0, p1, p2, ..., pn.
#'
#' @param p vector of probabilities
#'
#' @return int decision in the range from 0 to n
#'
#' @examples
#'
#' p <- c(0.6, 0.3, 0.1)
#' getDecision(p)
#' @export

getDecision <- function(p) {
  x <- runif(1)
  p <- cumsum(c(0, p))
  sum(x > p) - 1
}
