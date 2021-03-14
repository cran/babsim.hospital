#' @title rtgamma
#'
#' @param n number of observations
#' @param shape Gamma shape parameter
#' @param rate  Gamma rate parameter
#' @param shift shift parameter.
#' @param alpha upper quantile of gamma distribution. All values above alpha are truncated.
#'
#' @description Random generation for the shifted and truncated Gamma distribution with parameters
#'   \code{shape} and \code{scale}.
#'
#' @return rtgamma generates random deviates. The length of the result is determined by \code{n}.
#'
#' @examples
#'
#' rtgamma(n = 1, shape = 1, rate = 1, shift = 1, alpha = 0.95)
#' @importFrom stats qgamma
#' @export
#'
rtgamma <- function(n = 1, shape = 1, rate = 1, shift = 0, alpha = 0.95) {
  RNGkind("Wich")
  u <- runif(n, 0, alpha)
  shift + qgamma(u, shape = shape, rate = rate)
}
