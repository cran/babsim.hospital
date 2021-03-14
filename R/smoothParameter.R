#' @title Smooth a parameter set using another parameter set
#'
#' @description Calculate the average of two parameter sets to smooth out any local anomalies.
#' Mostly useful to smooth out a local (say OBK) parameter set using a global one (say NRW).
#'
#' Technically this function calculates
#'   \code{(1-weight) * para + weight * other}
#' ensuring that the names etc. of \code{para} are preserved.
#'
#' @param para Parameter set to smooth
#' @param other Other parameters to average in
#' @param weight Weight of other parameters
#'
#' @return Weighted parameter set
#'
#' @export
smoothParameter <- function(para, other, weight = 0.2) {
  newParameters <- as.list(unlist(para) * (1 - weight) + unlist(other) * weight)
  names(newParameters) <- names(para)
  newParameters
}
