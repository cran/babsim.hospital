#' @title softmax
#'
#' @description softmax function
#'
#' @param par vector
#'
#' @return num vector with components >= 0 and sum = 1
#'
#' @examples
#'
#' p <- c(0.6, 0.3, 0.1)
#' softmax(p)
#' @export

softmax <- function(par) {
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par - 1)) {
    Lk <- max(par1[k + 1], Lk) + log1p(exp(-abs(par1[k + 1] - Lk)))
  }
  val <- exp(par - Lk)
  return(val)
}
