#'
#' @title getBounds
#'
#' @description Returns parameter bounds for babsim runs
#' (settings version > v10.4.8)
#'
#' @return This function returns a list of two vectors
#' @examples
#' bounds <- getBounds()
#' lower <- bounds$lower
#' upper <- bounds$upper
#' @export
getBounds <- function() {
  n <- length(getStartParameter())
  a <- rep(0, n)
  a[1] <- 6
  a[2] <- 7
  a[3] <- 3
  a[4] <- 3
  a[5] <- 3
  a[6] <- 5
  a[7] <- 3
  a[8] <- 3
  a[9] <- 25
  a[10] <- 17
  a[11] <- 2
  a[12] <- 1
  a[13] <- 0.25
  a[14] <- 0.05
  a[15] <- 0.07 # 0.01 #0.005999
  a[16] <- 0.005 # 0.017999
  a[17] <- 0.07 # 0.025299
  a[18] <- 1e-04 # 0.050649
  a[19] <- 0.08 # 0.069499
  a[20] <- 0.25 # 0.124999
  a[21] <- 0.08 # 0.124999
  a[22] <- 0.5 # 0.209999
  a[23] <- 1e-06
  a[24] <- 2 # 14
  a[25] <- 1e-06
  a[26] <- 1e-06
  a[27] <- 1
  a[28] <- 2
  a[29] <- 0.5

  b <- rep(0, n)
  b[1] <- 14
  b[2] <- 13
  b[3] <- 7
  b[4] <- 9
  b[5] <- 7
  b[6] <- 9
  b[7] <- 5
  b[8] <- 7
  b[9] <- 35
  b[10] <- 25
  b[11] <- 5
  b[12] <- 7
  b[13] <- 2
  b[14] <- 0.15
  b[15] <- 0.11 # 0.08         #0.018001
  b[16] <- 0.02 # 0.08         #0.054001
  b[17] <- 0.13 # 0.075901
  b[18] <- 0.002 # 0.151951
  b[19] <- 0.12 # 0.208501
  b[20] <- 0.35 # 0.375001
  b[21] <- 0.12 # 0.375001
  b[22] <- 0.9 # 0.630001
  b[23] <- 0.01
  b[24] <- 4 # 28
  b[25] <- 1.1
  b[26] <- 0.0625
  b[27] <- 2
  b[28] <- 5
  b[29] <- 0.75

  ifelse(sum(a >= b) == 0, return(list(lower = a, upper = b)), stop("getBounds: inconsistent bounds"))
}
