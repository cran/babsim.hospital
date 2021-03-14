#' @title getMatrixP
#'
#' @description Builds the probability matrix
#'
#' @param para parameter vector, e.g., generated via
#' \code{\link{babsimHospitalPara}}. Default: \code{\link{babsimHospitalPara}}
#'
#'
#' @return a matrix with transition probabilities
#'
#' @examples
#'
#' getMatrixP()
#' @export

getMatrixP <- function(para = babsimHospitalPara()) {
  x <- getStartParameter(para = para)
  P <- matrix(rep(0, 100), nrow = 10, ncol = 10)
  P[1, 2] <- 1 - x[14]
  P[1, 3] <- x[14]
  P[2, 2] <- 1
  P[3, 4] <- 1 - x[15] - x[16]
  P[3, 5] <- x[15]
  P[3, 6] <- x[16]
  P[4, 5] <- x[17]
  P[4, 6] <- x[18]
  P[4, 9] <- x[19]
  P[4, 10] <- 1 - x[17] - x[18] - x[19]
  P[5, 6] <- x[20]
  P[5, 8] <- 1 - x[20] - x[21]
  P[5, 9] <- x[21]
  # P[5,10] = x[25]
  P[6, 7] <- x[22]
  # P[6,8] = 1 - x[26] - x[27]
  P[6, 9] <- 1 - x[22]
  P[7, 8] <- 1 - x[23] - x[29]
  P[7, 9] <- x[23]
  P[7, 10] <- x[29]
  P[8, 10] <- 1
  P[9, 9] <- 1
  P[10, 10] <- 1
  return(P)
}
