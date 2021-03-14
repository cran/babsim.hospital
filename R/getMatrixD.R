#' @title getMatrixD
#'
#' @description Builds the duration matrix
#'
#' @param para parameter vector, e.g., generated via
#' \code{\link{babsimHospitalPara}}. Default: \code{\link{babsimHospitalPara}}
#'
#'
#' @return a matrix with transition durations
#'
#' @examples
#'
#' getMatrixD()
#' @export

getMatrixD <- function(para = babsimHospitalPara()) {
  x <- getStartParameter(para = para)
  D <- matrix(rep(0, 100), nrow = 10, ncol = 10)
  D[1, 3] <- x[1]
  D[4, 5] <- x[3]
  D[4, 6] <- x[4]
  D[4, 9] <- x[5]
  D[4, 10] <- x[2]
  D[5, 6] <- x[7]
  D[5, 8] <- x[6]
  D[5, 9] <- x[8]
  # D[5,10] = x[9]
  D[6, 7] <- x[9]
  # D[6,8] = x[10]
  D[6, 9] <- x[10]
  D[7, 8] <- x[11]
  D[7, 9] <- x[12]
  D[7, 10] <- x[28]
  D[8, 10] <- x[24]
  return(D)
}
