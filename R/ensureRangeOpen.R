#' @title ensureRangeOpen
#' @description Ensure that value belongs to the open interval ]a,b[
#' @param x value
#' @param a lower limit
#' @param b upper limit
#' @return corrected value
#' @examples
#' # return 1:
#' ensureRangeOpen(x = 10, a = 0, b = 1)
#' # return 0:
#' ensureRangeOpen(x = 0, a = 0, b = 1)
#' # return 0.5:
#' ensureRangeOpen(x = 0.5, a = 0, b = 1)
#' @export
#'
ensureRangeOpen <- function(x, a, b) {
  if (x < a) {
    return(a)
  } else {
    if (x > b) {
      return(b)
    } else {
      return(x)
    }
  }
}
