#' Display date range of a vector
#'
#' @description Utility function to display the range of dates in a vector.
#'
#' @param prefix string to print before outputting range.
#' @param d vector of values.
#'
#' @return Nothing, called for the side effect.
#' 
#' @export
messageDateRange <- function(prefix, d) {
  r <- range(d)
  messagef("%s: %s - %s", prefix, r[1], r[2])
}
