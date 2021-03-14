#' @title getParameterNameList
#'
#' @description Returns the names (chr) of the babsim \code{x} parameter vector.
#'
#' @param x vector of int: positions

#' @return This function returns a vector. Its elements represent
#' the names of the n-th \code{x} variables.

#' @examples
#' getParameterNameList(c(16, 18))
#' @export


getParameterNameList <- function(x) {
  y <- matrix(x, 1, )
  y <- apply(X = y, FUN = getParameterName, 2)
  names(y) <- paste0("x", x)
  return(y)
}
