#' Output a formatted message
#'
#' @param fmt format string (see \code{\link{sprintf}} for details)
#' @param ... values passed into \code{fmt}.
#'        Only logical, integer, real and character vectors are supported, but some coercion will be done.
#'
#' @return Nothing, called for the side effect of outputting a message to the console.
#' @export
#' 
messagef <- function(fmt, ...) {
  message(sprintf(fmt, ...))
}
