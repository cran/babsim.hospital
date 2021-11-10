#' @title getParameterDataFrame
#'
#' @description Get parameterss (probabilities and durations) of the
#' babsim.hospital simulator
#'
#' @param paraList list of parameter values.
#' Each list element has the form \code{obk=getParaSet(5374)}.
#'
#' @return data.frame with parameters in each column.
#'
#' @examples
#'
#' df <- getParameterDataFrame()
#' @export
getParameterDataFrame <- function(paraList = list(
                                    obk = getParaSet(5374), koeln = getParaSet(5315)
                                  )) {
  x <- data.frame(getStartParameter())
  n <- length(x)
  p <- as.character(getParameterNameList(1:n))
  colnames(x) <- p
  x <- t(x)
  for (para in paraList) {
    yColumn <- match("y", colnames(para))
    bestRow <- which.min(para[, yColumn])
    q <- unlist(para[bestRow, -yColumn], use.names = FALSE)
    x <- cbind(x, q)
  }
  bounds <- getBounds()
  x <- cbind(x, bounds[[1]], bounds[[2]])

  colnames(x) <- c("default", names(paraList), "min", "max")
  return(x)
}
