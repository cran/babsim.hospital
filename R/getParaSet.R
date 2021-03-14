#' @title getParaSet
#'
#' @description Load a specific parameter set by region.
#'
#' @param region integer, the region id
#'
#' @return data.frame parameters of that specific region
#'
#' @examples
#' getParaSet(5315)
#' @export
getParaSet <- function(region) {
  return(babsim.hospital::paras[babsim.hospital::paras$region == region, -ncol(babsim.hospital::paras)])
}
