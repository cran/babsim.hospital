#' @title mapAgeGroupToAge
#'
#' @description Calculate real valued age based on RKI age classes
#'
#' @param x age vector
#'
#' @return age as a real value
#' @export
mapAgeGroupToAge <- function(x) {
  AgeMap <- c(
    `A00-A04` = 2, `A05-A14` = 10, `A15-A34` = 25, `A35-A59` = 47, `A60-A79` = 70,
    `A80+` = 90, unbekannt = 47
  )
  unname(AgeMap[x])
}
