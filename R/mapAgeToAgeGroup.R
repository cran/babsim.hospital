#' @title mapAgeToAgeGroup
#'
#' @description Calculate age based on RKI age classes
#'
#' @param x age vector
#'
#' @return age class
#' @export
mapAgeToAgeGroup <- function(x) {
  cut(x, breaks = c(-1, 4, 14, 34, 59, 79, 120), labels = c(
    "A00-A04", "A05-A14",
    "A15-A34", "A35-A59", "A60-A79", "A80+"
  ))
}
