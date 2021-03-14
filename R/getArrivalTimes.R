#' @title getArrivalTimes
#'
#' @description Generate arrival times.
#'
#' @param xDaily Vector that contains the number of arrivals for each day.
#'
#' @importFrom stats runif
#'
#' @return This function returns a data frame of arrival times with the following entries:
#' \describe{
#'     \item{\code{time} (num)}{name of the seized resource}
#'   }
#'
#'   @seealso \code{\link{rkiToBabsimArrivals}}
#'
#' @examples
#' x <- dataCovidBeds20200624
#' arrivalTimes <- getArrivalTimes(xDaily = x$Infected)
#' # For RKI data, use rkiToBabsimArrivals as follows:
#' arrivalTimes <- rkiToBabsimArrivals(rki = babsim.hospital::rkidata)
#' @export
getArrivalTimes <- function(xDaily) {
  # to avoid duplicates:
  orng <- RNGkind()
  on.exit(RNGkind(orng[1], orng[2], orng[3]))
  RNGkind("Wichmann-Hill")

  totalCases <- sum(xDaily)
  # Time = day + fractional part day
  arrivalTimes <- rep(seq_along(xDaily) - 1, xDaily) + runif(totalCases, 0, 1)
  data.frame(time = sort(arrivalTimes))
}
