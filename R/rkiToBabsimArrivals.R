#' @title rkiToBabsimArrivals
#'
#' @description Transforms the freshly downloaded rki data into the babsim fitting format of
#' arrival times. Also imputes mising dates as zero frequency in the data.
#'
#' @param rki data.frame of downloaded rki data before preprocessing
#'
#' @return a data.frame of arrival times suited for babsimHospital
#'
#' @examples
#'
#' arrivals <- rkiToBabsimArrivals(rkidata[1:100, ])
#' min(as.Date(rkidata$Refdatum)) + max(arrivals)
#' @export

rkiToBabsimArrivals <- function(rki) {
  rkiAgg <- rkiToBabsimData(rki)
  return(data.frame(time = getArrivalTimes(rkiAgg$Infected)))
}
