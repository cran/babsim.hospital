#' @title getPeakVec
#'
#' @description Generate peak values.
#'
#' @param peakData Vector of time steps and peak heights.
#' @param StartDate Day, simulation starts.
#' @param EndDate Day, simulation ends.
#'
#' @return This function returns a vector of peaks data.

#' @examples
#' getPeakVec()
#' @export

getPeakVec <- function(peakData = c(10, 100), StartDate = "2020-03-03", EndDate = "2020-06-24") {
  observedPeriod <- 1 + as.numeric(as.Date(EndDate) - as.Date(StartDate))
  v <- rep(0, observedPeriod)
  n <- length(peakData) / 2
  for (i in 1:n) {
    v[peakData[2 * i - 1]] <- peakData[2 * i]
  }
  return(v)
}
