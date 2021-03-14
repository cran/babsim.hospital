#' @title getInfectedPerDay
#'
#' @description Generate Poisson distributed infections.
#' This function calculates \code{n}, the number of days between \code{StartDate}
#' and \code{EndDate}, and returns a vector with \code{n} realizations of a
#' Poisson(lambda) distributed random variable.
#'
#' @param lambda Expected number of infections/day.
#' @param StartDate Day, simulation starts
#' @param EndDate Day, simulation ends
#'
#' @importFrom stats rpois
#'
#' @return This function returns a vector that lists the number of infections.

#' @examples
#' StartDate <- "2020-03-03"
#' EndDate <- "2020-06-24"
#' getInfectedPerDay(lambda = 4, StartDate = StartDate, EndDate = EndDate)
#' @export

getInfectedPerDay <- function(lambda = 4, StartDate = "2020-03-03", EndDate = "2020-06-24") {
  observedPeriod <- 1 + as.numeric(as.Date(EndDate) - as.Date(StartDate))
  return(rpois(observedPeriod, lambda))
}
