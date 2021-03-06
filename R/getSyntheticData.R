#' @title getSyntheticData
#'
#' @description Generate synthetic data
#'
#' @param StartDate Start date. Default: \code{'2020-09-01'}
#' @param EndDate Start date. Default: \code{'2020-11-30'}
#' @param lambda Average number of daily infections. Default: 4
#' @param peakData Vector to define peak events. Odd entries represent days,
#' even entries the number of infections. Default: \code{c(21,50, 28,40, 42,50)},
#' i.e., after 21 days 50 additional infections, after 28 days 40 additional infections,
#' and after 42 days 50 addditional infections to the base infection rate per day,
#' which is defined by \code{lambda}.
#' @param amntDaysSickness Length (in days) of the interval that is used to determine
#' the number of infected individuals that have to go to the hospital. Based on this
#' interval, the number of sick individuals is determined. The number of sick individuals
#' is multiplied by the hospitalization rate to determine the number of bed. Default: 20
#' @param hospitalizationRates list of hospitalization rates, i.e., percentage
#' of sick individuals that need a \code{bed}, or an \code{intensiveBed}, or
#' an \code{intensiveBedVentilation}. Default: \code{list(rBed = 0.1347,
#' rIntensiveBed = 0.004,  rIntensiveBedVentilation = 0.0171)}.
#'
#' @importFrom slider slide_dbl
#'
#' @return data frame with the following entries:
#' bed=bed,
#' intensiveBed = intensiveBed,
#' intensiveBedVentilation = intensiveBedVentilation,
#' Day = Day,
#' Infected=Infected,
#' Sick = Sick)
#' \describe{
#'   \item{\code{bed}}{int: COVID-19 beds}
#'   \item{\code{intensiveBed}}{int: COVID-19 ICU beds}
#'   \item{\code{intensiveBedVentilation}}{int  COVID-19 ICU beds with ventilation}
#'   \item{\code{Day}}{Date, format: '2020-05-01' '2020-05-02' '2020-05-03' '2020-05-04' ...}
#'   \item{\code{Infected}}{int: number of infected individuals (daily)}
#'   \item{\code{Sick}}{int: number of sick individuals (daily)}
#'  }
#'
#' @examples
#'
#' dataSynth <- getSyntheticData()
#' @export


getSyntheticData <- function(StartDate = "2020-09-01", EndDate = "2020-11-30", lambda = 4,
                             peakData = c(21, 50, 28, 40, 42, 50), amntDaysSickness = 20, hospitalizationRates = list(
                               rBed = 0.1347,
                               rIntensiveBed = 0.004, rIntensiveBedVentilation = 0.0171
                             )) {
  Infected <- getInfectedPerDay(lambda = lambda, StartDate = StartDate, EndDate = EndDate) +
    getPeakVec(peakData = peakData, StartDate = StartDate, EndDate = EndDate)
  Day <- seq(from = as.Date(StartDate), to = as.Date(EndDate), by = "1 day")
  Sick <- slide_dbl(Infected, ~ sum(.x), .before = (amntDaysSickness - 1))
  bed <- round(hospitalizationRates$rBed * Sick)
  intensiveBed <- round(hospitalizationRates$rIntensiveBed * Sick)
  intensiveBedVentilation <- round(hospitalizationRates$rIntensiveBedVentilation *
    Sick)

  simData <- data.frame(Day = Day, Infected = Infected)
  attr(simData, "StartDate") <- min(simData$Day)
  attr(simData, "EndDate") <- max(simData$Day)
  attr(simData, "Days") <- as.integer(1 + max(simData$Day) - min(simData$Day))
  attr(simData, "FeatureNames") <- c("Infected")

  fieldData <- data.frame(Day = Day, bed = bed, intensiveBed = intensiveBed, intensiveBedVentilation = intensiveBedVentilation)
  attr(fieldData, "StartDate") <- min(fieldData$Day)
  attr(fieldData, "EndDate") <- max(fieldData$Day)
  attr(fieldData, "Days") <- as.integer(1 + max(fieldData$Day) - min(fieldData$Day))
  attr(fieldData, "ResourceNames") <- c("bed", "intensiveBed", "intensiveBedVentilation")

  return(list(simData = simData, fieldData = fieldData))
}
