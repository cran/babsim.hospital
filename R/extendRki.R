#' @title  extendRki Erweiterung der RKI Daten
#'
#' @description Combine existing data with synthetic data
#'
#' @seealso \code{\link{getRkiData}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom stats rbinom
#' @importFrom utils tail
#'
#' @param data rki data, e.g., \code{getRkiData(babsim.hospital::rkidata)}
#' @param EndDate Ende (Tag), e.g., \code{'2020-05-04'}
#' @param R0 Basisreproduktionszahl. Constant, if a scalar value is given.
#' If a vector of two values are given, they will be interpreted as
#' a start and an end value, respectively. \code{c(1,2)} defines an increasing
#' \code{R0} value from \code{1} to \code{2}. Default: \code{1}, i.e., constant \code{1}.
#' Note: This is NOT exactly the same R0 value presented by the Robert-Koch Institute,
#' please refer to \url{https://en.wikipedia.org/wiki/Basic_reproduction_number}
#' for our implementation.
#' @param tau Ansteckungszeitraum in Tagen
#'
#'
#' @examples
#' # take 10,000 data points only:
#' data <- getRkiData(babsim.hospital::rkidata[1:10000, ])
#' # check whether enough data are provided
#' if (dim(data)[1] > 1e6){
#' n <- as.integer(max(data$Day) - min(data$Day))
#' StartDay <- min(data$Day) + round(n * 0.995)
#' data <- data[which(data$Day >= StartDay), ]
#' EndDate <- max(data$Day) + 2
#' dataExt <- extendRki(
#'   data = data,
#'   EndDate = EndDate,
#'   R0 = c(0.1, 0.2)
#' )
#' }
#' @export
extendRki <- function(data = getRkiData(babsim.hospital::rkidata), EndDate = max(data$Day) +
                        28, R0 = c(1, 1), tau = 5) {
  StartDate <- max(data$Day) + 1
  EndDate <- as.Date(EndDate, origin="1970-01-01")
  Day <- seq(StartDate, EndDate, by = 1)
  time <- seq_along(Day)


  N_DAYS_INTO_FUTURE <- as.numeric(EndDate - max(data$Day))


  # Initial number of cases is average number of cases in the last 7 days.
  InitialCases <- sum(data$Day >= (StartDate - 7)) / 7
  InitialCases <- max(InitialCases, 1)

  # Extend R0 for each day.
  R0 <- if (length(R0) > 1) {
    seq(R0[1], R0[2], length.out = length(Day))
  } else {
    rep.int(R0, times = length(Day))
  }
  # FIXME: We should really add some randomness here!  One approach would be to add
  # a small relative error to R0 (say 5%)
  K <- log(R0) / tau
  CasesPerDay <- round(InitialCases * exp(K * time))

  ## Repeat for each case
  Day <- rep.int(Day, times = CasesPerDay)
  time <- rep.int(time, times = CasesPerDay) + max(data$time)

  # Geschlecht gleichverteilt auswürfeln.
  Geschlecht <- sample(c("W", "M"), size = length(Day), replace = TRUE, prob = c(
    0.5,
    0.5
  ))

  # Wir brauchen für die 'neuen' Fälle ein Bundesland und Landkreis.  Dafür ziehen
  # wir zufällig Orte aus den bestehenden Daten. Das ist nicht optimal...
  location <- sample(nrow(data), size = length(Day), replace = TRUE)

  # Alter entsprechend der Verteilung in der Historie auswürfeln.  Wir gewichten
  # 'junge' Fälle, d.h. die nahe Vergangenheit, höher um die sich wandelnde
  # Altersverteilung zu berücksichtigen.  Verbesserung wäre ggf. noch pro
  # Bundesland zu ziehen um die (leicht) unterschiedliche Altersverteilung je
  # Bundesland zu berücksichtigen.
  ageProb <- exp(as.numeric(data$Day - max(data$Day)) / N_DAYS_INTO_FUTURE)
  Age <- sample(data$Age, length(Day), replace = TRUE, prob = ageProb)

  extData <- data.frame(
    Altersgruppe = rep("A-", length(Day)), Geschlecht = Geschlecht,
    Day = Day, IdBundesland = data$IdBundesland[location], IdLandkreis = data$IdLandkreis[location],
    time = time, Age = Age
  )
  rbind(data, extData)
}
