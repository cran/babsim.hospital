#' @title getRealBeds
#'
#' @description
#' Convert daily data, e.g., a data.frame with the columns
#' \code{bed, intensiveBedVentilation, Day}
#' into event data, e.g., a data.frame with the following columns
#' \code{resource, time,  med, source, date}.
#'
#' @details
#' Prepares data for combination with output (env)
#' from \link[simmer]{simmer}.
#' Extracts and formats real data from the real data sets, e.g.,
#' \code{icudata}.
#' The resulting data frame can be combined with
#' the output from the simulation run.
#' Can be used to add the true data (ground truth) to the simulated data.
#'
#' @param data (n, m) data frame with daily bed data, e.g.,
#' from \code{\link{icudata}}
#' @param resource vector of resource names, e.g., 'bed'. Default:
#'  \code{resource=c('bed', 'intensiveBedVentilation')}. For GA data use:
#'  \code{resource=c('bed', 'intensiveBed', 'intensiveBedVentilation')}
#'
#' @return This function returns a (n x m, 5) data frame with:
#' \describe{
#'     \item{\code{resource} (chr)}{name of the seized resource}
#'     \item{\code{time} (int)}{time step, starts with \code{1}}
#'     \item{\code{med} (int)}{amount of the seized resource}
#'     \item{\code{source} (chr)}{name of the simulation that was used}
#'     \item{\code{date} (Date)}{time, format: \code{yyyy-mm-dd}}
#'   }
#'
#' @seealso \code{\link{getIcuBeds}}.
#' @examples
#' # First example shows how to process the GA data
#' GABeds <- getRealBeds(
#'   data = babsim.hospital::dataCovidBeds20200624,
#'   resource = c("bed", "intensiveBed", "intensiveBedVentilation")
#' )
#'
#' # Second example shows how to process the DIVI ICU data.
#' icu <- babsim.hospital::icudata
#' icuCov <- as.data.frame(xtabs(faelle_covid_aktuell ~ daten_stand, icu))
#' icuCov$daten_stand <- as.Date(icuCov$daten_stand)
#' icuCovBeatm <- as.data.frame(xtabs(faelle_covid_aktuell_beatmet ~ daten_stand, icu))
#' icuCovBeatm$daten_stand <- as.Date(icuCovBeatm$daten_stand)
#' Day <- as.Date(icuCovBeatm$daten_stand)
#' dataICUBeds20200821 <- data.frame(
#'   bed = (icuCov$Freq - icuCovBeatm$Freq),
#'   intensiveBedVentilation = icuCovBeatm$Freq,
#'   Day = as.Date(icuCovBeatm$daten_stand)
#' )
#' ICUBeds <- getRealBeds(
#'   data = dataICUBeds20200821,
#'   resource = c("bed", "intensiveBedVentilation")
#' )
#' @export
getRealBeds <- function(data, resource) {
  n <- nrow(data)
  t <- 0:(n - 1)
  do.call(rbind, lapply(resource, function(r) {
    data.frame(resource = rep(r, n), time = t, med = data[, r], source = rep(
      "GA",
      n
    ), date = data$Day)
  }))
}
