#' @title getIcuBeds
#'
#' @description Convert the 9 dim DIVI ICU data (bundesland,gemeindeschluessel,..., daten_stand)
#' into a data.frame with bed, intensiveBedVentilation, and Day
#'
#' @param data data.frame with obs. of  9 variables
#'
#' @return data frame with observations of 3 variables:
#' \describe{
#'   \item{intensiveBed}{int COVID-19 ICU beds without ventilation}
#'   \item{intensiveBedVentilation}{int  COVID-19 ICU beds with ventilation}
#'   \item{Day}{Date, format: '2020-05-01' '2020-05-02' '2020-05-03' '2020-05-04' ...}
#'  }
#'
#'
#' @examples
#'
#' IcuBeds <- getIcuBeds(data = icudata)
#' @export
getIcuBeds <- function(data = babsim.hospital::icudata) {
  icuCov <- as.data.frame(xtabs(faelle_covid_aktuell ~ daten_stand, data))
  icuCov$daten_stand <- as.Date(icuCov$daten_stand)
  icuCovBeatm <- as.data.frame(xtabs(
    faelle_covid_aktuell_beatmet ~ daten_stand,
    data
  ))
  icuCovBeatm$daten_stand <- as.Date(icuCovBeatm$daten_stand)
  dataIcuBeds <- data.frame(
    intensiveBed = (icuCov$Freq - icuCovBeatm$Freq), intensiveBedVentilation = icuCovBeatm$Freq,
    Day = as.Date(icuCovBeatm$daten_stand)
  )
  return(dataIcuBeds)
}
