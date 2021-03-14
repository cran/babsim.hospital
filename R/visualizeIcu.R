#' @title  visualizeIcu Visualisierung der ICU Daten
#'
#' @description Quelle: ICU Daten bundesweit
#'
#' @seealso \code{\link{icudata}}
#'
#'
#' @param data icu data, e.g., \code{\link{icudata}}
#' @param region Region: Gemeindeschluessel, \code{int 05374} fuer OBK oder
#' |code{05315} fuer Koeln oder \code{05911} fuer Bochum.
#'
#' @importFrom padr pad
#' @importFrom stats xtabs
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom graphics plot
#'
#' @examples
#' require("stats")
#' icu <- babsim.hospital::icudata
#' icuCov <- as.data.frame(xtabs(faelle_covid_aktuell ~ daten_stand, icu))
#' icuCov$daten_stand <- as.Date(icuCov$daten_stand)
#' icuCovBeatm <- as.data.frame(xtabs(faelle_covid_aktuell_beatmet ~ daten_stand, icu))
#' icuCovBeatm$daten_stand <- as.Date(icuCovBeatm$daten_stand)
#' dataICUBeds <- data.frame(
#'   bed = (icuCov$Freq - icuCovBeatm$Freq),
#'   intensiveBedVentilation = icuCovBeatm$Freq,
#'   Day = as.Date(icuCovBeatm$daten_stand)
#' )
#'
#' require("padr")
#' require("stats")
#' icu <- babsim.hospital::icudata
#' icu <- pad(icu, interval = "day")
#' icuCov <- as.data.frame(xtabs(faelle_covid_aktuell ~ daten_stand, icu))
#' icuCovBeatm <- as.data.frame(xtabs(faelle_covid_aktuell_beatmet ~ daten_stand, icu))
#' icuCovBett <- as.data.frame(xtabs(betten_belegt ~ daten_stand, icu))
#' plot(icuCov$daten_stand, icuCov$Freq,
#'   type = "p", xlab = "Tag", ylab = "COVID ",
#'   main = "COVID-Faelle in Behandlung im KHaus"
#' )
#' plot(icuCovBeatm$daten_stand, icuCovBeatm$Freq,
#'   type = "p", xlab = "Tag",
#'   ylab = "Patienten", main = "Beatmete COVID-19-Pat. nur invasive Beatmung und ECMO"
#' )
#' plot(icuCovBett$daten_stand, icuCovBett$Freq, type = "l", xlab = "Tag", ylab = "ICU Betten belegt")
#'
#' # Nur Daten des OBK:
#' icu <- babsim.hospital::icudata
#' icu <- icu[icu$gemeindeschluessel == 05374, ]
#' @format data.frame of 9 variables
#' \describe{
#'   \item{bundesland}{ int  1 1 1 1 1 1 1 1 1 1 ...}
#'   \item{gemeindeschluessel}{ int  1001 1002 1003 1004 1051 1053 1054 1055 1056 1057 ...}
#'   \item{anzahl_meldebereiche}{ int  2 3 2 1 1 2 1 3 2 1 ...}
#'   \item{faelle_covid_aktuell}{ int  0 3 5 1 3 1 0 0 5 1 ...}
#'   \item{faelle_covid_aktuell_beatmet}{ int  0 2 5 1 1 1 0 0 4 0 ...}
#'   \item{anzahl_standorte}{ int  2 3 2 1 1 2 1 3 2 1 ...}
#'   \item{betten_frei}{ int  44 113 115 19 54 7 7 18 10 7 ...}
#'   \item{betten_belegt}{ int  38 110 108 19 26 17 3 34 27 5 ...}
#'   \item{daten_stand}{Date, format: '2020-05-01' '2020-05-01' '2020-05-01' '2020-05-01' ... '2020-08-21'}
#' }
#'
#' @return NULL
#'
#' @export

visualizeIcu <- function(data = babsim.hospital::icudata, region = 5315) {
  ## Landkreis
  if (region > 0 & region > 100) {
    data <- data[data$gemeindeschluessel == region, ]
  }
  ## Bundesland
  if (region > 0 & region < 100) {
    data <- data[data$bundesland == region, ]
  }
  data <- pad(data, interval = "day")
  dataCov <- as.data.frame(xtabs(faelle_covid_aktuell ~ daten_stand, data))
  dataCov$daten_stand <- as.Date(dataCov$daten_stand)
  dataCovBeatm <- as.data.frame(xtabs(
    faelle_covid_aktuell_beatmet ~ daten_stand,
    data
  ))
  dataCovBeatm$daten_stand <- as.Date(dataCovBeatm$daten_stand)
  dataCovBett <- as.data.frame(xtabs(betten_belegt ~ daten_stand, data))
  dataCovBett$daten_stand <- as.Date(dataCovBett$daten_stand)
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow = c(1, 2))
  plot(dataCov$daten_stand, dataCov$Freq,
    type = "b", xlab = "Tag", ylab = "COVID-Faelle in Behandlung im KHaus",
    main = paste0("COVID-Intensiv. Region: ", region)
  )
  dataCov$weekly <- slide_dbl(dataCov$Freq, ~ mean(.x), .before = (7 - 1))
  lines(dataCov$daten_stand, dataCov$weekly, col = "red")
  plot(dataCovBeatm$daten_stand, dataCovBeatm$Freq,
    type = "b", xlab = "Tag", ylab = "Beatmete COVID-19-Pat. nur invasive Beatmung und ECMO",
    main = paste0("Beatmete COVID-Intensiv. Region: ", region)
  )
  dataCovBeatm$weekly <- slide_dbl(dataCovBeatm$Freq, ~ mean(.x), .before = (7 -
    1))
  lines(dataCov$daten_stand, dataCovBeatm$weekly, col = "red")
  par(mfrow = c(1, 1))
}
