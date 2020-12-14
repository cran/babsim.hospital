
###################################################################################
#' @title  icudata IntensivbettenDaten (aktueller Stand)
#' 
#' @description ICU Daten bundesweit
#' 
#' @details 1. Heruntergeladen mittels 
#' \code{getDiviData}
#' 2. Manuell kopiert in den Ordner All
#' 3. Eingelesen  mittels \code{
#' icudata <- readCsvRecursively(path="All", pattern="download")
#' }
#' 4. Weiterverarbeitung  mittels \code{\link{getIcuBeds}} 
#' \code{icu <- icudata
#' icuCov <- as.data.frame(xtabs( faelle_covid_aktuell ~  daten_stand, icu)) 
#' icuCov$daten_stand <- as.Date(icuCov$daten_stand)
#' icuCovBeatm <- as.data.frame(xtabs(faelle_covid_aktuell_beatmet ~ daten_stand, icu)) 
#' icuCovBeatm$daten_stand <- as.Date(icuCovBeatm$daten_stand)
#' dataICUBeds <- data.frame(bed=(icuCov$Freq - icuCovBeatm$Freq), intensiveBedVentilation=icuCovBeatm$Freq, Day =  as.Date(icuCovBeatm$daten_stand))
#' }
#' Oder  Weiterverarbeitung z.B. mit:
#' \code{library(padr)
#' icu <- pad(icudata,interval="day")
#' icuCov <- as.data.frame(xtabs( faelle_covid_aktuell ~ daten_stand, icu))
#' icuCovBeatm <- as.data.frame(xtabs( faelle_covid_aktuell_beatmet   ~ daten_stand, icu))
#' icuCovBett <- as.data.frame(xtabs( betten_belegt   ~ daten_stand, icu))
#' plot(icuCov$daten_stand, icuCov$Freq, type = "p", xlab = "Tag", ylab="COVID ", main= "COVID-Fälle in Behandlung im KHaus")
#' plot(icuCovBeatm$daten_stand, icuCovBeatm$Freq, type = "p", xlab = "Tag", ylab="Patienten", main="Beatmete COVID-19-Pat. nur invasive Beatmung und ECMO")
#' plot(icuCovBett$daten_stand, icuCovBett$Freq, type = "l", xlab = "Tag", ylab="ICU Betten belegt")
#' }
#' Oder nur die Betten des OBK:
#' \code{
#' icu20200915Obk <- icu[icu$gemeindeschluessel==05374, ]
#' }
#' 
#'
#'
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
#'   \item{daten_stand}{Date, format: "2020-05-01" "2020-05-01" "2020-05-01" "2020-05-01" ... "2020-08-21"}
#' }
#'
###################################################################################
"icudata"
