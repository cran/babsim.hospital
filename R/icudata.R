
###################################################################################
#' @title  icudata IntensivbettenDaten (Beispieldatensatz)
#' 
#' @description ICU Beispiel-Datensatz (nur für Demonstrationszwecke).
#' Der Beispieldatensatz \code{icudata} dient nur zu Demonstrationszwecken.
#' Er besitzt das gleiche Format wie Tagesreports des DIVI Intensivregisters, siehe
#' \code{https://www.divi.de/register/tagesreport}. 
#' Ziel des DIVI-Intensivregisters ist, 
#' die Verfügbarkeiten von Beatmungsbetten und von erweiterten Therapiemaßnahmen 
#' bei akutem Lungenversagen in Deutschland sichtbar zu machen. 
#' Eine weitere wissenschaftliche Nutzung der Daten ist nur mit Zustimmung der DIVI gestattet. 
#' Bitte kontaktieren Sie die wissenschaftliche Leitung des DIVI-Intensivregisters. 
#' Please contact DIVI e.V. if you are interested in the full data:
#' \code{https://www.divi.de/register/anprechpartner-register}
#' 
#' @details A sample of the ICU data
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
