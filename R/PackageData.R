#' @title dataCovidBeds20200624
#'
#' @description
#' A data set of COVID-19 cases with 99 obs. of  11 variables.
#'
#' @format A data frame with 32239 rows and 6 columns:
#' \describe{
#'   \item{bed}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{intensiveBed}{int  0 0 0 0 0 0 0 0 0 0 ...}
#'   \item{intensiveBedVentilation}{int  0 0 0 0 0 0 0 0 0 0 ...}
#'   \item{Day}{Date, format: '2020-03-03' '2020-03-04' '2020-03-05' '2020-03-06' ...}
#'   \item{Infected}{num  5 0 0 0 0 0 0 7 2 5 ...}
#'   \item{Sick}{num  5 5 5 5 5 5 5 12 14 19 ...}
#' }
#' The variable \code{obk$Sick} was generated from the Infected data as follows:
#' \code{slide_dbl(obk$Infected, ~sum(.x), .before = (amntDaysSickness -1)) }
#' \code{amntDaysSickness} was set to 20.
#'
#'
#' @examples
#' x <- dataCovidBeds20200624
#' # first look
#' str(x)
#'
#' # plot
#' x$InfCum <- cumsum(x$Infected)
#' plot(x$Day, x$InfCum, type = "l", log = "y", ylim = c(1, 500))
#' lines(x$Day, x$Infected + 1e-6)
"dataCovidBeds20200624"

#' @title dataICUBeds20200821
#'
#' @description
#' A data set of COVID-19 ICU beds with 113 obs. of  3 variables.
#'
#'
#' @format A data frame with the following entries:
#' \describe{
#'   \item{bed}{640 597 538 553 591 573 593 527 529 508 ...}
#'   \item{intensiveBedVentilation}{int  1549 1508 1441 1396 1346 1311 1230 1185 1121 1073 ...}
#'   \item{Day}{Date, format: '2020-05-01' '2020-05-02' '2020-05-03' '2020-05-04' ...}
#' }
#' The data frame was generated as follows:
#' \code{icu <- icudata
#' icuCov <- as.data.frame(xtabs( faelle_covid_aktuell ~  daten_stand, icu))
#' icuCov$daten_stand <- as.Date(icuCov$daten_stand)
#' icuCovBeatm <- as.data.frame(xtabs(faelle_covid_aktuell_beatmet ~ daten_stand, icu))
#' icuCovBeatm$daten_stand <- as.Date(icuCovBeatm$daten_stand)
#' dataICUBeds20200821 <- data.frame(bed=(icuCov$Freq - icuCovBeatm$Freq), intensiveBedVentilation=icuCovBeatm$Freq, Day =  as.Date(icuCovBeatm$daten_stand))
#' }
#'
#'
#' @examples
#' x <- dataICUBeds20200821
#' # first look
#' str(x)
#'
#' # plot
#' x <- dataICUBeds20200821
#' plot(x$Day, x$bed, type = "o")
#' lines(x$Day, x$intensiveBedVentilation, type = "o", col = "red")
"dataICUBeds20200821"


#' @title koelnarchive archived koeln data
#'
#' @description
#' Data: koeln data generated with SPOT.
#'
#' @details
#' Result from the \code{\link{runoptDirect}} run.
#'
#' @format 'data.frame':  obs. of  28 variables:
#' \describe{
#'   \item{y}{num  311 158 180 232 297 ...}
#'   \item{x.1}{num  10.55 17.91 3.19 9.5 17.71 ...}
#'   \item{...}{ ...}
#'   \item{x.33}{num  1.072 1.015 1.044 1.057 0.556 ...}
#' }
#'
"koelnarchive"

#' @title nrwarchive archived nrw data
#'
#' @description
#' Data: nrw data generated with SPOT.
#'
#' @details
#' Result from the \code{\link{runoptDirect}} run.
#'
#' @format 'data.frame':  obs. of  28 variables:
#' \describe{
#'   \item{y}{num  311 158 180 232 297 ...}
#'   \item{x.1}{num  10.55 17.91 3.19 9.5 17.71 ...}
#'   \item{...}{ ...}
#'   \item{x.33}{num  1.072 1.015 1.044 1.057 0.556 ...}
#' }
"nrwarchive"

#' @title obkarchive archived obk data
#'
#' @description
#' Data: OBK data generated with SPOT.
#'
#' @details
#' Result from the \code{\link{runoptDirect}} run.
#' to extract the best parameter set x.
#'
#' @format 'data.frame':  obs. of  28 variables:
#' \describe{
#'   \item{y}{num  311 158 180 232 297 ...}
#'   \item{x.1}{num  10.55 17.91 3.19 9.5 17.71 ...}
#'   \item{...}{ ...}
#'   \item{x.33}{num  1.072 1.015 1.044 1.057 0.556 ...}
#' }
"obkarchive"

#' @title ex1InfectedDf
#'
#' @description
#' Data used in example 1 and for testing
#' A synthetic data set of COVID-19 cases with 99 obs. of  8 variables:
#'
#' @format A data frame with 99 obs. of  8 variables:
#' \describe{
#'   \item{index}{int  1 2 3 4 5 6 7 8 9 10 ...}
#'   \item{Day}{Date, format: '2020-03-03' '2020-03-04' '2020-03-05' '2020-03-06' ...}
#'   \item{Infected}{num  6 0 1 3 3 1 5 1 6 104 ...}
#'   \item{Sick}{num  6 6 7 10 13 14 19 20 26 130 ...}
#'   \item{InfectedCum}{num  6 6 7 10 13 14 19 20 26 130 ...}
#'   \item{normalStation}{num  1 1 1 1 2 2 3 3 4 18 ...}
#'   \item{intensive}{num  0 0 0 0 0 0 0 0 0 1 ...}
#'   \item{ventilation}{num  0 0 0 0 0 0 0 0 0 2 ...}
#' }
#'
#' @examples
#' x <- ex1InfectedDf
#' # first look
#' str(x)
#'
#' # plot
#' x$InfCum <- cumsum(x$Infected)
#' plot(x$Day, x$InfCum, type = "l", log = "y", ylim = c(1, 500))
#' lines(x$Day, x$Infected + 1e-6)
"ex1InfectedDf"

#' @title synthpara data
#'
#' @description
#' Data: Synthetic data generated with SPOT.
#'
#' @details
#' Result from the \code{\link{runoptDirect}} run.
#' Use \code{yx <- synthpara[synthpara$y == min(synthpara$y), ]
#' x <- yx[1,2:34]
#' }
#' to extract the best parameter set x.
#'
#' @format 'data.frame':  obs. of  34 variables:
#' \describe{
#'   \item{y}{num  311 158 180 232 297 ...}
#'   \item{x.1}{num  10.55 17.91 3.19 9.5 17.71 ...}
#'   \item{...}{ ...}
#'   \item{x.33}{num  1.072 1.015 1.044 1.057 0.556 ...}
#' }
#'
"synthpara"

#' @title paras data
#'
#' @description
#' Data: Parameters for all regions, generated through SPOT optimization
#'
#' @details
#' Result from the \code{\link{runoptDirect}} run.
#'
#' @format 'data.frame' obs. of  31 variables:
#' \describe{
#'   \item{y}{num  311 158 180 232 297 ...}
#'   \item{x.1}{num  10.55 17.91 3.19 9.5 17.71 ...}
#'   \item{...}{ ...}
#'   \item{x.29}{num  1.072 1.015 1.044 1.057 0.556 ...}
#'   \item{region}{num 5315}
#' }
#'
"paras"

#' List of German states
#' 
#' A dataset containing all 16 German states. 
#'
#' @format A \code{\link[data.table]{data.table}} with 16 rows an 2 variables:
#' \describe{
#'   \item{\code{stateId}}{unique id, first two digits of the German AGS.}
#'   \item{\code{state}}{name of state in German.}
#' }
#' 
#' @seealso \code{\link{GermanCounties}} for German counties.
#'
#' @source Gemeindeverzeichnis-Informationssystem (GV-ISys) of the German Federal Statistics Office 
#'
#' @examples
#' head(GermanStates)
#'
"GermanStates"

#' List of German administrative counties
#' 
#' A dataset containing the names and AGS of all 401 German counties.
#'
#' @format A \code{\link[data.table]{data.table}} with 401 rows and 3 variables:
#' \describe{
#'   \item{\code{stateId}}{id of state containing this county.}
#'   \item{\code{countyId}}{unique id, first five digits of the German AGS.}
#'   \item{\code{county}}{name of county in German.}
#' }
#' 
#' @seealso \code{\link{GermanStates}} for German states.
#'
#' @source Gemeindeverzeichnis-Informationssystem (GV-ISys) of the German Federal Statistics Office  
#'
#' @examples
#' x <- merge(GermanCounties, GermanStates, by="stateId")
#' subset(x, countyId == "05135") # Cologne, North Rhine-Westphalia
#'
"GermanCounties"

#' @title Amount of vaccinated people in germany
#'
#' @description
#' Data: Amount of vaccinated people in germany. Collected by day and state.
#' Additionally the vaccine-quota (vaccinated/capita) is stored
#'
#' @format 'data.frame':  obs. of  4 variables:
#' \describe{
#'   \item{vaccinated}{num  311 158 180 232 297 ...}
#'   \item{quote}{num  10.55 17.91 3.19 9.5 17.71 ...}
#'   \item{state}{char Berlin Bayern ...}
#'   \item{date}{date  2020-12-29 2020-12-30 ...}
#' }
#'
"vaccineCounts"