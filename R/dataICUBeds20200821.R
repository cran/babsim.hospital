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
#'   \item{Day}{Date, format: "2020-05-01" "2020-05-02" "2020-05-03" "2020-05-04" ...}
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
#' lines(x$Day, x$intensiveBedVentilation, type = "o", col="red")
#'  
#'    
###################################################################################
"dataICUBeds20200821"
