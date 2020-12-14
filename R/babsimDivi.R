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
#'   \item{Day}{Date, format: "2020-05-01" "2020-05-02" "2020-05-03" "2020-05-04" ...}
#'  }
#' 
#' 
#' @examples 
#' 
#' IcuBeds <- getIcuBeds(data = icudata) 
#' 
#' @export
getIcuBeds <- function(data = babsim.hospital::icudata){
 icuCov <- as.data.frame(xtabs( faelle_covid_aktuell ~  daten_stand, data)) 
 icuCov$daten_stand <- as.Date(icuCov$daten_stand)
 icuCovBeatm <- as.data.frame(xtabs(faelle_covid_aktuell_beatmet ~ daten_stand, data)) 
 icuCovBeatm$daten_stand <- as.Date(icuCovBeatm$daten_stand)
 dataIcuBeds <- data.frame(intensiveBed=(icuCov$Freq - icuCovBeatm$Freq), intensiveBedVentilation=icuCovBeatm$Freq, Day =  as.Date(icuCovBeatm$daten_stand))
 return(dataIcuBeds)
}


#' @title  getRegionIcu Auswahl der ICU Daten fuer eine Region
#' 
#' @description Auswahl anhand der Bundeslaender, Landkreis IDs 
#' 
#' @param data Daten, z.B. icudata
#' @param region Id der Region, \code{0} Deutschland
#' 
#' @examples 
#' data <- getRegionIcu(babsim.hospital::icudata, 5315)
#' 
#' @export

getRegionIcu <- function(data = babsim.hospital::icudata,
                      region = 5315) {
  region <- as.integer(region)
    if (region > 0 & region < 100) {
      return (data[data$bundesland == region,])
    } else if (region >= 100) {
      return(data[data$gemeindeschluessel == region,])
    } else {
      return(data)
  }
}


#' @title  updateIcudataFile Update ICU data File
#' 
#' @description Update icudata (divi)
#' 
#' @param oldData Old icu (DIVI) data, default \code{babsim.hospital::icudata}.
#' @param overwrite logical Overwrite existing file. Default \code{TRUE}.
#' 
#' @importFrom utils read.csv
#' 
#' @return True if new data was downloaded, otherwise false
#' 
#' 
#' @export
updateIcudataFile <- function(oldData = babsim.hospital::icudataFull,
                              overwrite = TRUE){
    lastDay <- as.Date(max(oldData$daten_stand))
    nextDay <- lastDay + 1
    today <-  Sys.Date()
    missingDays <- as.integer(today- lastDay)
    dataWasUpdated <- FALSE
    if (missingDays > 0){
        for (i in 1:missingDays){
            fileName <- 
                paste0("https://www.divi.de/joomlatools-files/docman-files/divi-intensivregister-tagesreports-csv/DIVI-Intensivregister_",
                       as.character(nextDay),
                       "_12-15.csv")
            X <- read.csv(url(fileName),
                          header=TRUE,
                          encoding="UTF-8")
            dataWasUpdated <- TRUE
            icudataFull <- rbind(oldData,X)
            usethis::use_data(name = icudataFull, overwrite = overwrite)
            
            icudata <- icudataFull[which(icudataFull$daten_stand >= as.Date("2020-09-01")), ]
            
            usethis::use_data(name = icudata, overwrite = overwrite)
            nextDay <- nextDay + 1 
        }
    }
    return(dataWasUpdated)
}






