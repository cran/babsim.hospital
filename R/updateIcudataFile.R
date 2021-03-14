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
updateIcudataFile <- function(oldData = babsim.hospital::icudataFull, overwrite = TRUE) {
  lastDay <- as.Date(max(oldData$daten_stand))
  nextDay <- lastDay + 1
  today <- Sys.Date()
  missingDays <- as.integer(today - lastDay)
  dataWasUpdated <- FALSE
  if (missingDays > 0) {
    for (i in 1:missingDays) {
      fileName <- paste0(
        "https://www.divi.de/joomlatools-files/docman-files/divi-intensivregister-tagesreports-csv/DIVI-Intensivregister_",
        as.character(nextDay), "_12-15.csv"
      )
      X <- read.csv(url(fileName), header = TRUE, encoding = "UTF-8")
      dataWasUpdated <- TRUE
      icudataFull <- rbind(oldData, X)
      usethis::use_data(name = icudataFull, overwrite = overwrite)

      icudata <- icudataFull[which(icudataFull$daten_stand >= as.Date("2020-09-01")), ]

      usethis::use_data(name = icudata, overwrite = overwrite)
      nextDay <- nextDay + 1
    }
  }
  return(dataWasUpdated)
}
