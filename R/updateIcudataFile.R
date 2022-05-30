#' @title  updateIcudataFile Update ICU data File
#'
#' @description Update icudata (divi)
#'
#' @param overwrite logical Overwrite existing file. Default \code{TRUE}.
#'
#' @importFrom utils read.csv
#'
#' @return True if new data was downloaded, otherwise false
#'
#'
#' @export
updateIcudataFile <- function(overwrite = TRUE){
    # Testing:
    # fileName = "https://www.divi.de/joomlatools-files/docman-files/divi-intensivregister-tagesreports-csv/DIVI-Intensivregister_2020-10-25_12-15.csv"
    # oldData = babsim.hospital::icudataFull
    # overwrite = FALSE
    
    load("babsim.hospital/data/icudataFull.rda")
    oldData <- icudataFull
    lastDay <- as.Date(max(oldData$daten_stand))
    nextDay <- lastDay + 1
    today <-  Sys.Date()
    missingDays <- as.integer(today- lastDay)
    dataWasUpdated <- FALSE
    if (missingDays > 0){
        for (i in 1:missingDays){
            tryCatch({
                print(paste("Trying DIVI update for:", nextDay))
                fileName <- 
                    paste0("https://www.divi.de/joomlatools-files/docman-files/divi-intensivregister-tagesreports-csv/DIVI-Intensivregister_",
                           as.character(nextDay),
                           "_12-15.csv")
                X <- read.csv(url(fileName),
                              header=TRUE,
                              encoding="UTF-8")
                names(X)[names(X) == "faelle_covid_aktuell_invasiv_beatmet"] <- "faelle_covid_aktuell_beatmet"
                newNames <- names(X)[!(names(X) %in% names(oldData))]
                if(length(newNames) > 0){
                    for(n in newNames){
                        oldData[[n]] <- NA
                    }
                }
                dataWasUpdated <- TRUE
                icudataFull <- rbind(oldData,X)
                oldData <- icudataFull
                icudataFull <- unique(icudataFull)
                save(icudataFull, file = "babsim.hospital/data/icudataFull.rda", compress="xz")
                
                icudata <- icudataFull[which(icudataFull$daten_stand >= as.Date("2020-09-01")), ]
                
                save(icudata, file = "babsim.hospital/data/icudata.rda", compress="xz")
                nextDay <- nextDay + 1 
            },error=function(e){
                return(FALSE)
            })
        }
    }
    return(dataWasUpdated)
}

