#' @title getError
#'
#' @description Determine error from babsim runs.
#' This error is the sum of the RMSE values for bed, intensiveBed, and intensiveBedVentilation.
#'
#' @param res Results from \code{getDailyMaxResults}.
#' @param conf configuration
#'
#' @importFrom  dplyr filter
#' @importFrom  dplyr left_join
#' @importFrom  stats complete.cases
#'
#' @return This function returns a num value, that represents the combined rmse from the beds.
#'
#' @examples
#' para <- babsimHospitalPara()
#' conf <- babsimToolsConf()
#' data <- getObkData()
#' set.seed(conf$seed)
#' para <- checkSimPara(para)
#' arrivalTimes <- getArrivalTimes(data$simData$Infected)
#' envs <- babsimHospital(
#'   arrivalTimes = arrivalTimes,
#'   conf = conf,
#'   para = para
#' )
#' fieldEvents <- getRealBeds(
#'   data = data$fieldData,
#'   resource = conf$ResourceNames
#' )
#' res <- getDailyMaxResults(
#'   envs = envs,
#'   fieldEvents = fieldEvents,
#'   conf = conf
#' )
#' err <- getError(res, conf = conf)
#' @export

getError <- function(res, conf) {
  if (conf$verbosity > 100) {
    messagef("BEGIN: getEror: ###########################")
    printConf(conf)
    messagef("END: getError: ###########################")
  }
  rmseBed <- 0
  ### 20201014: resource <- unique(res$resource)
  resource <- conf$ResourceEval
  ## experimental: weighting factor 10 for ventilation:
  w <- rep(1, length(resource))
  w[2] <- conf$w2[2]
  i <- 1
  for (r in resource) {
    res1 <- res %>% filter(resource == r & source == "babsim")
    df1 <- unique(data.frame(date = res1$date, x = res1$med))
    df1 <- df1[order(df1$date), ]
    res2 <- res %>% filter(resource == r & source == "GA")
    df2 <- unique(data.frame(date = res2$date, x = res2$med))
    df2 <- df2[order(df2$date), ]
    fillDate <- which(!(df2$date %in% df1$date))
    if (length(fillDate) > 0) {
      df1 <- rbind(df1, data.frame(date = df2$date[fillDate], x = 0))
    }
    dfBed <- dplyr::left_join(df1, df2, by = c("date"))
    # dfBed[is.na(dfBed)] <- 0
    dfBed <- dfBed[complete.cases(dfBed), ]
    dfBed <- dfBed[order(dfBed$date), ]
    if (conf$verbosity > 100) {
      messagef("BEGIN: getEror: dfBed ##########################")
      printConf(dfBed)
      print(summary(dfBed$date))
      messagef("END: getError: dfBed ###########################")
    }
    # browser()
    rmseBed <- rmseBed + w[i] * weighted_rmse(dfBed[, 3], dfBed[, 2])
    i <- i + 1
  }
  return(rmseBed)
}
