#' @title getObkData
#'
#' @description Generate simData and fieldData from OBK data
#'
#' @param data OBK data. Default: \code{\link{dataCovidBeds20200624}}
#'
#' @return list with simData and fieldData
#'
#' @examples
#'
#' data <- getObkData()
#' @export


getObkData <- function(data = babsim.hospital::dataCovidBeds20200624) {
  simData <- data.frame(Day = data$Day, Infected = data$Infected)
  attr(simData, "StartDate") <- min(simData$Day)
  attr(simData, "EndDate") <- max(simData$Day)
  attr(simData, "Days") <- as.integer(1 + max(simData$Day) - min(simData$Day))
  attr(simData, "FeatureNames") <- c("Infected")
  fieldData <- data.frame(
    Day = data$Day, bed = data$bed, intensiveBed = data$intensiveBed,
    intensiveBedVentilation = data$intensiveBedVentilation
  )
  attr(fieldData, "StartDate") <- min(fieldData$Day)
  attr(fieldData, "EndDate") <- max(fieldData$Day)
  attr(fieldData, "Days") <- as.integer(1 + max(fieldData$Day) - min(fieldData$Day))
  attr(fieldData, "ResourceNames") <- c("bed", "intensiveBed", "intensiveBedVentilation")

  return(list(simData = simData, fieldData = fieldData))
}
