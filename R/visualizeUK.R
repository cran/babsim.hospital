###################################################################################
#' @title  visualizeUK 
#' 
#' @description Anzeigen der UK Daten
#'
#' @param data Date
#' @param region Landkreis Id, e.g., \code{5374} fuer OBK, \code{5315} fuer Koeln,
#' \code{0} fuer Deutschland,
#' oder Bundesland ID, e.g., \code{5} fuer NRW.
#' 
#' @importFrom SPOT spot
#' @importFrom SPOT optimNLOPTR
#' @importFrom SPOT buildKriging
#' @importFrom SPOT selectN
#' 
#' @importFrom graphics par
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' 
#' @examples 
#' 
#' \dontrun{
#' res <- visualizeUK()
#' }
#' 
#' @export
###################################################################################
visualizeUK <- function(data,
                        region = 05315){
  dataCov <- data$fieldData
  par(mfrow=c(3,1))
  plot(dataCov$Day, dataCov$bed, type = "b", xlab = "Day", 
       ylab="Patients", 
       main = paste0("bed: non ICU patients in hospital")
  )
  dataCov$weekly <- slide_dbl(dataCov$bed, ~mean(.x), .before = (7 -1))
  lines(dataCov$Day, dataCov$weekly, col = "red")
  ##
  plot(dataCov$Day, dataCov$intensiveBed, type = "b", xlab = "Day", 
       ylab="Patients", 
       main = paste0("intensiveBed: ICU bed without ventilation")
  )
  dataCov$weekly <- slide_dbl(dataCov$intensiveBed, ~mean(.x), .before = (7 -1))
  lines(dataCov$Day, dataCov$weekly, col = "red")
  ##
  plot(dataCov$Day, dataCov$intensiveBedVentilation, type = "b", xlab = "Day", 
       ylab="Patients", 
       main = paste0("intensiveBedVentilation: ICU bed with ventilation")
  )
  dataCov$weekly <- slide_dbl(dataCov$intensiveBedVentilation, ~mean(.x), .before = (7 -1))
  lines(dataCov$Day, dataCov$weekly, col = "red")
}
# visualizeUK <- function(data,
#                         region = 05315){
#   dataCov <- data$fieldData
#   par(mfrow=c(3,1))
#   plot(dataCov$Day, dataCov$bed, type = "b", xlab = "Tag", 
#        ylab="COVID-Faelle in Behandlung im KHaus", 
#        main = paste0("COVID")
#   )
#   dataCov$weekly <- slide_dbl(dataCov$bed, ~mean(.x), .before = (7 -1))
#   lines(dataCov$Day, dataCov$weekly, col = "red")
#   ##
#   plot(dataCov$Day, dataCov$intensiveBed, type = "b", xlab = "Tag", 
#        ylab="COVID-Faelle Intensiv", 
#        main = paste0("ICU. Region: ", region)
#   )
#   dataCov$weekly <- slide_dbl(dataCov$intensiveBed, ~mean(.x), .before = (7 -1))
#   lines(dataCov$Day, dataCov$weekly, col = "red")
#   ##
#   plot(dataCov$Day, dataCov$intensiveBedVentilation, type = "b", xlab = "Tag", 
#        ylab="COVID-Faelle beatmet", 
#        main = paste0("ICU-Beatmet. Region: ", region)
#   )
#   dataCov$weekly <- slide_dbl(dataCov$intensiveBedVentilation, ~mean(.x), .before = (7 -1))
#   lines(dataCov$Day, dataCov$weekly, col = "red")
#   
  # plot(dataCovBeatm$daten_stand, dataCovBeatm$Freq, type = "b", xlab = "Tag", 
  #      ylab="Beatmete COVID-19-Pat. nur invasive Beatmung und ECMO", 
  #      main = paste0("Beatmete COVID-Intensiv. Region: ", region))
  # dataCovBeatm$weekly <- slide_dbl(dataCovBeatm$Freq, ~mean(.x), .before = (7 -1))
  # lines(dataCov$daten_stand, dataCovBeatm$weekly, col = "red")
  # par(mfrow=c(1,1))
  # plot(dataCovBett$daten_stand, dataCovBett$Freq, type = "b", xlab = "Tag", ylab="ICU Betten belegt")
  # dataCovBett$weekly <- slide_dbl(dataCovBett$Freq, ~mean(.x), .before = (7 -1))
  # lines(dataCov$daten_stand, dataCovBett$weekly, col = "red")
  # plot(dataCov$daten_stand, dataCov$weekly, col = "black", type="l", ylab = "Faelle, im 7 Tage Mittel")
  # lines(dataCov$daten_stand, dataCovBeatm$weekly, col = "red")
  # legend('topright', lty = 1, c("COVID-Faelle in Behandlung im KHaus","Beatmete COVID-19-Pat. nur invasive Beatmung und ECMO"), col = c("black", "red"))
#}