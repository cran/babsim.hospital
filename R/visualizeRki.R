#' @title  visualizeRki Visualisierung der RKI Daten
#'
#' @description Quelle: RKI Daten bundesweit
#'
#' @seealso \code{\link{rkidata}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom graphics plot
#'
#' @param data rki data, e.g., \code{\link{rkidata}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{'2020-05-01'}
#'
#'
#' @export

visualizeRki <- function(data = babsim.hospital::rkidata, region = 5374, StartDate = "2020-05-01") {
  ## Landkreis
  if (region > 0 & region > 100) {
    data <- data[data$IdLandkreis == region, ]
  }
  ## Bundesland
  if (region > 0 & region < 100) {
    data <- data[data$IdBundesland == region, ]
  }
  data <- data[which(data$Refdatum >= as.Date(StartDate)), ]
  data$Refdatum <- as.Date(data$Refdatum)
  dataAgg <- as.data.frame(xtabs(AnzahlFall ~ Refdatum, data))
  dataAgg$Refdatum <- as.Date(dataAgg$Refdatum)
  plot(dataAgg$Refdatum, dataAgg$Freq,
    type = "b", ylab = "Faelle", xlab = "Refdatum",
    main = paste0("Infizierte. Region: ", region)
  )
  dataAgg$weekly <- slide_dbl(dataAgg$Freq, ~ mean(.x), .before = (7 - 1))
  lines(dataAgg$Refdatum, dataAgg$weekly, col = "red")
}
