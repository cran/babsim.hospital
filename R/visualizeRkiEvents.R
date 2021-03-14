#' @title  visualizeRkiEvents Visualisation of the pre-processed RKI Data
#'
#' @description RKI data as result from \code{getRkiData(babsim.hospital::rkidata)}
#'
#' @seealso \code{\link{getRkiData}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom graphics plot
#'
#' @param data rki data as preprocessed by \code{\link{getRkiData}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{'2020-05-01'}
#'
#' @examples
#'
#' p <- visualizeRkiEvents(getRkiData(babsim.hospital::rkidata[1:1000, ]))
#' @export

visualizeRkiEvents <- function(data = getRkiData(babsim.hospital::rkidata), region = 0,
                               StartDate = "2020-05-01") {
  ## Landkreis
  if (region > 0 & region > 100) {
    data <- data[data$IdLandkreis == region, ]
  }
  ## Bundesland
  if (region > 0 & region < 100) {
    data <- data[data$IdBundesland == region, ]
  }
  data <- data[which(data$Day >= as.Date(StartDate)), ]
  data$w <- rep(1, dim(data)[1])
  dataAgg <- as.data.frame(xtabs(w ~ Day, data))
  dataAgg$Day <- as.Date(dataAgg$Day)
  plot(dataAgg$Day, dataAgg$Freq,
    type = "b", ylab = "COVID-19 Cases", xlab = "Date",
    main = paste0("Infizierte. Region: ", region)
  )
  dataAgg$weekly <- slide_dbl(dataAgg$Freq, ~ mean(.x), .before = (7 - 1))
  lines(dataAgg$Day, dataAgg$weekly, col = "red")
}
