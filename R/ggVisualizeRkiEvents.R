#' @title  ggVisualizeRkiEvents Visualisation of the pre-processed RKI Data
#'
#' @description ggplot RKI data as result from \code{getRkiData(babsim.hospital::rkidata)}
#'
#' @seealso \code{\link{getRkiData}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_color_gradient2
#'
#' @param data rki data as preprocessed by \code{\link{getRkiData}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{'2020-05-01'}
#'
#' @examples
#' # use 10000 data points only:
#' data <- getRkiData(babsim.hospital::rkidata[1:10000, ])
#' p <- ggVisualizeRkiEvents(data = data, region = 0, StartDate = "2020-10-01")
#' @export

ggVisualizeRkiEvents <- function(data = getRkiData(babsim.hospital::rkidata), region = 5374,
                                 StartDate = "2020-10-01") {
  Day <- Freq <- Age <- NULL
  region <- as.integer(region)
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
  data$Age <- apply(as.matrix(data$Altersgruppe), 1, FUN = mapAgeGroupToAge)
  dataAgg$Age <- as.integer(xtabs(Age ~ Day, data))
  dataAgg$Age <- dataAgg$Age / dataAgg$Freq
  p <- ggplot(data = dataAgg, aes(x = Day, y = Freq, color = Age)) +
    geom_point() +
    geom_line()
  mid <- mean(dataAgg$Age)
  print(mid)
  p + scale_color_gradient2(
    midpoint = mid, low = "blue", mid = "gray", high = "red",
    space = "Lab"
  )
}
