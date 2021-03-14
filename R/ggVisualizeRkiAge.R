#' @title  ggVisualizeRkiAge Visualisation of the pre-processed RKI Data with
#' respect to age and gender
#'
#' @description ggplot RKI data as result from \code{getRkiData(babsim.hospital::rkidata)}
#'
#' @seealso \code{\link{rkiToBabsimData}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme_bw
#'
#' @param data rki data as preprocessed by \code{\link{rkiToBabsimData}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{'2020-05-01'}
#' @param simplify logical. Simplify presentation, Default: TRUE.
#'
#' @export

ggVisualizeRkiAge <- function(data = babsim.hospital::rkidata, region = 5374, StartDate = "2020-10-01",
                              simplify = TRUE) {
  Day <- Freq <- Age <- Infected <- InfectedWeekly <- value <- variable <- NULL
  region <- as.integer(region)
  ## Landkreis
  if (region > 0 & region > 100) {
    data <- data[data$IdLandkreis == region, ]
  }
  ## Bundesland
  if (region > 0 & region < 100) {
    data <- data[data$IdBundesland == region, ]
  }
  data$Refdatum <- as.Date(data$Refdatum)
  data <- rkiToBabsimData(data)
  data <- data[which(as.Date(data$Day) >= as.Date(StartDate)), ]

  if (simplify == TRUE) {
    data$AgeYoung <- data$A00A04 + data$A05A14 + data$A15A34 + data$A35A59
    # data$MittelA59 <- data$A35A59
    data$AgeOld60 <- data$A60A79 + data$A80
    data$A00A04 <- data$A05A14 <- data$A15A34 <- data$A35A59 <- data$A60A79 <- data$A80 <- NULL
  }

  k <- colnames(data)
  k2 <- k[!k %in% c("Day", "GUnbekannt", "AUnbekannt")]
  getWeekly <- function(x) {
    slide_dbl(x, ~ mean(.x), .before = (7 - 1))
  }

  varNames <- c()
  for (i in k2) {
    var1 <- paste0(i, "Weekly")
    var2 <- paste0("data$", i)
    y <- getWeekly(eval(parse(text = var2)))
    data <- data.frame(data, assign(var1, y))
    varNames <- c(varNames, var1)
  }

  colnames(data) <- c(k, varNames)
  weeklyData <- data[, varNames]
  xy <- cbind(Day = data[, c("Day")], weeklyData)
  data.table::setDT(xy)
  xymelt <- data.table::melt(xy, id.vars = "Day")
  ggplot(xymelt, aes(x = Day, y = value, color = variable)) +
    theme_bw() +
    geom_line()
}
