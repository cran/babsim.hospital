#' @title  ggVisualizeRki ggPlot Visualisierung der RKI Daten
#'
#' @description Quelle: RKI Daten bundesweit
#'
#' @seealso \code{\link{rkidata}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom ggplot2 scale_fill_brewer
#'
#' @param data rki data, e.g., \code{\link{rkidata}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{'2020-05-01'}
#'
#'
#' @export

ggVisualizeRki <- function(data = babsim.hospital::rkidata, region = 5374, StartDate = "2020-05-01") {
  Freq <- Refdatum <- weekly <- NULL
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
  dataAgg$weekly <- slide_dbl(dataAgg$Freq, ~ mean(.x), .before = (7 - 1))
  maxCaseNumberdataAgg <- max(dataAgg$Freq)

  translator <- golem::get_golem_options("translator")
  if (!is.null(translator)) {
    xlabT <- translator$t("Datum")
    ylabT <- translator$t("infPerDay")
    plotLegendTitle <- translator$t("Legende")
    legDaily <- translator$t("legDaily")
    legWeek <- translator$t("legWeek")
  } else {
    xlabT <- "Datum"
    ylabT <- "Infektionen"
    plotLegendTitle <- "Legende"
    legDaily <- "Tagesmeldung"
    legWeek <- "7-Tage Durchschnitt"
  }

  rki <- ggplot() +
    geom_point(aes(x = Refdatum, y = Freq, col = legDaily),
      data = dataAgg,
      size = 0.5
    ) +
    geom_line(aes(x = Refdatum, y = weekly, col = legWeek), data = dataAgg) +
    scale_colour_manual(values = c("red", "black")) +
    ggplot2::labs(
      color = plotLegendTitle,
      title = "Robert-Koch Institut (RKI)"
    ) +
    xlab(xlabT) +
    ylab(ylabT) +
    expand_limits(y = c(
      0,
      maxCaseNumberdataAgg
    )) +
    theme(plot.title = element_text(hjust = 0.5))
}
