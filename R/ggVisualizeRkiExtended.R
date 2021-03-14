#' @title  ggVisualizeRkiExtended Visualisation of the extended RKI Data
#'
#' @description ggplot RKI data as result from \code{\link{extendRki}}
#'
#' @seealso \code{\link{rkidata}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 ggtitle
#'
#'
#' @param data rki data as preprocessed by \code{\link{extendRki}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{'2020-05-01'}
#' @param simplify logical. Simplify presentation, Default: TRUE.
#' @param preloadedData optional way to pass the result of preloading.
#'
#' @export

ggVisualizeRkiExtended <- function(data = extendRki(), region = 5374, StartDate = "2020-10-01",
                                   simplify = TRUE, preloadedData = NULL) {
  Day <- value <- variable <- NULL
  if (is.null(preloadedData)) {
    preloadedData <- ggVisualizeRkiExtendedDataCalculation(
      data, region, StartDate,
      simplify
    )
  }
  xymelt <- preloadedData

  translator <- golem::get_golem_options("translator")
  if (!is.null(translator)) {
    plotTitle <- translator$t("titlePlotInfections")
    plotXlab <- translator$t("Datum")
    plotYlab <- translator$t("infPerDay")
    plotLegendTitle <- translator$t("Legende")
    xymelt$variable <- translator$t(as.character(xymelt$variable))
  } else {
    plotTitle <- "Konfigurierte Infektionszahlen 7-Tage Durchschnitt"
    plotXlab <- "Datum"
    plotYlab <- "Infektionen pro Tag"
    plotLegendTitle <- "Legende"
  }

  xymelt.extended <- xymelt[xymelt$Day >= Sys.Date(), ]
  xymelt <- xymelt[xymelt$Day <= Sys.Date(), ]

  ggplot(xymelt, aes(x = Day, y = value, color = variable)) +
    geom_line() +
    geom_line(
      data = xymelt.extended,
      aes(x = Day, y = value, color = variable), linetype = "dashed"
    ) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::xlab(plotXlab) +
    ggplot2::ylab(plotYlab) +
    ggplot2::labs(color = plotLegendTitle)
}
