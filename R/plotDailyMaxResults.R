#' @title plotDailyMaxResults
#'
#' @description  Plot output from \code{getDailyMax()}.
#'
#' @param results Results from \code{getDailyMax}.
#' @param labels  Axes labels (vector). Default: \code{c('babsim', 'DIVI')}
#' @param title Title. Default:
#' \code{'Betten: Tuerkis = Realdaten, Rot = Simulation'}
#' @param showBeds should normal beds be shown in the plot?
#' @param icuDataRegion regional \code{icudata}
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 vars
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labeller
#' @importFrom ggplot2 as_labeller
#' @importFrom plyr ddply
#' @importFrom plyr numcolwise
#'
#'
#' @return This function returns a \code{ggplot} object.
#' @examples
#' set.seed(123)
#' # 1. Generate simulation data based on number of infected persons per day:
#' x <- dataCovidBeds20200624
#' StartDate <- x$Day[1]
#' EndDate <- x$Day[length(x$Day)]
#' arrivalTimes <- getArrivalTimes(x$Infected)
#' para <- babsimHospitalPara()
#' conf <- babsimToolsConf()
#' y <- babsimHospital(
#'   arrivalTimes = arrivalTimes,
#'   conf = conf,
#'   para = para
#' )
#'
#' # 2. Extract real data:
#' fieldEvents <- getRealBeds(
#'   data = babsim.hospital::dataCovidBeds20200624,
#'   resource = c("bed", "intensiveBed", "intensiveBedVentilation")
#' )
#' conf <- babsimToolsConf()
#' # 3. Combine simlated and real data:
#' res <- getDailyMaxResults(
#'   envs = y,
#'   fieldEvents = fieldEvents,
#'   conf = conf
#' )
#' # 4. Plot results
#' p <- plotDailyMaxResults(res)
#' # print(p)
#' @export

plotDailyMaxResults <- function(results, labels = c("babsim", "DIVI"), title = "Betten: Tuerkis = Readfssldaten, Rot = Simulation",
                                showBeds = FALSE, icuDataRegion = NULL) {
  ## the following variables are local:
  med <- lower <- upper <- resource <- NULL
  if (!showBeds) {
    results <- results[results$resource != "bed", ]
  }
  if (!is.null(icuDataRegion)) {
    icuDataRegion$bedsTotal <- icuDataRegion$faelle_covid_aktuell + icuDataRegion$betten_frei
    results <- as.data.frame(results)
    resultsTotal <- ddply(results[, which(!(colnames(results) == "resource"))], c("source", "date"), numcolwise(sum))
    resultsTotal$resource <- "bedsTotal"

    dfDiviAll <- data.frame(
      resource = "bedsTotal", source = "GesamteBettenDIVI",
      date = icuDataRegion$daten_stand, upper = icuDataRegion$bedsTotal, lower = icuDataRegion$bedsTotal,
      med = icuDataRegion$bedsTotal
    )
    dfDiviAll <- ddply(dfDiviAll, c("resource", "source", "date"), numcolwise(sum))
    results <- rbind(results, resultsTotal, dfDiviAll)

    results$resource <- factor(results$resource, levels = c(
      "bed", "intensiveBed",
      "intensiveBedVentilation", "bedsTotal"
    ))


    dfDiviAll.extended <- dfDiviAll[which.max(dfDiviAll$date), ]
    dfDiviAll.extended <- rbind(dfDiviAll.extended, dfDiviAll.extended)
    dfDiviAll.extended[2, ]$date <- max(results$date)
  }

  translator <- golem::get_golem_options("translator")
  if (!is.null(translator)) {
    to_string <- as_labeller(c(
      bed = translator$t("label1"), intensiveBed = translator$t("label2"),
      intensiveBedVentilation = translator$t("label3"), bedsTotal = translator$t("bettenGesamt")
    ))
  } else {
    to_string <- as_labeller(c(
      bed = "Bett", intensiveBed = "Nicht beatmet",
      intensiveBedVentilation = "Beatmet", bedsTotal = "Intensiv Gesamt"
    ))
  }

  if (!is.null(translator)) {
    plotTitle <- translator$t("titlePlotBeds")
    plotXlab <- translator$t("Datum")
    plotYlab <- translator$t("bedsUsed")
    plotLegendTitle <- translator$t("Legende")
    results$source <- translator$t(as.character(results$source))
    if (!is.null(icuDataRegion)) {
      dfDiviAll.extended$source <- translator$t(as.character(dfDiviAll.extended$source))
    }
  } else {
    plotTitle <- "Bettenauslastung: Gemeldet DIVI und Simulation"
    plotXlab <- "Datum"
    plotYlab <- "Belegte Intensiv Betten"
    plotLegendTitle <- "Legende"
  }

  if (!is.null(icuDataRegion)) {
    results$source <- factor(results$source, levels = c(
      unique(results$source)[2],
      unique(results$source)[3], unique(results$source)[1]
    ))
    p <- ggplot(results, aes(x = date, y = med, color = source)) +
      geom_line() +
      geom_line(
        data = dfDiviAll.extended, aes(x = date, y = med, color = source),
        linetype = "dashed"
      ) +
      scale_colour_manual(values = c(
        "black", "red",
        "blue"
      )) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4) +
      facet_grid(
        facets = vars(resource), labeller = labeller(resource = to_string),
        scales = "free"
      ) +
      ggplot2::xlab(plotXlab) +
      ggplot2::ylab(plotYlab) +
      ggplot2::labs(color = plotLegendTitle, title = plotTitle) +
      ggplot2::ggtitle(plotTitle)
  } else {
    p <- ggplot(results, aes(x = date, y = med, color = source)) +
      geom_line() +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4) +
      facet_grid(
        facets = vars(resource),
        labeller = labeller(resource = to_string), scales = "free"
      ) +
      ggplot2::xlab(plotXlab) +
      ggplot2::ylab(plotYlab) +
      ggplot2::labs(color = plotLegendTitle, title = plotTitle) +
      ggplot2::ggtitle(plotTitle)
  }
  return(p)
}
