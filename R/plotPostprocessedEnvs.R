#' @title plotPostprocessedEnvs
#'
#' @description  Plot output from \code{\link{postprocessEnvs}}.
#'
#' @param results Results from \code{\link{postprocessEnvs}}.
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 vars
#' @importFrom ggplot2 labs
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
#' # 2. Postprocess simmer environment:
#' res <- postprocessEnvs(envs = y, StartDate = "2020-03-03")
#' # 4. Plot results
#' p <- plotPostprocessedEnvs(res)
#' # print(p)
#' @export

plotPostprocessedEnvs <- function(results) {
  ## the following variables are local:
  med <- lower <- upper <- resource <- NULL
  p <- ggplot(results, aes(x = date, y = med, color = source)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4) +
    facet_wrap(facets = vars(resource)) +
    labs(x = "Date", y = "Usage")
  return(p)
}
