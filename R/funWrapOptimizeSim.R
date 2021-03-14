#' @title funWrapOptimizeSim
#'
#' @description Wrapper function for \code{funOptimizeSim}
#'
#' @param x num: real values. Will be interpreted as parameter values from \code{babsimHospital}.
#' Names of these parameters can be obtained via \code{\link{getParameterName}}.
#' @param conf list with the following entries:
#'       \describe{
#'       \item{\code{seed}}{seed. Default: 123}
#'       \item{\code{simRepeats}}{simmer repeats}
#'       \item{\code{parallel}}{simmer parallel runs. Default: FALSE}
#'       \item{\code{perCores}}{percentage of cores used for parallel simmer simulations. Default: 0.5 (=50 percent)}
#'       \item{\code{ICU}}{use ICU infection data. Default: FALSE}
#'       \item{\code{logLevel}}{log leved (0 or 1). Default: 0 (no output)}
#'       }
#'       For example: \code{conf <- babsimToolsConf()}
#' @param data list with simData and fieldData, e.g. \code{data <- getObkData()}.
#'
#' @return This function returns a  num [1, 1] matrix,
#' that represents the combined rmse from the three beds.

#' @examples
#' para <- getStartParameter()
#' conf <- babsimToolsConf()
#' data <- getObkData()
#' funWrapOptimizeSim(x = para, conf = conf, data = data)
#' @export
funWrapOptimizeSim <- function(x,
                               conf,
                               data) {
  matrix(apply(
    x, # matrix
    1, # margin (apply over rows)
    funOptimizeSim,
    conf, data
  ))
}
