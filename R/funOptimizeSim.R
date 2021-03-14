#' @title funOptimizeSim
#'
#' @description Interface function to evaluate one parameter configuration
#' from \code{\link{babsimHospitalPara}}
#'
#' @param x num:  real values. Will be interpreted as parameter values for \code{\link{babsimHospital}}.
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
#' @param data list with simData and fieldData
#' @param ... additional variables
#'
#' @return This function returns a real value, that represents the combined rmse from the three beds
#' types.

#' @examples
#' x <- getStartParameter()
#' conf <- babsimToolsConf()
#' data <- getObkData()
#' err <- funOptimizeSim(x = x, conf = conf, data = data)
#' @export
funOptimizeSim <- function(x, conf, data, ...) {
  para <- mapXToPara(x)
  res <- modelResultHospital(para = para, conf = conf, data = data)
  err <- getError(res = res, conf = conf)
  if (conf$verbosity > 1000){
    print(x)
    str(para)
    str(res)
  }
  if (conf$verbosity > 10) {
    print(paste0("Err:", err))
  }
  return(err)
}
