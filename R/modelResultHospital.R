#' @title modelResultHospital
#'
#' @description Simulate one parameter configuration from \code{\link{babsimHospitalPara}}. The simulation is by default
#' deterministic, because \code{conf$seed} is used for \code{\link[base]{set.seed}}.
#'
#' @param para Simulation model parameters. The function \code{\link{babsimHospitalPara}}
#' can be used to generate these parameters.
#' @param conf list with the following entries:
#'       \describe{
#'       \item{\code{seed}}{seed. Change the \code{seed} value to get different output for the same
#'       input parameters. Default: 123}
#'       \item{\code{simRepeats}}{simmer repeats}
#'       \item{\code{parallel}}{simmer parallel runs. Default: FALSE}
#'       \item{\code{perCores}}{percentage of cores used for parallel simmer simulations. Default: 0.5 (=50 percent)}
#'       \item{\code{ICU}}{use ICU infection data. Default: FALSE}
#'       \item{\code{logLevel}}{log leved (0 or 1). Default: 0 (no output)}
#'       \item{\code{maxCapacity}}{max capacity of resources. Default: 1e6}
#'       \item{\code{dataset}}{char name of the data set. Default: 'GA'}
#'       \item{\code{simulationDates}}{list with \code{StartDate} and \code{EndDate}.
#'       Period that is used for the simulation (babsim, simmer). Default:
#'       \code{list(StartDate = '2020-03-03', EndDate = '2020-06-24')}}
#'       \item{\code{fieldDates}}{list with \code{StartDate} and \code{EndDate}.
#'       Period when real data is available (resource usage). Default:
#'       \code{list(StartDate = '2020-03-03', EndDate = '2020-06-24')}}
#'       \item{\code{simulationData}}{data frame. Data used for the simulation. Default:
#'       \code{\link{dataCovidBeds20200624}}}
#'       \item{\code{fieldEvents}}{data frame. Data used for the evaluation (error). Default:
#'       \code{\link{GABeds220200624}}}
#'       \item{\code{resource}}{vector with resource names.
#'       Default: c('bed', 'intensiveBed', 'intensiveBedVentilation')}
#'       }
#' @param data list with simData and fieldData
#'
#' @importFrom stats xtabs
#'
#' @return This function returns an env data frame (tibble [560 × 15] (S3: grouped_df/tbl_df/tbl/data.frame))
#' with the following entries:
#' \describe{
#'     \item{\code{resource} (chr)}{name of the seized resource: 'bed' 'bed' 'bed' 'bed' ...}
#'     \item{\code{time} (num)}{time step:  3 10 12 13 14 15 15 15 15 16 ...}
#'     \item{\code{server} (int)}{server: 1 2 3 2 3 4 3 4 5 6 ...}
#'     \item{\code{queue} (int)}{1 1 2 3 4 3 4 1 0 2 ...}
#'     \item{\code{capacity} (num)}{10000 10000 10000 10000 10000 10000 10000 10000 10000 10000 ...}
#'     \item{\code{queue_size} (num)}{Inf Inf Inf Inf Inf ...}
#'     \item{\code{system} (int)}{1 1 2 3 4 3 4 1 0 2 ...}
#'     \item{\code{limit} (num)}{limit: Inf Inf Inf Inf Inf ...}
#'     \item{\code{replication} (int)}{replication:  1 1 1 1 1 1 1 1 1 1 ...}
#'     \item{\code{upper} (int)}{upper: 1 2 3 2 3 5 5 5 5 7 ...}
#'     \item{\code{lower} (int)}{lower: 1 2 3 2 3 3 3 3 3 5 ...}
#'     \item{\code{med} (num)}{med: 1 2 3 2 3 4 4 4 4 6 ...}
#'     \item{\code{date} (POSIXct)}{time, format: \code{yyyy-mm-dd hh:mm.ss}}
#'     \item{\code{rwdate} (POSIXct)}{format: '2020-03-01' '2020-03-08' '2020-03-15' '2020-03-15' ...}
#'     \item{\code{source} (chr)}{name of the simulation that was used: 'babsim' 'babsim' 'babsim' 'babsim' ...}
#'     }
#' @examples
#' # First example: OBK data
#' data <- getObkData()
#' para <- babsimHospitalPara()
#' para$GammaShapeParameter <- 0.8
#' # turn off parallelized simulation:
#' conf <- babsimToolsConf()
#' conf <- getConfFromData(
#'   conf = conf,
#'   simData = data$simData,
#'   fieldData = data$fieldData
#' )
#' # no logging (default)
#' conf$logLevel <- 0
#' res <- modelResultHospital(
#'   para = para,
#'   conf = conf,
#'   data = data
#' )
#'
#' # Second example: synthetic data
#' data <- getSyntheticData()
#' para <- babsimHospitalPara()
#' conf <- babsimToolsConf()
#' conf <- getConfFromData(
#'   conf = conf,
#'   simData = data$simData,
#'   fieldData = data$fieldData
#' )
#' res <- modelResultHospital(para = para, conf = conf, data = data)
#' @export

modelResultHospital <- function(para, conf, data) {
  set.seed(conf$seed)
  para <- checkSimPara(para)
  if (conf$ICU) {
    rkiWithRisk <- getRkiRisk(data$simData, para)
    arrivalTimes <- data.frame(time = rkiWithRisk$time, risk = rkiWithRisk$Risk)
  } else {
    arrivalTimes <- getArrivalTimes(data$simData$Infected)
  }
  con <- list(arrivalTimes = arrivalTimes)
  con[names(data)] <- data
  data <- con

  if (conf$verbosity > 1000) {
    messagef("BEGIN: modelResultHospital() calling babsimHospital: ###########################")
    printConf(conf)
    messagef("END: modelResultHospital()  ###########################")
  }

  envs <- babsimHospital(arrivalTimes = data$arrivalTimes, conf = conf, para = para)
  fieldEvents <- getRealBeds(data = data$fieldData, resource = conf$ResourceNames)
  return(getDailyMaxResults(envs = envs, fieldEvents = fieldEvents, conf = conf))
}
