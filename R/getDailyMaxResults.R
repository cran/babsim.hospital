#' @title getDailyMaxResults
#'
#' @description Combine babsim simulation results with real (field) data. Use daily max resources.
#' Input: \code{\link[simmer]{simmer}} simulation environment and field data formatted
#' using \code{\link{getRealBeds}}. The formatted filed date has dim (nxm, 5), the output data
#' has dimension (nxm, 15).
#' The method \code{\link[simmer]{get_mon_resources}} function is used to extract information from
#' the \code{babsim.hospital} simulation.
#' The function is used by \code{\link{modelResultHospital}} to prepare the calculation of the error.
#'
#' @seealso \code{\link{modelResultHospital}}
#'
#' @details \code{\link[simmer]{get_mon_resources}} returns state changes in resources:
#' \itemize{
#'  \item{'resource': }{resources name}
#'  \item{'time': }{time instant of the event that triggered the state change}
#'  \item{'server': }{server count}
#'  \item{'queue': }{queue count}
#'  \item{'capacity': }{capacity}
#'  \item{'queue_size': }{queue size}
#'  \item{'system': }{system count (server + queue). If no queues are used, system values equal server values.}
#'  \item{'system_limit': }{system limit (capacity + queue_size)}
#'  }
#'
#' @param envs \code{\link[simmer]{simmer}} simulation environment.
#' Result from \code{babsim.hospital} simulation runs, e.g., output from \code{\link{babsimHospital}}.
#' @param fieldEvents Real values. Output from \code{\link{getRealBeds}}, i.e.,
#' a (nxm, 5)-dim data.frame with the following variables:
#' \describe{
#'   \item{\code{resource}}{chr:  'bed' 'bed' 'bed' 'bed' ...}
#'   \item{\code{time}}{int:  1 2 3 4 5 6 7 8 9 10 ...}
#'   \item{\code{med}}{int:   2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{\code{source}}{chr:  'GA' 'GA' 'GA' 'GA' ...}
#'   \item{\code{date}}{ POSIXct, format: '2020-03-03 01:00:00' '2020-03-04 01:00:00' '2020-03-05 01:00:00' '2020-03-06 01:00:00' ...}
#'   }
#' @param conf list with the following entries (generated with \code{\link{babsimToolsConf}}):
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
#'
#' @importFrom dplyr slice
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom lubridate round_date
#' @importFrom simmer get_mon_resources
#' @importFrom stats median
#' @importFrom dplyr bind_rows
#'
#'
#' @return This function returns an env data frame (tibble [nxm, 15] (S3: grouped_df/tbl_df/tbl/data.frame))
#' with the following entries:
#' \describe{
#'     \item{\code{resource} (chr)}{name of the seized resource: 'bed' 'bed' 'bed' 'bed' ...}
#'     \item{\code{time} (num)}{time step:  3 10 12 13 14 15 15 15 15 16 ...}
#'     \item{\code{server} (int)}{server: 1 2 3 2 3 4 3 4 5 6 ...}
#'     \item{\code{limit} (num)}{limit: Inf Inf Inf Inf Inf ...}
#'     \item{\code{replication} (int)}{replication:  1 1 1 1 1 1 1 1 1 1 ...}
#'     \item{\code{upper} (int)}{upper: 1 2 3 2 3 5 5 5 5 7 ...}
#'     \item{\code{lower} (int)}{lower: 1 2 3 2 3 3 3 3 3 5 ...}
#'     \item{\code{med} (num)}{med: 1 2 3 2 3 4 4 4 4 6 ...}
#'     \item{\code{date} (POSIXct)}{time, format: \code{yyyy-mm-dd hh:mm.ss}}
#'     \item{\code{rwdate} (POSIXct)}{format: '2020-03-01' '2020-03-08' '2020-03-15' '2020-03-15' ...}
#'     \item{\code{source} (chr)}{name of the simulation that was used: 'babsim' 'babsim' 'babsim' 'babsim' ...}
#'     }
#'
#' @examples
#' data <- getSyntheticData()
#' para <- babsimHospitalPara()
#' conf <- babsimToolsConf()
#' conf <- getConfFromData(
#'   conf = conf,
#'   simData = data$simData,
#'   fieldData = data$fieldData
#' )
#' arrivalTimes <- getArrivalTimes(data$simData$Infected)
#' envs <- babsimHospital(arrivalTimes = arrivalTimes, conf = conf, para = para)
#' fieldEvents <- getRealBeds(
#'   data = data$fieldData,
#'   resource = c("bed", "intensiveBed", "intensiveBedVentilation")
#' )
#' res <- getDailyMaxResults(envs = envs, fieldEvents = fieldEvents, conf = conf)
#' @export

getDailyMaxResults <- function(envs, fieldEvents, conf) {
  ## the following variables are local, see
  ## https://www.r-bloggers.com/no-visible-binding-for-global-variable:
  resource <- time <- replication <- NULL
  ICU <- conf$ICU
  simStartDate <- as.Date(conf$simulationDates$StartDate, format = "%Y-%m-%d")
  ## determine start and end date of the field data (ground truth) StartDate should
  ## be >= simStartDate fieldStartDate <- as.Date(min(fieldEvents$date))
  ## fieldEndDate <- as.Date(max(fieldEvents$date))
  fieldStartDate <- as.Date(conf$fieldDates$StartDate, format = "%Y-%m-%d")
  fieldEndDate <- as.Date(conf$fieldDates$EndDate, format = "%Y-%m-%d")
  #
  offset <- as.numeric(fieldStartDate - simStartDate)
  duration <- as.numeric(fieldEndDate - fieldStartDate)
  total <- offset + duration
  ##
  resources <- get_mon_resources(envs)
  ## RKI data based simulations start earlier (when no ICU test data are available).
  ## These days without corresponding test/real world data will be cut off, e.g.,
  ## they are considered as an offset: amntDaysLateStart <-
  ## as.numeric(fieldStartDate-simStartDate)
  resources <- resources %>% dplyr::filter((time >= offset) & (time < total))
  #### observedPeriod <- as.numeric(fieldEndDate-fieldStartDate) fieldStartDate is not
  #### needed: fieldStartDate-simStartDate + fieldEndDate-fieldStartDate =
  #### fieldEndDate -simStartDate finalTimeICU <- amntDaysLateStart + observedPeriod
  #### resources <- resources %>% dplyr::filter(time <= finalTimeICU)
  resources$time <- round(resources$time)
  ## ICU data treat intensiveBed as bed: New 2020-10-05 (ver 2.0.0): bed -> NULL
  ## 20201014: if(ICU) resources <- resources %>% dplyr::filter(resource != 'bed')
  ## intensiveBed -> bed intensiveBedVentilation -> intensiveBedVentilation
  ## 20201014: if(ICU) resources$resource <- gsub('intensiveBed', 'bed',
  ## resources$resource) 20201014: if(ICU) resources$resource <-
  ## gsub('bedVentilation', 'intensiveBedVentilation', resources$resource) Resource
  ## requirement of a day is set to the maximum of that day not needed:
  ## resourcesMaxSystem1 <- resources %>% group_by(resource, time, replication) %>%
  ## slice(which.max(system)) If there are replicates, look for median, worst and
  ## best case scenario
  if (dim(resources)[1] > 0) {
    resourcesMaxSystem <- resources %>%
      dplyr::group_by(resource, time) %>%
      dplyr::mutate(upper = max(system)) %>%
      dplyr::mutate(lower = min(system)) %>%
      dplyr::mutate(med = median(system))
    resourcesMaxSystem$date <- as.Date(as.POSIXct((resourcesMaxSystem$time) *
      24 * 60 * 60, origin = simStartDate))
    resourcesMaxSystem$source <- "babsim"
  } else {
    resourcesMaxSystem <- resources
  }
  ###
  n <- dim(fieldEvents)[1]
  fieldEvents$server <- fieldEvents$med
  fieldEvents$queue <- rep(0, n)
  fieldEvents$capacity <- rep(Inf, n)
  fieldEvents$queue_size <- rep(Inf, n)
  fieldEvents$system <- fieldEvents$med
  fieldEvents$limit <- rep(Inf, n)
  fieldEvents$replication <- rep(1, n)
  fieldEvents$upper <- fieldEvents$med
  fieldEvents$lower <- fieldEvents$med
  ## Add simulations from other sources to plots
  resourcesMaxSystem <- dplyr::bind_rows(resourcesMaxSystem, fieldEvents)
  return(resourcesMaxSystem)
}
