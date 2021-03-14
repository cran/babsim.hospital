#' @title postprocessEnvs
#'
#' @description Postprocess results from several \code{\link[simmer]{simmer}} results.
#' Input: \code{\link[simmer]{simmer}} simulation environment.
#' The method \code{\link[simmer]{get_mon_resources}} function is used to extract information from
#' the \code{babsim.hospital} simulation.
#'
#' @section \code{\link{plotPostprocessedEnvs}}
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
#'
#' @param StartDate Date[1:1], format: 'YYYY-MM-DD' First day of the simulation. Default: \code{'2020-03-03'}.
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
#' # 2. Postprocess simulation results:
#' res <- postprocessEnvs(envs = y)
#' # 3. Plot results
#' p <- plotPostprocessedEnvs(res)
#' @export

postprocessEnvs <- function(envs, StartDate = "2020-03-03") {
  ## the following variable are local, see
  ## https://www.r-bloggers.com/no-visible-binding-for-global-variable:
  resource <- time <- replication <- NULL
  ## Extract resources from simulation
  resources <- get_mon_resources(envs)
  resources$time <- round(resources$time) ## Round Time to single days!
  ## Resource requirement of a day is set to the maximum of that day not needed:
  ## resourcesMaxSystem1 <- resources %>% group_by(resource, time, replication) %>%
  ## slice(which.max(system)) If there are replicates, look for median, worst and
  ## best case scenario
  resourcesMaxSystem <- resources %>%
    group_by(resource, time) %>%
    mutate(upper = max(system)) %>%
    mutate(lower = min(system)) %>%
    mutate(med = median(system))
  ## Cutoff date for plotting resourcesMaxSystem <- resourcesMaxSystem %>%
  ## filter(time <= (observedPeriod + as.numeric(as.Date(StartDateSimulation) -
  ## as.Date(StartDate)))) not used: resourcesMaxSystem <- resourcesMaxSystem %>%
  ## filter(time <= (observedPeriod)) modified:
  ## resourcesMaxSystem$date<-as.POSIXct((resourcesMaxSystem$time-min(resourcesMaxSystem$time))*24*60*60,origin=StartDate)
  ## not correct: -min(resourcesMaxSystem$time)
  resourcesMaxSystem$date <- as.POSIXct((resourcesMaxSystem$time) * 24 * 60 * 60,
    origin = StartDate
  )
  # resourcesMaxSystem$rwdate<-round_date(resourcesMaxSystem$date,unit='week')
  resourcesMaxSystem$source <- "babsim"
  # resourcesMaxSystem <- resourcesMaxSystem %>% filter(resource != 'nurse')
  # resourcesMaxSystem <- bind_rows(resourcesMaxSystem, fieldEvents)
  return(resourcesMaxSystem)
}
