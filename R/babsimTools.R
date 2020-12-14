#' @title getRealBeds
#' 
#' @description  
#' Convert daily data, e.g., a data.frame with the columns
#' \code{bed, intensiveBedVentilation, Day} 
#' into event data, e.g., a data.frame with the following columns
#' \code{resource, time,  med, source, date}. 
#' 
#' @details  
#' Prepares data for combination with output (env) 
#' from \link[simmer]{simmer}.
#' Extracts and formats real data from the real data sets, e.g., 
#' \code{icudata}. 
#' The resulting data frame can be combined with
#' the output from the simulation run.
#' Can be used to add the true data (ground truth) to the simulated data.
#'  
#' @param data (n, m) data frame with daily bed data, e.g., 
#' from \code{\link{icudata}}
#' @param resource vector of resource names, e.g., "bed". Default:
#'  \code{resource=c("bed", "intensiveBedVentilation")}. For GA data use:
#'  \code{resource=c("bed", "intensiveBed", "intensiveBedVentilation")}
#'
#' @return This function returns a (n x m, 5) data frame with:
#' \describe{
#'		\item{\code{resource} (chr)}{name of the seized resource}
#'		\item{\code{time} (int)}{time step, starts with \code{1}}
#'		\item{\code{med} (int)}{amount of the seized resource}
#'		\item{\code{source} (chr)}{name of the simulation that was used}
#'		\item{\code{date} (Date)}{time, format: \code{yyyy-mm-dd}}
#'	}
#'	
#' @seealso \code{\link{getIcuBeds}}.
#' @examples
#' # First example shows how to process the GA data
#' GABeds <- getRealBeds(data = babsim.hospital::dataCovidBeds20200624,
#'                       resource = c("bed", "intensiveBed", "intensiveBedVentilation"))
#'  
#' # Second example shows how to process the DIVI ICU data.
#' icu <- babsim.hospital::icudata
#' icuCov <- as.data.frame(xtabs(faelle_covid_aktuell ~  daten_stand, icu))
#' icuCov$daten_stand <- as.Date(icuCov$daten_stand)
#' icuCovBeatm <- as.data.frame(xtabs(faelle_covid_aktuell_beatmet ~ daten_stand, icu))
#' icuCovBeatm$daten_stand <- as.Date(icuCovBeatm$daten_stand)
#' Day <- as.Date(icuCovBeatm$daten_stand)
#' dataICUBeds20200821 <- data.frame(bed = (icuCov$Freq - icuCovBeatm$Freq), 
#'                        intensiveBedVentilation = icuCovBeatm$Freq, 
#'                        Day =  as.Date(icuCovBeatm$daten_stand))
#' ICUBeds <- getRealBeds(data = dataICUBeds20200821,
#'                        resource = c("bed", "intensiveBedVentilation"))
#'                      
#' @export
getRealBeds <- function(data, resource){
  n <- nrow(data)
  t <- 0:(n-1)
  do.call(rbind, lapply(resource, function(r) {
    data.frame("resource" = rep(r, n),
               "time" = t,
               "med" = data[, r],
               "source" = rep("GA", n),
               "date" = data$Day)
  }))
}


#' @title getPeakVec
#' 
#' @description Generate peak values.
#' 
#' @param peakData Vector of time steps and peak heights.
#' @param StartDate Day, simulation starts.
#' @param EndDate Day, simulation ends.
#' 
#' @return This function returns a vector of peaks data.

#' @examples
#' getPeakVec()
#' @export

getPeakVec <- function(
  peakData = c(10,100),
  StartDate = "2020-03-03",
  EndDate = "2020-06-24"
  ){
  observedPeriod = 1 + as.numeric(as.Date(EndDate)- as.Date(StartDate))
  v <- rep(0, observedPeriod)
  n <- length(peakData)/2
  for (i in 1:n){
  v[peakData[2*i-1]] <- peakData[2*i] 
    }
  return(v)
  }

#' @title getInfectedPerDay
#' 
#' @description Generate Poisson distributed infections.
#' This function calculates \code{n}, the number of days between \code{StartDate}
#' and \code{EndDate}, and returns a vector with \code{n} realizations of a  
#' Poisson(lambda) distributed random variable.
#' 
#' @param lambda Expected number of infections/day.
#' @param StartDate Day, simulation starts
#' @param EndDate Day, simulation ends
#' 
#' @importFrom stats rpois
#' 
#' @return This function returns a vector that lists the number of infections.

#' @examples
#' StartDate <- "2020-03-03"
#' EndDate <- "2020-06-24"
#' getInfectedPerDay(lambda = 4, StartDate = StartDate, EndDate = EndDate)
#' @export

getInfectedPerDay <- function(
  lambda = 4,
  StartDate = "2020-03-03",
  EndDate = "2020-06-24"
  ){
  observedPeriod = 1 + as.numeric(as.Date(EndDate)- as.Date(StartDate))
  return(rpois(observedPeriod , lambda))
}


#' @title getArrivalTimes
#' 
#' @description Generate arrival times.
#' 
#' @param xDaily Vector that contains the number of arrivals for each day.
#' 
#' @importFrom stats runif
#' 
#' @return This function returns a data frame of arrival times with the following entries:
#' \describe{
#'		\item{\code{time} (num)}{name of the seized resource}
#'	}
#'	
#'	@seealso \code{\link{rkiToBabsimArrivals}}
#'	
#' @examples
#' x <- dataCovidBeds20200624
#' arrivalTimes <- getArrivalTimes(xDaily=x$Infected) 
#' # For RKI data, use rkiToBabsimArrivals as follows:
#' arrivalTimes <- rkiToBabsimArrivals(rki=babsim.hospital::rkidata) 
#' @export
getArrivalTimes <- function(xDaily){
  # to avoid duplicates:
  orng <- RNGkind()
  on.exit(RNGkind(orng[1], orng[2], orng[3]))
  RNGkind("Wichmann-Hill")

  totalCases <- sum(xDaily)
  # Time       =  day                              + fractional part day
  arrivalTimes <- rep(seq_along(xDaily)-1, xDaily) + runif(totalCases, 0, 1)
  data.frame(time = sort(arrivalTimes))
}



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
#'  \item{"resource": }{resources name}
#'  \item{"time": }{time instant of the event that triggered the state change}
#'  \item{"server": }{server count}
#'  \item{"queue": }{queue count}
#'  \item{"capacity": }{capacity}
#'  \item{"queue_size": }{queue size}
#'  \item{"system": }{system count (server + queue). If no queues are used, system values equal server values.}
#'  \item{"system_limit": }{system limit (capacity + queue_size)}
#'  }
#' 
#' @param envs \code{\link[simmer]{simmer}} simulation environment. 
#' Result from \code{babsim.hospital} simulation runs, e.g., output from \code{\link{babsimHospital}}.
#' 
#' @param StartDate Date[1:1], format: "YYYY-MM-DD" First day of the simulation. Default: \code{"2020-03-03"}.
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
#'		\item{\code{resource} (chr)}{name of the seized resource: "bed" "bed" "bed" "bed" ...}
#'		\item{\code{time} (num)}{time step:  3 10 12 13 14 15 15 15 15 16 ...}
#'		\item{\code{server} (int)}{server: 1 2 3 2 3 4 3 4 5 6 ...}
#'		\item{\code{limit} (num)}{limit: Inf Inf Inf Inf Inf ...}
#'		\item{\code{replication} (int)}{replication:  1 1 1 1 1 1 1 1 1 1 ...}
#'		\item{\code{upper} (int)}{upper: 1 2 3 2 3 5 5 5 5 7 ...}
#'		\item{\code{lower} (int)}{lower: 1 2 3 2 3 3 3 3 3 5 ...}
#'		\item{\code{med} (num)}{med: 1 2 3 2 3 4 4 4 4 6 ...}
#'		\item{\code{date} (POSIXct)}{time, format: \code{yyyy-mm-dd hh:mm.ss}}
#'		\item{\code{rwdate} (POSIXct)}{format: "2020-03-01" "2020-03-08" "2020-03-15" "2020-03-15" ...}
#'		\item{\code{source} (chr)}{name of the simulation that was used: "babsim" "babsim" "babsim" "babsim" ...}
#'		}
#'
#' @examples
#' set.seed(123)
#' # 1. Generate simulation data based on number of infected persons per day: 
#' x <- dataCovidBeds20200624
#' StartDate <- x$Day[1]
#' EndDate <- x$Day[length(x$Day)] 
#' arrivalTimes <- getArrivalTimes(x$Infected) 
#' para = babsimHospitalPara()
#' conf = babsimToolsConf()
#' y <- babsimHospital(arrivalTimes = arrivalTimes, 
#'                     conf = conf,
#'                     para = para)
#'
#' # 2. Postprocess simulation results:
#' res <- postprocessEnvs(envs = y)
#' # 3. Plot results
#' p <- plotPostprocessedEnvs(res)
#' @export

postprocessEnvs <- function(envs, StartDate = "2020-03-03")
  {
  ## the following variable are local, see 
  ## https://www.r-bloggers.com/no-visible-binding-for-global-variable:
  resource <- time <- replication <- NULL
  ## Extract resources from simulation
  resources <- get_mon_resources(envs)
  resources$time <- round(resources$time) ## Round Time to single days!
  ## Resource requirement of a day is set to the maximum of that day
  ### not needed: 
  ### resourcesMaxSystem1 <- resources %>% group_by(resource, time, replication) %>% slice(which.max(system))
  ## If there are replicates, look for median, worst and best case scenario
  resourcesMaxSystem <- resources %>% group_by(resource, time) %>%  mutate(upper = max(system)) %>% mutate(lower = min(system)) %>% mutate(med = median(system))
  ## Cutoff date for plotting
  ### resourcesMaxSystem <- resourcesMaxSystem %>% filter(time <= (observedPeriod + as.numeric(as.Date(StartDateSimulation) - as.Date(StartDate))))
  ### not used: resourcesMaxSystem <- resourcesMaxSystem %>% filter(time <= (observedPeriod))
  ### modified: 
  ### resourcesMaxSystem$date<-as.POSIXct((resourcesMaxSystem$time-min(resourcesMaxSystem$time))*24*60*60,origin=StartDate)
  ### not correct: -min(resourcesMaxSystem$time)
  resourcesMaxSystem$date <- as.POSIXct((resourcesMaxSystem$time)*24*60*60,origin=StartDate)
  # resourcesMaxSystem$rwdate<-round_date(resourcesMaxSystem$date,unit="week")
  resourcesMaxSystem$source = "babsim"
  # resourcesMaxSystem <- resourcesMaxSystem %>% filter(resource != "nurse")
  # resourcesMaxSystem <- bind_rows(resourcesMaxSystem, fieldEvents)
  return(resourcesMaxSystem)
}


#' @title envToTibble
#' 
#' @description Convert  babsim simulation results to tibble data.
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
#'  \item{"resource": }{resources name}
#'  \item{"time": }{time instant of the event that triggered the state change}
#'  \item{"server": }{server count}
#'  \item{"queue": }{queue count}
#'  \item{"capacity": }{capacity}
#'  \item{"queue_size": }{queue size}
#'  \item{"system": }{system count (server + queue). If no queues are used, system values equal server values.}
#'  \item{"system_limit": }{system limit (capacity + queue_size)}
#'  }
#' 
#' @param envs \code{\link[simmer]{simmer}} simulation environment. 
#' Result from \code{babsim.hospital} simulation runs, e.g., output from \code{\link{babsimHospital}}.
#' 
#' @param fieldEvents Real values. Output from \code{\link{getRealBeds}}, i.e., 
#' a (nxm, 5)-dim data.frame with the following variables:
#' \describe{
#'   \item{\code{resource}}{chr:  "bed" "bed" "bed" "bed" ...}
#'   \item{\code{time}}{int:  1 2 3 4 5 6 7 8 9 10 ...}
#'   \item{\code{med}}{int:   2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{\code{source}}{chr:  "GA" "GA" "GA" "GA" ...}
#'   \item{\code{date}}{ POSIXct, format: "2020-03-03 01:00:00" "2020-03-04 01:00:00" "2020-03-05 01:00:00" "2020-03-06 01:00:00" ...}
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
#'       \item{\code{dataset}}{char name of the data set. Default: "GA"}
#'       \item{\code{simulationDates}}{list with \code{StartDate} and \code{EndDate}. 
#'       Period that is used for the simulation (babsim, simmer). Default: 
#'       \code{list(StartDate = "2020-03-03", EndDate = "2020-06-24")}}
#'       \item{\code{fieldDates}}{list with \code{StartDate} and \code{EndDate}. 
#'       Period when real data is available (resource usage). Default: 
#'       \code{list(StartDate = "2020-03-03", EndDate = "2020-06-24")}}
#'       \item{\code{simulationData}}{data frame. Data used for the simulation. Default: 
#'       \code{\link{dataCovidBeds20200624}}}
#'       \item{\code{fieldEvents}}{data frame. Data used for the evaluation (error). Default: 
#'       \code{\link{GABeds220200624}}}
#'       \item{\code{resource}}{vector with resource names. 
#'       Default: c("bed", "intensiveBed", "intensiveBedVentilation")}      
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
#'		\item{\code{resource} (chr)}{name of the seized resource: "bed" "bed" "bed" "bed" ...}
#'		\item{\code{time} (num)}{time step:  3 10 12 13 14 15 15 15 15 16 ...}
#'		\item{\code{server} (int)}{server: 1 2 3 2 3 4 3 4 5 6 ...}
#'		\item{\code{limit} (num)}{limit: Inf Inf Inf Inf Inf ...}
#'		\item{\code{replication} (int)}{replication:  1 1 1 1 1 1 1 1 1 1 ...}
#'		\item{\code{upper} (int)}{upper: 1 2 3 2 3 5 5 5 5 7 ...}
#'		\item{\code{lower} (int)}{lower: 1 2 3 2 3 3 3 3 3 5 ...}
#'		\item{\code{med} (num)}{med: 1 2 3 2 3 4 4 4 4 6 ...}
#'		\item{\code{date} (POSIXct)}{time, format: \code{yyyy-mm-dd hh:mm.ss}}
#'		\item{\code{rwdate} (POSIXct)}{format: "2020-03-01" "2020-03-08" "2020-03-15" "2020-03-15" ...}
#'		\item{\code{source} (chr)}{name of the simulation that was used: "babsim" "babsim" "babsim" "babsim" ...}
#'		}
#'
#' @examples
#' data <- getSyntheticData() 
#' para <- babsimHospitalPara()
#' conf <- babsimToolsConf()
#' conf <- getConfFromData(conf = conf,
#'                        simData = data$simData, 
#'                        fieldData = data$fieldData) 
#' arrivalTimes <- getArrivalTimes(data$simData$Infected)
#' fieldEvents <- getRealBeds(data = data$fieldData,
#' resource = c("bed", "intensiveBed", "intensiveBedVentilation"))
#' envs <- babsimHospital(arrivalTimes = arrivalTimes, conf = conf, para = para)
#' res <- envToTibble(envs = envs, conf = conf, fieldEvents=fieldEvents)
#' @export

envToTibble <- function(envs,
                        fieldEvents,
                        conf = babsimToolsConf()){
  ## the following variables are local, see 
  ## https://www.r-bloggers.com/no-visible-binding-for-global-variable:
  resource <- time <- replication <- NULL
  ICU <- conf$ICU
  simStartDate <- as.Date(conf$simulationDates$StartDate)
  ## determine start and end date of the field data (ground truth)
  ## StartDate should be >= simStartDate
  fieldStartDate <- as.Date(conf$simulationDates$StartDate)
  fieldEndDate <- as.Date(conf$simulationDates$EndDate)
  #
  offset <- as.numeric(fieldStartDate - simStartDate)
  duration <- as.numeric(fieldEndDate - fieldStartDate) 
  total <- offset + duration
  ##
  resources <- get_mon_resources(envs)
  ## RKI data based simulations start earlier (when no ICU test data are available).
  ## These days without corresponding test/real world data will be cut off, e.g.,
  ## they are considered as an offset:
  ### amntDaysLateStart <- as.numeric(fieldStartDate-simStartDate)
  resources <- resources %>% dplyr::filter( (time >= offset) & (time < total) )
  #### observedPeriod <- as.numeric(fieldEndDate-fieldStartDate)
  ## fieldStartDate is not needed:
  ## fieldStartDate-simStartDate + fieldEndDate-fieldStartDate = fieldEndDate -simStartDate
  ### finalTimeICU <- amntDaysLateStart + observedPeriod
  #resources <- resources %>% dplyr::filter(time <= finalTimeICU)
  resources$time <- round(resources$time) 
  ## ICU data treat intensiveBed as bed:
  ## New 2020-10-05 (ver 2.0.0):
  ## bed -> NULL
  ### 20201014: if(ICU) resources <- resources %>% filter(resource != "bed")
  ## intensiveBed -> bed
  ## intensiveBedVentilation -> intensiveBedVentilation
  ### 20201014: if(ICU) resources$resource <- gsub('intensiveBed', 'bed', resources$resource)
  ### 20201014: if(ICU) resources$resource <- gsub('bedVentilation', 'intensiveBedVentilation', resources$resource)
  ## Resource requirement of a day is set to the maximum of that day
  ### not needed: 
  ### resourcesMaxSystem1 <- resources %>% group_by(resource, time, replication) %>% slice(which.max(system))
  ## If there are replicates, look for median, worst and best case scenario
  if( dim(resources)[1] > 0){
    resourcesMaxSystem <- resources %>% dplyr::group_by(resource, time) %>%  dplyr::mutate(upper = max(system)) %>% dplyr::mutate(lower = min(system)) %>% dplyr::mutate(med = median(system))
    resourcesMaxSystem$date <- as.Date( as.POSIXct((resourcesMaxSystem$time)*24*60*60,origin=simStartDate) )
    resourcesMaxSystem$source = "babsim"
  }else{
    resourcesMaxSystem <- resources
  }
  ###
  ###
  n <- dim(fieldEvents)[1]
  fieldEvents$server <- fieldEvents$med
  fieldEvents$queue <- rep(0,n)
  fieldEvents$capacity <- rep(Inf,n)
  fieldEvents$queue_size <- rep(Inf,n)
  fieldEvents$system <- fieldEvents$med
  fieldEvents$limit <- rep(Inf, n)
  fieldEvents$replication <- rep(1,n)
  fieldEvents$upper <- fieldEvents$med
  fieldEvents$lower <- fieldEvents$med
  ## Add simulations from other sources to plots
  resourcesMaxSystem <- dplyr::bind_rows(resourcesMaxSystem, fieldEvents)
  ###
  return(resourcesMaxSystem)
}



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
#'  \item{"resource": }{resources name}
#'  \item{"time": }{time instant of the event that triggered the state change}
#'  \item{"server": }{server count}
#'  \item{"queue": }{queue count}
#'  \item{"capacity": }{capacity}
#'  \item{"queue_size": }{queue size}
#'  \item{"system": }{system count (server + queue). If no queues are used, system values equal server values.}
#'  \item{"system_limit": }{system limit (capacity + queue_size)}
#'  }
#' 
#' @param envs \code{\link[simmer]{simmer}} simulation environment. 
#' Result from \code{babsim.hospital} simulation runs, e.g., output from \code{\link{babsimHospital}}.
#' @param fieldEvents Real values. Output from \code{\link{getRealBeds}}, i.e., 
#' a (nxm, 5)-dim data.frame with the following variables:
#' \describe{
#'   \item{\code{resource}}{chr:  "bed" "bed" "bed" "bed" ...}
#'   \item{\code{time}}{int:  1 2 3 4 5 6 7 8 9 10 ...}
#'   \item{\code{med}}{int:   2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{\code{source}}{chr:  "GA" "GA" "GA" "GA" ...}
#'   \item{\code{date}}{ POSIXct, format: "2020-03-03 01:00:00" "2020-03-04 01:00:00" "2020-03-05 01:00:00" "2020-03-06 01:00:00" ...}
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
#'       \item{\code{dataset}}{char name of the data set. Default: "GA"}
#'       \item{\code{simulationDates}}{list with \code{StartDate} and \code{EndDate}. 
#'       Period that is used for the simulation (babsim, simmer). Default: 
#'       \code{list(StartDate = "2020-03-03", EndDate = "2020-06-24")}}
#'       \item{\code{fieldDates}}{list with \code{StartDate} and \code{EndDate}. 
#'       Period when real data is available (resource usage). Default: 
#'       \code{list(StartDate = "2020-03-03", EndDate = "2020-06-24")}}
#'       \item{\code{simulationData}}{data frame. Data used for the simulation. Default: 
#'       \code{\link{dataCovidBeds20200624}}}
#'       \item{\code{fieldEvents}}{data frame. Data used for the evaluation (error). Default: 
#'       \code{\link{GABeds220200624}}}
#'       \item{\code{resource}}{vector with resource names. 
#'       Default: c("bed", "intensiveBed", "intensiveBedVentilation")}      
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
#'		\item{\code{resource} (chr)}{name of the seized resource: "bed" "bed" "bed" "bed" ...}
#'		\item{\code{time} (num)}{time step:  3 10 12 13 14 15 15 15 15 16 ...}
#'		\item{\code{server} (int)}{server: 1 2 3 2 3 4 3 4 5 6 ...}
#'		\item{\code{limit} (num)}{limit: Inf Inf Inf Inf Inf ...}
#'		\item{\code{replication} (int)}{replication:  1 1 1 1 1 1 1 1 1 1 ...}
#'		\item{\code{upper} (int)}{upper: 1 2 3 2 3 5 5 5 5 7 ...}
#'		\item{\code{lower} (int)}{lower: 1 2 3 2 3 3 3 3 3 5 ...}
#'		\item{\code{med} (num)}{med: 1 2 3 2 3 4 4 4 4 6 ...}
#'		\item{\code{date} (POSIXct)}{time, format: \code{yyyy-mm-dd hh:mm.ss}}
#'		\item{\code{rwdate} (POSIXct)}{format: "2020-03-01" "2020-03-08" "2020-03-15" "2020-03-15" ...}
#'		\item{\code{source} (chr)}{name of the simulation that was used: "babsim" "babsim" "babsim" "babsim" ...}
#'		}
#'
#' @examples
#' data <- getSyntheticData() 
#' para <- babsimHospitalPara()
#' conf <- babsimToolsConf()
#' conf <- getConfFromData(conf = conf,
#'                         simData = data$simData, 
#'                         fieldData = data$fieldData) 
#' arrivalTimes <- getArrivalTimes(data$simData$Infected)
#' envs <- babsimHospital(arrivalTimes = arrivalTimes, conf = conf, para = para)
#' fieldEvents <- getRealBeds(data = data$fieldData,
#'     resource = c("bed", "intensiveBed", "intensiveBedVentilation"))
#' res <- getDailyMaxResults(envs = envs,  fieldEvents = fieldEvents, conf = conf)
#' @export

getDailyMaxResults <- function(envs,
                               fieldEvents,
                               conf){
  ## the following variables are local, see 
  ## https://www.r-bloggers.com/no-visible-binding-for-global-variable:
  resource <- time <- replication <- NULL
  ICU <- conf$ICU
  simStartDate <- as.Date(conf$simulationDates$StartDate, format="%Y-%m-%d")
  ## determine start and end date of the field data (ground truth)
  ## StartDate should be >= simStartDate
  #fieldStartDate <- as.Date(min(fieldEvents$date))
  #fieldEndDate <- as.Date(max(fieldEvents$date))
  fieldStartDate <- as.Date(conf$fieldDates$StartDate, format="%Y-%m-%d")
  fieldEndDate <- as.Date(conf$fieldDates$EndDate, format="%Y-%m-%d")
  #
  offset <- as.numeric(fieldStartDate - simStartDate)
  duration <- as.numeric(fieldEndDate - fieldStartDate) 
  total <- offset + duration
  ##
  resources <- get_mon_resources(envs)
  ## RKI data based simulations start earlier (when no ICU test data are available).
  ## These days without corresponding test/real world data will be cut off, e.g.,
  ## they are considered as an offset:
  ### amntDaysLateStart <- as.numeric(fieldStartDate-simStartDate)
  resources <- resources %>% dplyr::filter( (time >= offset) & (time < total) )
  #### observedPeriod <- as.numeric(fieldEndDate-fieldStartDate)
  ## fieldStartDate is not needed:
  ## fieldStartDate-simStartDate + fieldEndDate-fieldStartDate = fieldEndDate -simStartDate
  ### finalTimeICU <- amntDaysLateStart + observedPeriod
  #resources <- resources %>% dplyr::filter(time <= finalTimeICU)
  resources$time <- round(resources$time) 
  ## ICU data treat intensiveBed as bed:
  ## New 2020-10-05 (ver 2.0.0):
  ## bed -> NULL
  ### 20201014:  if(ICU) resources <- resources %>% dplyr::filter(resource != "bed")
  ## intensiveBed -> bed
  ## intensiveBedVentilation -> intensiveBedVentilation
  ### 20201014:  if(ICU) resources$resource <- gsub('intensiveBed', 'bed', resources$resource)
  ### 20201014:  if(ICU) resources$resource <- gsub('bedVentilation', 'intensiveBedVentilation', resources$resource)
  ## Resource requirement of a day is set to the maximum of that day
  ### not needed: 
  ### resourcesMaxSystem1 <- resources %>% group_by(resource, time, replication) %>% slice(which.max(system))
  ## If there are replicates, look for median, worst and best case scenario
  if( dim(resources)[1] > 0){
  resourcesMaxSystem <- resources %>% dplyr::group_by(resource, time) %>%  
      dplyr::mutate(upper = max(system)) %>% 
      dplyr::mutate(lower = min(system)) %>% 
      dplyr::mutate(med = median(system))
  resourcesMaxSystem$date <- as.Date( as.POSIXct((resourcesMaxSystem$time)*24*60*60,origin=simStartDate) )
  resourcesMaxSystem$source = "babsim"
  }else{
    resourcesMaxSystem <- resources
  }
  ###
  n <- dim(fieldEvents)[1]
  fieldEvents$server <- fieldEvents$med
  fieldEvents$queue <- rep(0,n)
  fieldEvents$capacity <- rep(Inf,n)
  fieldEvents$queue_size <- rep(Inf,n)
  fieldEvents$system <- fieldEvents$med
  fieldEvents$limit <- rep(Inf, n)
  fieldEvents$replication <- rep(1,n)
  fieldEvents$upper <- fieldEvents$med
  fieldEvents$lower <- fieldEvents$med
  ## Add simulations from other sources to plots
  resourcesMaxSystem <- dplyr::bind_rows(resourcesMaxSystem, fieldEvents)
  return(resourcesMaxSystem)
}


#' @title getError
#' 
#' @description Determine error from babsim runs. 
#' This error is the sum of the RMSE values for bed, intensiveBed, and intensiveBedVentilation.
#' 
#' @param res Results from \code{getDailyMaxResults}.
#' @param conf configuration
#' 
#' @importFrom  dplyr filter
#' @importFrom  dplyr left_join
#' @importFrom  stats complete.cases
#'  
#' @return This function returns a num value, that represents the combined rmse from the beds.
#' 
#' @examples
#' para = babsimHospitalPara()
#' conf = babsimToolsConf()
#' data = getObkData()
#' set.seed(conf$seed)
#' para <- checkSimPara(para)
#' arrivalTimes <- getArrivalTimes(data$simData$Infected)
#' envs <- babsimHospital(arrivalTimes = arrivalTimes,
#'                          conf = conf,
#'                          para = para)
#' fieldEvents <- getRealBeds(data = data$fieldData,
#'                            resource= conf$ResourceNames)
#' res <- getDailyMaxResults(envs = envs,  
#'                           fieldEvents = fieldEvents,
#'                           conf = conf)
#' err <- getError(res, conf = conf)
#' 
#' @export

getError <- function(res,
                     conf){
  if (conf$verbosity > 100){
  print("BEGIN: getEror: ###########################")
  printConf(conf)
  print("END: getError: ###########################")
  }
  rmseBed = 0
  ### 20201014:   resource <- unique(res$resource)
  resource <- conf$ResourceEval
  ## experimental: weighting factor 10 for ventilation:
  w <- rep(1, length(resource))
  w[2] <- conf$w2[2]
  i <- 1
  for (r in resource){
    res1 <- res %>% filter(resource == r  & source == "babsim")
    df1 <- unique(data.frame(date = res1$date, x = res1$med))
    df1 <- df1[order(df1$date),]
    res2 <- res %>% filter(resource == r  & source == "GA")
    df2 <- unique(data.frame(date = res2$date, x= res2$med))
    df2 <- df2[order(df2$date),]
    fillDate <- which(!(df2$date %in% df1$date))
    if(length(fillDate) > 0){
        df1 <- rbind(df1, data.frame("date" = df2$date[fillDate],
                                          "x" = 0))
    }
    dfBed <- dplyr::left_join(df1, df2, by=c("date"))
    #dfBed[is.na(dfBed)] <- 0
    dfBed <- dfBed[complete.cases(dfBed),]
    dfBed <- dfBed[order(dfBed$date),]
    if (conf$verbosity > 100){
      print("BEGIN: getEror: dfBed ##########################")
      printConf(dfBed)
      print(summary(dfBed$date))
      print("END: getError: dfBed ###########################")
    }
    #browser()
    rmseBed <- rmseBed + w[i] * weighted_rmse(dfBed[,3], dfBed[,2])
    i <- i +1
  }
  return(rmseBed)
}


#' weighted_rmse
#' 
#' Calculate a weighted RMSE. Weights are based on "time" in the case of the weights variable. 
#' (E.g. older errors weigh less). And also based on the "direction" e.g. predicting to few used ressources
#' is worse than predicting a few ressources used too much.
#'
#' @param actual Real Data, vector of observations
#' @param predicted Predicted Data, vector of observations
#' @param weights Time based decay. Default is an exponential decay
#' @param worsenGoodExpections Factor by how much predicting too few used ressources should be punished more.
#'
#' @return weighted RMSE
#' @export
weighted_rmse <- function(actual, predicted, weights = exp(-(length(actual):1)/14), worsenGoodExpections = 1.5){
    scaledweights <- weights/sum(weights)
    error <- predicted-actual
    error[error > 0] <- error[error > 0] - 1
    error[error < 0] <- error[error < 0] * worsenGoodExpections
    sqrt(sum((error)^2*scaledweights))
}

#' @title rtgamma 
#' 
#' @param n number of observations
#' @param shape Gamma shape parameter
#' @param rate  Gamma rate parameter
#' @param shift shift parameter.
#' @param alpha upper quantile of gamma distribution. All values above alpha are truncated.
#' 
#' @description Random generation for the shifted and truncated Gamma distribution with parameters 
#'   \code{shape} and \code{scale}.
#'  
#' @return rtgamma generates random deviates. The length of the result is determined by \code{n}.
#' 
#' @examples 
#' 
#' rtgamma(n=1, shape=1, rate=1, shift=1, alpha=0.95)
#' 
#' @importFrom stats qgamma
#' @export
#'
rtgamma <- function(n=1, shape=1, rate=1, shift=0, alpha=0.95) {
  RNGkind("Wich")  
  u <- runif(n, 0, alpha)
  shift + qgamma(u, shape=shape, rate=rate)
}


#' @title getMatrixD 
#' 
#' @description Builds the duration matrix
#' 
#' @param para parameter vector, e.g., generated via 
#' \code{\link{babsimHospitalPara}}. Default: \code{\link{babsimHospitalPara}}
#' 
#' 
#' @return a matrix with transition durations
#' 
#' @examples 
#' 
#' getMatrixD()
#' 
#' @export

getMatrixD <- function(para = babsimHospitalPara()){
   x <- getStartParameter(para=para)
   D <- matrix(rep(0,100), nrow = 10, ncol = 10)
   D[1,3] = x[1]
   D[4,5] = x[3]
   D[4,6] = x[4]
   D[4,9] = x[5]
   D[4,10] = x[2] 
   D[5,6] = x[7]
   D[5,8] = x[6]
   D[5,9] = x[8]
   # D[5,10] = x[9]
   D[6,7] = x[9]
   # D[6,8] = x[10]
   D[6,9] = x[10]
   D[7,8] = x[11]
   D[7,9] = x[12]
   D[7,10] = x[28]
   D[8,10] = x[24]
   return(D)
  }


#' @title getMatrixP 
#' 
#' @description Builds the probability matrix
#' 
#' @param para parameter vector, e.g., generated via 
#' \code{\link{babsimHospitalPara}}. Default: \code{\link{babsimHospitalPara}}
#' 
#' 
#' @return a matrix with transition probabilities
#' 
#' @examples 
#' 
#' getMatrixP()
#' 
#' @export

getMatrixP <- function(para = babsimHospitalPara()){
  x <- getStartParameter(para=para)
  P <- matrix(rep(0,100), nrow = 10, ncol = 10)
  P[1,2] = 1-x[14]
  P[1,3] = x[14]
  P[2,2] = 1
  P[3,4] = 1- x[15]-x[16]
  P[3,5] = x[15]
  P[3,6] = x[16]
  P[4,5] = x[17]
  P[4,6] = x[18]
  P[4,9] = x[19]
  P[4,10] = 1 - x[17] - x[18] - x[19]
  P[5,6] = x[20]
  P[5,8] = 1 - x[20] - x[21] 
  P[5,9] = x[21]
  # P[5,10] = x[25]
  P[6,7] = x[22]
  # P[6,8] = 1 - x[26] - x[27]
  P[6,9] = 1- x[22]
  P[7,8] = 1 - x[23] - x[29]
  P[7,9] = x[23]
  P[7,10] = x[29]
  P[8,10] = 1
  P[9,9] = 1
  P[10, 10] = 1
  return(P)
}


#' @title updateMatrixP 
#' 
#' @description Updates the probability matrix
#' 
#' @param P matrix P. Default \code{\link{getMatrixP}} 
#' @param u list of factors used for the update
#' 
#' @return a matrix with updated transition probabilities
#' 
#' @examples 
#' 
#' u <- list(k=2)
#' R <- updateMatrixP(u=u)
#' 
#' @export

updateMatrixP <- function(P = getMatrixP(),
                          u){
  eps <- .Machine$double.eps * 1e10
  m <- rep(1, nrow(P))
  # Constrain u$k to [eps, 1e6]
  m[9] <- min(max(u$k, eps), 1e6)
  Q <- P %*% diag(m)
  S <- diag(1/rowSums(Q))
  return(S %*% Q)
}



#' @title getExpCoeff
#' 
#' @description Get coeff from an exponential fit
#' 
#' @param x x data
#' @param y y data
#' @param a0 start value (default: 1)
#' @param b0 start value (default: 1)
#' 
#' @return named vector of coefficients ("a", "b")
#' 
#' @importFrom stats nls
#' @importFrom stats coef
#' 
#' @examples 
#' age <- c(2,10,25,47,70,90)
#' risk <- c(0.01,0.07,0.15,0.65,3,12.64)
#' plot(age,risk)
#' ab <- getExpCoeff(x=age, y=risk, a0 = 1, b0 = 0)
#' y <- ab[1] * exp( ab[2] * age)
#' lines(age, y)
#' 
#' @export

getExpCoeff <- function(x, y, a0 = 0, b0 = 0){
data <- data.frame(x=x, y=y)
fit <- nls(y ~ a * exp( b * x), 
           data = data, 
           start = list(a = a0, b = b0))
return(coef(fit))
}


#' @title getDecision 
#' 
#' @description For given n probabilities 0 <= pi <= 1 with sum(pi)=1,
#' return 0,1,2,3,..,n with probability p0, p1, p2, ..., pn.
#' 
#' @param p vector of probabilities 
#'
#' @return int decision in the range from 0 to n
#' 
#' @examples 
#' 
#' p <- c(0.6,0.3,0.1)
#' getDecision(p)
#' 
#' @export

getDecision <- function(p){
  RNGkind("Wich") 
  ## tests will fail, because probabilities do not add up to 1
  ## p <- softmax(p)
  x <- runif(1)
  p <- cumsum ( c(0, p) )
  askRecursively <- function(i){
      ifelse( ( p[i] < x)  & (x <= p[i+1] ) , i-1, askRecursively(i+1) )
    }
  askRecursively(1)
}


#' @title softmax 
#' 
#' @description softmax function
#' 
#' @param par vector  
#'
#' @return num vector with components >= 0 and sum = 1
#' 
#' @examples 
#' 
#' p <- c(0.6,0.3,0.1)
#' softmax(p)
#' 
#' @export

softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
  }
  val <- exp(par - Lk)
  return(val)
}



#' @title getSyntheticData
#' 
#' @description Generate synthetic data
#' 
#' @param StartDate Start date. Default: \code{"2020-09-01"}
#' @param EndDate Start date. Default: \code{"2020-11-30"}
#' @param lambda Average number of daily infections. Default: 4
#' @param peakData Vector to define peak events. Odd entries represent days, 
#' even entries the number of infections. Default: \code{c(21,50, 28,40, 42,50)}, 
#' i.e., after 21 days 50 additional infections, after 28 days 40 additional infections,
#' and after 42 days 50 addditional infections to the base infection rate per day,
#' which is defined by \code{lambda}.
#' @param amntDaysSickness Length (in days) of the interval that is used to determine
#' the number of infected individuals that have to go to the hospital. Based on this
#' interval, the number of sick individuals is determined. The number of sick individuals
#' is multiplied by the hospitalization rate to determine the number of bed. Default: 20 
#' @param hospitalizationRates list of hospitalization rates, i.e., percentage
#' of sick individuals that need a \code{bed}, or an \code{intensiveBed}, or
#' an \code{intensiveBedVentilation}. Default: \code{list(rBed = 0.1347, 
#' rIntensiveBed = 0.004,  rIntensiveBedVentilation = 0.0171)}.
#' 
#' @importFrom slider slide_dbl
#'
#' @return data frame with the following entries: 
#' bed=bed,
#' intensiveBed = intensiveBed,
#' intensiveBedVentilation = intensiveBedVentilation, 
#' Day = Day,
#' Infected=Infected, 
#' Sick = Sick)
#' \describe{
#'   \item{\code{bed}}{int: COVID-19 beds}
#'   \item{\code{intensiveBed}}{int: COVID-19 ICU beds}
#'   \item{\code{intensiveBedVentilation}}{int  COVID-19 ICU beds with ventilation}
#'   \item{\code{Day}}{Date, format: "2020-05-01" "2020-05-02" "2020-05-03" "2020-05-04" ...}
#'   \item{\code{Infected}}{int: number of infected individuals (daily)}
#'   \item{\code{Sick}}{int: number of sick individuals (daily)}
#'  }
#'  
#' @examples 
#' 
#' dataSynth <- getSyntheticData() 
#' 
#' @export


getSyntheticData <- function(StartDate = "2020-09-01",
                             EndDate = "2020-11-30",
                             lambda = 4,
                             peakData = c(21,50, 28,40, 42,50),
                             amntDaysSickness = 20,
                             hospitalizationRates = list(rBed = 0.1347, 
                                                         rIntensiveBed = 0.004,
                                                         rIntensiveBedVentilation = 0.0171)){
  Infected <- getInfectedPerDay(lambda = lambda,
                                StartDate = StartDate, 
                                EndDate = EndDate) + 
              getPeakVec(peakData = peakData,
               StartDate = StartDate, 
               EndDate = EndDate)
  Day <- seq(from=as.Date(StartDate), to = as.Date(EndDate), by = "1 day")
  Sick <- slide_dbl(Infected, ~sum(.x), .before = (amntDaysSickness -1))
  bed <- round(hospitalizationRates$rBed * Sick)
  intensiveBed <- round(hospitalizationRates$rIntensiveBed * Sick)       
  intensiveBedVentilation <- round(hospitalizationRates$rIntensiveBedVentilation * Sick)  

  simData <- data.frame(Day = Day,
                        Infected = Infected)
  attr(simData,"StartDate") <- min(simData$Day)
  attr(simData,"EndDate") <- max(simData$Day)
  attr(simData,"Days") <- as.integer(1+ max(simData$Day) - min(simData$Day))
  attr(simData,"FeatureNames") <- c("Infected")
  
  fieldData <- data.frame(Day = Day,
                          bed = bed,
                          intensiveBed = intensiveBed,
                          intensiveBedVentilation = intensiveBedVentilation)
  attr(fieldData,"StartDate") <- min(fieldData$Day)
  attr(fieldData,"EndDate") <- max(fieldData$Day)
  attr(fieldData,"Days") <- as.integer(1+ max(fieldData$Day) - min(fieldData$Day))
  attr(fieldData, "ResourceNames") <- c("bed", "intensiveBed", "intensiveBedVentilation")
  
  return(list(simData = simData,
              fieldData = fieldData))
}



#' @title getObkData
#' 
#' @description Generate simData and fieldData from OBK data
#' 
#' @param data OBK data. Default: \code{\link{dataCovidBeds20200624}}
#'
#' @return list with simData and fieldData 
#' 
#' @examples 
#' 
#' data <- getObkData() 
#' 
#' @export


getObkData <- function(data = babsim.hospital::dataCovidBeds20200624){

simData <- data.frame(Day = data$Day,
                      Infected = data$Infected)
attr(simData,"StartDate") <- min(simData$Day)
attr(simData,"EndDate") <- max(simData$Day)
attr(simData,"Days") <- as.integer(1+ max(simData$Day) - min(simData$Day))
attr(simData,"FeatureNames") <- c("Infected")
fieldData <- data.frame(Day = data$Day,
                        bed = data$bed,
                        intensiveBed = data$intensiveBed,
                        intensiveBedVentilation = data$intensiveBedVentilation)
attr(fieldData,"StartDate") <- min(fieldData$Day)
attr(fieldData,"EndDate") <- max(fieldData$Day)
attr(fieldData,"Days") <- as.integer(1+ max(fieldData$Day) - min(fieldData$Day))
attr(fieldData, "ResourceNames") <- c("bed", "intensiveBed", "intensiveBedVentilation")

return(list(simData = simData,
             fieldData = fieldData))
}

  

