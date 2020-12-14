#' @title babsimToolsConf 
#' 
#' @description Default configuration list for babsimTools. 
#' This function returns the default configuration settings of the babsimTools functions
#' used to run \code{\link{babsimHospital}}.
#' Configuration \code{conf}  is a list of the following settings.
#' \describe{
#'		\item{\code{seed}}{(int) Initial seed. Default: 123}
#'		\item{\code{simRepeats}}{(int) Number of \code{\link[simmer]{simmer}} simulation runs. Default: 1}
#'		\item{\code{parallel}}{(logical) Use parallel simulations based on \code{\link[parallel]{mclapply}}. Default: FALSE}
#'		\item{\code{perCores}}{(num) Percentage of cores used, if \code{parallel == TRUE}. Default: 0.5}
#'		\item{\code{ICU}}{(logical) Use ICU (RKI) data. Default: FALSE.}
#'		\item{\code{logLevel}}{(int) 0 = no logging, >= 1 logging. Default: 0. If larger than 10, shown detailed simmer output.}
#'		\item{\code{maxCapacity}}{(num) Maximum capacity used for \code{\link{babsimHospital}} resources. Default: 1e6.}
#'		\item{\code{dataset}}{(chr)  "GA" or "ICU". Default: "GA".}
#'		\item{\code{simulationDates}}{ List with the following entries:
#'		  \describe{
#'		  \item{\code{StartDate}}{(chr) Start date of the simulation data (infection data used to generate 
#'		  arrival times), first day. Default: "2020-03-03"}
#'		  \item{\code{EndDate}}{(chr) End date of the simulation data, last day. Default: "2020-06-24"}
#'		}}
#'		\item{\code{fieldDates}}{ List with the following entries:
#'		  \describe{
#'		  \item{\code{StartDate}}{(chr) Start date of the field (resources)  data, first day. Default: "2020-03-03"}
#'		  \item{\code{EndDate}}{(chr) End date of the field data, last day. Default: "2020-06-24"}
#'		}}
#'	 \item{\code{simulationData}}{(data frame) Data used for the simulation. Default \code{\link{dataCovidBeds20200624}}
#'		\describe{
#'   \item{bed}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{intensiveBed}{int  0 0 0 0 0 0 0 0 0 0 ...}
#'   \item{intensiveBedVentilation}{int  0 0 0 0 0 0 0 0 0 0 ...}
#'   \item{Day}{Date, format: "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" ...}
#'   \item{Infected}{num  5 0 0 0 0 0 0 7 2 5 ...}
#'   \item{Sick}{num  5 5 5 5 5 5 5 12 14 19 ...}
#'    }} 
#'		\item{\code{fieldEvents}}{(data frame) Data used for evalution of the simulation. Default \code{\link{GABeds220200624}}
#'	 \describe{
#'   \item{ressource}{chr  "bed" "bed" "bed" "bed" ...}
#'   \item{time}{int  1 2 3 4 5 6 7 8 9 10 ...}
#'   \item{med}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{source}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{date}{Date, format: "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" ...}
#'    }}
#'		\item{\code{resource}}{(vector) Resources used in the simulation. 
#'		Default: \code{c("bed", "intensiveBed", "intensiveBedVentilation")}. For ICU data use:
#'		\code{c("bed", "intensiveBedVentilation")}
#'	 \describe{
#'   \item{ressource}{chr  "bed" "bed" "bed" "bed" ...}
#'   \item{time}{int  1 2 3 4 5 6 7 8 9 10 ...}
#'   \item{med}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{source}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{date}{Date, format: "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" ...}
#'    }}    
#'	}
#' @return a list
#' 
#' @examples 
#' 
#' # turn on parallel simulation:
#' conf <- babsimToolsConf()
#' conf$parallel <- TRUE
#' conf$simulationDates$StartDate = "2020-01-01"
#' 
#' @export
babsimToolsConf <- function() {
  conf = list(
    seed = 123,
    simRepeats = 1,
    parallel = FALSE,
    percCores = 0.5,
    ICU = FALSE,
    logLevel = 0,
    maxCapacity = 1e6,
    dataset = "GA",
    simulationDates = list(StartDate = "",
                          EndDate = ""),
    fieldDates = list(StartDate = "", 
                      EndDate = ""),
    ResourceNames = c("bed", "intensiveBed", "intensiveBedVentilation"),
    # Resources used in the error function:
    ResourceEval = c("intensiveBed", "intensiveBedVentilation"),
    w2 = c(1,10),
    verbosity = 0
  )
}


#' @title getConfFromData
#' 
#' @description  Generate a configuration list for babsimTools based on the data file and the
#' vector of resources.
#' This function returns the default configuration settings of the babsimTools functions
#' used to run \code{\link{babsimHospital}}.
#' 
#' @details  Configuration \code{conf}  is a list of the following settings.
#' \describe{
#'		\item{\code{seed}}{(int) Initial seed. Default: 123}
#'		\item{\code{simRepeats}}{(int) Number of \code{\link[simmer]{simmer}} simulation runs. Default: 1}
#'		\item{\code{parallel}}{(logical) Use parallel simulations based on \code{\link[parallel]{mclapply}}. Default: FALSE}
#'		\item{\code{perCores}}{(num) Percentage of cores used, if \code{parallel == TRUE}. Default: 0.5}
#'		\item{\code{ICU}}{(logical) Use ICU (RKI) data. Default: FALSE.}
#'		\item{\code{logLevel}}{(int) 0 = no logging, >= 1 logging. Default: 0. If larger than 10, shown detailed simmer output.}
#'		\item{\code{maxCapacity}}{(num) Maximum capacity used for \code{\link{babsimHospital}} resources. Default: 1e6.}
#'		\item{\code{dataset}}{(chr)  "GA" or "ICU". Default: "GA".}
#'		\item{\code{simulationDates}}{ List with the following entries:
#'		  \describe{
#'		  \item{\code{StartDate}}{(chr) Start date of the simulation data (infection data used to generate 
#'		  arrival times), first day. Default: "2020-03-03"}
#'		  \item{\code{EndDate}}{(chr) End date of the simulation data, last day. Default: "2020-06-24"}
#'		}}
#'		\item{\code{fieldDates}}{ List with the following entries:
#'		  \describe{
#'		  \item{\code{StartDate}}{(chr) Start date of the field (resources)  data, first day. Default: "2020-03-03"}
#'		  \item{\code{EndDate}}{(chr) End date of the field data, last day. Default: "2020-06-24"}
#'		}}
#'	 \item{\code{simulationData}}{(data frame) Data used for the simulation. Default \code{\link{dataCovidBeds20200624}}
#'		\describe{
#'   \item{bed}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{intensiveBed}{int  0 0 0 0 0 0 0 0 0 0 ...}
#'   \item{intensiveBedVentilation}{int  0 0 0 0 0 0 0 0 0 0 ...}
#'   \item{Day}{Date, format: "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" ...}
#'   \item{Infected}{num  5 0 0 0 0 0 0 7 2 5 ...}
#'   \item{Sick}{num  5 5 5 5 5 5 5 12 14 19 ...}
#'    }} 
#'		\item{\code{fieldEvents}}{(data frame) Data used for evalution of the simulation. Default \code{\link{GABeds220200624}}
#'	 \describe{
#'   \item{ressource}{chr  "bed" "bed" "bed" "bed" ...}
#'   \item{time}{int  1 2 3 4 5 6 7 8 9 10 ...}
#'   \item{med}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{source}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{date}{Date, format: "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" ...}
#'    }}
#'		\item{\code{resource}}{(vector) Resources used in the simulation. 
#'		Default: \code{c("bed", "intensiveBed", "intensiveBedVentilation")}. For ICU data use:
#'		\code{c("bed", "intensiveBedVentilation")}
#'	 \describe{
#'   \item{ressource}{chr  "bed" "bed" "bed" "bed" ...}
#'   \item{time}{int  1 2 3 4 5 6 7 8 9 10 ...}
#'   \item{med}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{source}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{date}{Date, format: "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" ...}
#'    }}    
#'	}
#'	
#' @param conf Configuration. Default: \code{\link{babsimToolsConf}}
#' @param simData Simulation data. Default: \code{\link{dataCovidBeds20200624}}
#' @param fieldData Field (real) data. Default: \code{\link{dataCovidBeds20200624}}
#' @return a list
#' 
#' @examples 
#' 
#' conf <- babsimToolsConf()
#' simData  <- babsim.hospital::dataCovidBeds20200624
#' fieldData <- babsim.hospital::dataCovidBeds20200624
#' conf <- getConfFromData(conf = conf,
#'                         simData = simData,
#'                         fieldData = fieldData)
#' # turn on parallel simulation:
#' conf$parallel <- TRUE
#' # Change the start date of the simulations
#' conf$simulationDates$StartDate = "2020-01-01"
#' 
#' @export
#'
getConfFromData <- function(conf,
                            simData,
                            fieldData){
  conf$simulationDates = list(StartDate = min(simData$Day),
                           EndDate =  max(simData$Day))
  conf$fieldDates = list(StartDate = min(fieldData$Day), 
                      EndDate = max(fieldData$Day))
  conf$start <- max(conf$simulationDates$StartDate,
                    conf$fieldDates$StartDate)
  conf$end <- min(conf$simulationDates$EndDate,
                    conf$fieldDates$EndDate)
  return(conf)
}


#' @title printConf
#' 
#' @description  Print configuration information, e.g., for debugging.
#' This function returns the configuration settings of the babsim.hospital functions
#' used to run \code{\link{babsimHospital}}.
#' Currently, only \code{\link[utils]{str}} is used. 
#' 
#' @details  Configuration \code{conf}  is a list of the following settings.
#' \describe{
#'		\item{\code{seed}}{(int) Initial seed. Default: 123}
#'		\item{\code{simRepeats}}{(int) Number of \code{\link[simmer]{simmer}} simulation runs. Default: 1}
#'		\item{\code{parallel}}{(logical) Use parallel simulations based on \code{\link[parallel]{mclapply}}. Default: FALSE}
#'		\item{\code{perCores}}{(num) Percentage of cores used, if \code{parallel == TRUE}. Default: 0.5}
#'		\item{\code{ICU}}{(logical) Use ICU (RKI) data. Default: FALSE.}
#'		\item{\code{logLevel}}{(int) 0 = no logging, >= 1 logging. Default: 0. If larger than 10, shown detailed simmer output.}
#'		\item{\code{maxCapacity}}{(num) Maximum capacity used for \code{\link{babsimHospital}} resources. Default: 1e6.}
#'		\item{\code{dataset}}{(chr)  "GA" or "ICU". Default: "GA".}
#'		\item{\code{simulationDates}}{ List with the following entries:
#'		  \describe{
#'		  \item{\code{StartDate}}{(chr) Start date of the simulation data (infection data used to generate 
#'		  arrival times), first day. Default: "2020-03-03"}
#'		  \item{\code{EndDate}}{(chr) End date of the simulation data, last day. Default: "2020-06-24"}
#'		}}
#'		\item{\code{fieldDates}}{ List with the following entries:
#'		  \describe{
#'		  \item{\code{StartDate}}{(chr) Start date of the field (resources)  data, first day. Default: "2020-03-03"}
#'		  \item{\code{EndDate}}{(chr) End date of the field data, last day. Default: "2020-06-24"}
#'		}}
#'	 \item{\code{simulationData}}{(data frame) Data used for the simulation. Default \code{\link{dataCovidBeds20200624}}
#'		\describe{
#'   \item{bed}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{intensiveBed}{int  0 0 0 0 0 0 0 0 0 0 ...}
#'   \item{intensiveBedVentilation}{int  0 0 0 0 0 0 0 0 0 0 ...}
#'   \item{Day}{Date, format: "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" ...}
#'   \item{Infected}{num  5 0 0 0 0 0 0 7 2 5 ...}
#'   \item{Sick}{num  5 5 5 5 5 5 5 12 14 19 ...}
#'    }} 
#'		\item{\code{fieldEvents}}{(data frame) Data used for evalution of the simulation. Default \code{\link{GABeds220200624}}
#'	 \describe{
#'   \item{ressource}{chr  "bed" "bed" "bed" "bed" ...}
#'   \item{time}{int  1 2 3 4 5 6 7 8 9 10 ...}
#'   \item{med}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{source}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{date}{Date, format: "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" ...}
#'    }}
#'		\item{\code{resource}}{(vector) Resources used in the simulation. 
#'		Default: \code{c("bed", "intensiveBed", "intensiveBedVentilation")}. For ICU data use:
#'		\code{c("bed", "intensiveBedVentilation")}
#'	 \describe{
#'   \item{ressource}{chr  "bed" "bed" "bed" "bed" ...}
#'   \item{time}{int  1 2 3 4 5 6 7 8 9 10 ...}
#'   \item{med}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{source}{int  2 2 3 3 3 3 3 3 4 4 ...}
#'   \item{date}{Date, format: "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" ...}
#'    }}    
#'	}
#'	
#' @param conf Configuration, e.g., generated with \code{\link{babsimToolsConf}}.
#' 
#' @importFrom utils str 
#' 
#' @return conf elements
#' 
#' @examples 
#' 
#' conf = babsimToolsConf()
#' # turn on parallel simulation:
#' conf$parallel <- TRUE
#' # Change the start date of the simulations
#' printConf(conf=conf)
#' 
#' @export
#'
printConf <- function(conf){
   str(conf)
}
