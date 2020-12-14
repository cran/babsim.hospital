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
#' err <- funOptimizeSim(x=x, conf=conf, data=data)
#' 
#' @export
  funOptimizeSim <- function(x, conf, data, ...){
  para <- mapXToPara(x) 
  res <- modelResultHospital(para=para, conf = conf, data = data)
  err <- getError(res = res, conf = conf)
  
  if(conf$verbosity > 10){print(paste0("Err:", err))}
  return(err)
}



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
#' funWrapOptimizeSim(x=para, conf=conf, data=data)
#' 
#' @export
funWrapOptimizeSim <- function (x, 
                                conf, 
                                data){
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               funOptimizeSim,
               conf, data))
}



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
#' @param data list with simData and fieldData       
#'       
#' @importFrom stats xtabs
#' 
#' @return This function returns an env data frame (tibble [560 × 15] (S3: grouped_df/tbl_df/tbl/data.frame))
#' with the following entries:
#' \describe{
#'		\item{\code{resource} (chr)}{name of the seized resource: "bed" "bed" "bed" "bed" ...}
#'		\item{\code{time} (num)}{time step:  3 10 12 13 14 15 15 15 15 16 ...}
#'		\item{\code{server} (int)}{server: 1 2 3 2 3 4 3 4 5 6 ...}
#'		\item{\code{queue} (int)}{1 1 2 3 4 3 4 1 0 2 ...}
#'		\item{\code{capacity} (num)}{10000 10000 10000 10000 10000 10000 10000 10000 10000 10000 ...}
#'		\item{\code{queue_size} (num)}{Inf Inf Inf Inf Inf ...}
#'		\item{\code{system} (int)}{1 1 2 3 4 3 4 1 0 2 ...}		
#'		\item{\code{limit} (num)}{limit: Inf Inf Inf Inf Inf ...}
#'		\item{\code{replication} (int)}{replication:  1 1 1 1 1 1 1 1 1 1 ...}
#'		\item{\code{upper} (int)}{upper: 1 2 3 2 3 5 5 5 5 7 ...}
#'		\item{\code{lower} (int)}{lower: 1 2 3 2 3 3 3 3 3 5 ...}
#'		\item{\code{med} (num)}{med: 1 2 3 2 3 4 4 4 4 6 ...}
#'		\item{\code{date} (POSIXct)}{time, format: \code{yyyy-mm-dd hh:mm.ss}}
#'		\item{\code{rwdate} (POSIXct)}{format: "2020-03-01" "2020-03-08" "2020-03-15" "2020-03-15" ...}
#'		\item{\code{source} (chr)}{name of the simulation that was used: "babsim" "babsim" "babsim" "babsim" ...}
#'		}
#' @examples 
#' # First example: OBK data
#' data <- getObkData()
#' para <- babsimHospitalPara()
#' para$GammaShapeParameter = 0.8
#' # turn off parallelized simulation:
#' conf <- babsimToolsConf()
#' conf <- getConfFromData(conf = conf,
#'                         simData = data$simData,
#'                         fieldData = data$fieldData) 
#' # no logging (default)
#' conf$logLevel = 0
#' res <- modelResultHospital(para=para, 
#'                            conf=conf, 
#'                            data = data)
#' 
#'  # Second example: synthetic data
#' data <- getSyntheticData() 
#' para <- babsimHospitalPara()
#' conf <- babsimToolsConf()
#' conf <- getConfFromData(conf = conf,
#'                         simData = data$simData,
#'                         fieldData = data$fieldData) 
#' res <- modelResultHospital(para=para, conf=conf, data = data)
#' 
#' @export

modelResultHospital <- function(para, 
                                conf,
                                data){
  set.seed(conf$seed)
  para <- checkSimPara(para)
  if (conf$ICU){
    rkiWithRisk <- getRkiRisk(data$simData, para)
    arrivalTimes <- data.frame(time = rkiWithRisk$time,
                               risk = rkiWithRisk$Risk)
  } else{
    arrivalTimes <- getArrivalTimes(data$simData$Infected)
  }
  con <- list(arrivalTimes=arrivalTimes)
  con[names(data)] <- data
  data <- con
  
  if (conf$verbosity > 1000){
    print("BEGIN: modelResultHospital() calling babsimHospital: ###########################")
    printConf(conf)
    print("END: modelResultHospital()  ###########################")
  }
  
  envs <- babsimHospital(arrivalTimes = data$arrivalTimes,
                         conf = conf,
                         para = para)
  fieldEvents <- getRealBeds(data = data$fieldData,
                             resource= conf$ResourceNames)
  return( getDailyMaxResults(envs = envs,  
                             fieldEvents = fieldEvents,
                             conf = conf) )
}


#' @title  getTrainTestObjFun
#'
#' @description Generate objective functions (one for train, optionally one for test)
#'
#' @param rkiwerte RKI Daten
#' @param icuwerte ICU Daten
#' @param region Landkreis Id, e.g., \code{5374} fuer OBK, \code{5315} fuer Koeln,
#' \code{0} fuer Deutschland,
#' oder Bundesland ID, e.g., \code{5} fuer NRW.
#' @param TrainFieldStartDate Start (Tag), e.g., \code{"2020-06-01"}
#' @param TrainSimStartDate Start (Tag), e.g., \code{"2020-05-01"}
#' @param TestFieldStartDate Start (Day), e.g., \code{"2020-06-01"} for test field data
#' @param TestSimStartDate Start (Day), e.g., \code{"2020-05-01"} for test simulation data,
#' TestSimStartDate is usually before TestFieldStartDate
#' @param parallel logical
#' @param percCores percentage
#' @param icu ICU Daten
#' @param icuWeights Gewichtung der ICU Betten
#' @param resourceNames Name der Ressourcen
#' @param resourceEval Name der zu evaluierenden Ressourcen
#' @param verbosity verbosity (int). Default: \code{0} 
#' @param tryOnTestSet Should results be tested on a separate test set?
#' 
#' 
#' @export
getTrainTestObjFun <- function(
    rkiwerte = babsim.hospital::rkidata,
    icuwerte = babsim.hospital::icudata,
    region = 5374,
    TrainSimStartDate = Sys.Date() - 12*7, 
    TrainFieldStartDate = Sys.Date() - 8*7, 
    TestSimStartDate = Sys.Date() - 8*7 , 
    TestFieldStartDate = Sys.Date() - 4*7, 
    verbosity = 0,
    parallel = FALSE,
    percCores = NULL, 
    icu = TRUE,
    icuWeights = c(1,1),
    resourceNames =  c("intensiveBed", "intensiveBedVentilation"),
    resourceEval = c("intensiveBed", "intensiveBedVentilation"),
    tryOnTestSet = TRUE){
    
    print(c(min(rkiwerte$Refdatum), max(rkiwerte$Refdatum)))    
    print(c(min(icuwerte$daten_stand), max(icuwerte$daten_stand)))    
    
    result.df <- data.frame(x = NULL, y = NULL)
    reslist <- list()
    
    ## General data range adaptation for train and test data:
    ##  Field data = Realdata, ICU, beds:
    regionIcuwerte <- getRegionIcu(data = icuwerte,
                                   region = region)
    fieldData <- getIcuBeds(regionIcuwerte)
    fieldData <-
        fieldData[which(fieldData$Day >= as.Date(TrainFieldStartDate)), ]
    
    ## Sim data = RKI, infections:
    regionRkiwerte <- getRegionRki(data = rkiwerte,
                                   region = region)
    simData <- getRkiData(regionRkiwerte)
    simData <- simData[which(simData$Day >= as.Date(TrainSimStartDate)), ]
    
    ## v 10.3.4: TrainSimStartDate must be corrected if it is not in the RKI data.
    ## This happens, e.g., if no data are transfered on Sundays:.
    TrainSimStartDate <- min(simData$Day)
    
    EndDate <- min( max(as.Date(simData$Day)), 
                    max(as.Date(fieldData$Day)) )
    fieldData <-
        fieldData[which(fieldData$Day <= EndDate), ]
    simData <-
        simData[which(simData$Day <= EndDate), ]
    simData$time <- simData$time - min(simData$time)
    rownames(fieldData) <- NULL
    rownames(simData) <- NULL
    ##
    
    if(tryOnTestSet){
        TrainEndDate <- as.Date(TestFieldStartDate)
    }else{
        TrainEndDate <- as.Date(EndDate)
    }
    
    TrainSimData <- simData[which(simData$Day <= TrainEndDate), ]
    TrainFieldData <- fieldData[which(fieldData$Day <= TrainEndDate), ]  
    ## v10.3.4: end days synchronized:
    syncedEndTrainDate <- min( max(TrainSimData$Day), max(TrainFieldData$Day) )
    
    TrainSimData <- simData[which(simData$Day <= syncedEndTrainDate), ]
    TrainFieldData <- fieldData[which(fieldData$Day <= syncedEndTrainDate), ]  
    
    trainData <- list(simData = TrainSimData,
                      fieldData = TrainFieldData)
    
    ## Check Train Data:
    print( paste("trainDataSim: ", c(min(trainData$simData$Day), max(trainData$simData$Day))))
    print( paste("trainDataField: ", c(min(trainData$fieldData$Day), max(trainData$fieldData$Day))))
    SIM_EQ_FIELD_TRAINDATA <- 
        (min(as.Date(trainData$simData$Day)) == as.Date(TrainSimStartDate)) 
    if(SIM_EQ_FIELD_TRAINDATA == FALSE){print(as.Date(TrainSimStartDate))
        stop("babsim.hospital::runoptDirect: Check TrainSimStartDate.")}
    
    SIM_EQ_FIELD_TRAINDATA <- 
        (min(as.Date(trainData$fieldData$Day)) == as.Date(TrainFieldStartDate)) 
    if(SIM_EQ_FIELD_TRAINDATA == FALSE){stop("babsim.hospital::runoptDirect: Check TrainFieldStartDate.")}
    
    SIM_EQ_FIELD_TRAINDATA <- 
        (max(as.Date(trainData$simData$Day)) == max(as.Date(trainData$fieldData$Day)))
    if(SIM_EQ_FIELD_TRAINDATA == FALSE){stop("babsim.hospital::runoptDirect: Check trainData:
                                             sim and field End data do not agree.")}
    
    
    conf <- babsimToolsConf()
    trainConf <- getConfFromData(simData = trainData$simData,
                                 fieldData = trainData$fieldData,
                                 conf = conf)
    trainConf$seed = NULL
    trainConf$verbosity = verbosity
    trainConf$parallel = parallel
    trainConf$ICU = icu
    trainConf$ResourceNames = resourceNames
    trainConf$ResourceEval = resourceEval
    trainConf$percCores = percCores
    trainConf$logLevel = 1
    trainConf$w2 = icuWeights
    ## Check train config:
    print( paste("trainConfSim: ", c(trainConf$simulationDates$StartDate, trainConf$simulationDates$EndDate)))
    print( paste("trainConfField: ", c(trainConf$fieldDates$StartDate, trainConf$fieldDates$EndDate)))
    SIM_EQ_FIELD_TRAINCONF <- 
        (min(as.Date(trainConf$simulationDates$StartDate)) == as.Date(TrainSimStartDate)) &
        (min(as.Date(trainConf$fieldDates$StartDate)) == as.Date(TrainFieldStartDate)) &
        (max(as.Date(trainConf$simulationDates$EndDate)) == max(as.Date(trainConf$fieldDates$EndDate)))
    if(SIM_EQ_FIELD_TRAINCONF == FALSE){stop("babsim.hospital::runoptDirect: Check trainConf.")}
    bounds <- getBounds()
    a <- bounds$lower
    b <- bounds$upper
    conf <- trainConf
    if (conf$verbosity > 1000){
        print("conf before spot optimization is started")
        printConf(conf)
    }
    data <- trainData
    
    trainFun <- function(x){
        funOptimizeSim(x, trainConf, trainData)
    } 
    
    if(tryOnTestSet){
        
        ## Generate test data for final evaluation of one optimization run:
        testFieldData <-
            fieldData[which(fieldData$Day >= as.Date(TestFieldStartDate)), ]
        rownames(testFieldData) <- NULL
        
        ## testSimData
        testSimData <- 
            simData[which(simData$Day >= as.Date(TestSimStartDate)), ]
        
        ## v 10.3.6: 
        TestSimStartDate <- min(testSimData$Day)
        
        testSimData <-
            testSimData[as.Date(testSimData$Day) <= max(as.Date(testFieldData$Day)),]
        testFieldData <-
            testFieldData[as.Date(testFieldData$Day) <= max(as.Date(testSimData$Day)),]
        
        ## v10.3.6: end days synchronized:
        syncedEndTestDate <- min( max(testSimData$Day), max(testFieldData$Day) )
        testSimData <- testSimData[which(testSimData$Day <= syncedEndTestDate), ]
        TestFieldData <- fieldData[which(fieldData$Day <= syncedEndTestDate), ]  
        ###
        
        testSimData$time <- testSimData$time - min(testSimData$time)
        rownames(testSimData) <- NULL
        
        ## Combine data
        testData <- list(simData = testSimData,
                         fieldData = testFieldData)
        
        ## Check test data:
        print( paste("testDataSim: ", c(min(testData$simData$Day), max(testData$simData$Day))))
        print( paste("testDataField: ", c(min(testData$fieldData$Day), max(testData$fieldData$Day))))
        SIM_EQ_FIELD_TESTDATA <- 
            (min(as.Date(testData$simData$Day)) == as.Date(TestSimStartDate)) 
        
        if(SIM_EQ_FIELD_TESTDATA == FALSE){print(as.Date(TestSimStartDate))
            stop("babsim.hospital::runoptDirect: Check testData: 
                 TestSimStartDate.")}
        
        
        SIM_EQ_FIELD_TESTDATA <- 
            (min(as.Date(testData$fieldData$Day)) == as.Date(TestFieldStartDate)) 
        if(SIM_EQ_FIELD_TESTDATA == FALSE){
            print(as.Date(TestFieldStartDate))
            stop("babsim.hospital::runoptDirect: Check testData: TestFieldStartDate.")}
        SIM_EQ_FIELD_TESTDATA <- 
            (max(as.Date(testData$simData$Day)) == max(as.Date(testData$fieldData$Day)))
        if(SIM_EQ_FIELD_TESTDATA == FALSE){stop("babsim.hospital::runoptDirect: Check testData:
                                                sim and field data End date differ!")}
        conf <- babsimToolsConf()
        testConf <- getConfFromData(simData = testSimData,
                                    fieldData = testFieldData,
                                    conf = conf)
        testConf$verbosity = verbosity
        testConf$parallel = parallel
        testConf$ICU = icu
        testConf$ResourceNames = resourceNames
        testConf$ResourceEval = resourceEval
        testConf$percCores = percCores
        testConf$logLevel = 1
        testConf$w2 = icuWeights
        testConf$seed = NULL
        
        ## Check test config:
        print( paste("testConfSim: ", c(testConf$simulationDates$StartDate, testConf$simulationDates$EndDate)))
        print( paste("testConfField: ", c(testConf$fieldDates$StartDate, testConf$fieldDates$EndDate)))
        SIM_EQ_FIELD_TESTCONF <- ( (testConf$simulationDates$StartDate != testConf$fieldDates$StartDate) &
                                       (testConf$simulationDates$EndDate != testConf$fieldDates$EndDate))
        SIM_EQ_FIELD_TESTCONF <- 
            (min(as.Date(testConf$simulationDates$StartDate)) == as.Date(TestSimStartDate)) &
            (min(as.Date(testConf$fieldDates$StartDate)) == as.Date(TestFieldStartDate)) &
            (max(as.Date(testConf$simulationDates$EndDate)) == max(as.Date(testConf$fieldDates$EndDate)))
        if(SIM_EQ_FIELD_TESTCONF == FALSE){stop("babsim.hospital::runoptDirect: Check testConf.")}
        
        testFun <- function(x){
            funOptimizeSim(x, testConf, testData)
        } 
        return(list(trainFun = trainFun, testFun = testFun))
        }
    
    return(trainFun)
}
