#' @title  runoptUK Optimierung der babsim.hospital Parameter
#'
#' @description SPOT Aufruf zur Optimierung der babsim Parameter 
#'
#' @param expName Experiment Name
#' @param simData RKI Daten
#' @param fieldData ICU Daten
#' @param TrainFieldStartDate Start (Tag), e.g., \code{"2020-06-01"}
#' @param TrainSimStartDate Start (Tag), e.g., \code{"2020-05-01"}
#' @param TestFieldStartDate Start (Day), e.g., \code{"2020-06-01"} for test field data
#' @param TestSimStartDate Start (Day), e.g., \code{"2020-05-01"} for test simulation data,
#' TestSimStartDate is usually before TestFieldStartDate
#' @param Overlap integer. Days, train data will be extended (overlap with test data). Default: 7
#' @param seed Seed
#' @param repeats Wiederholungen fuer SPOT (Optimierungslaeufe mit unterschiedlichem Seed)
#' @param funEvals Auswertungen fuer SPOT (Simulationen, 
#' die fuer einen SPOT Lauf zur Verfuegung stehen)
#' @param size Groesse des initialen Desings
#' @param simrepeats Sim Wdhlg
#' @param subset Subset (SPOT)
#' @param parallel logical
#' @param percCores percentage
#' @param icu ICU Daten
#' @param icuWeights Gewichtung der ICU Betten
#' @param resourceNames Name der Ressourcen
#' @param resourceEval Name der zu evaluierenden Ressourcen
#' @param verbosity verbosity (int). Default: \code{0} 
#' @param testRepeats number of final evaluations on the test data
#' @param factorUK factor to adapt the percentage of patients ventilated. For example,
#' if set to `0.5`, only 50 percent of the default number of patients will be ventilated.
#' Default: 1 
#' @param factorIcuUK factor to adapt the duration on ICU. For example,
#' if set to `0.5`, patients stay on half the time at ICU
#' Default: 1 
#' @importFrom SPOT spot
#' @importFrom SPOT optimNLOPTR
#' @importFrom SPOT buildLasso
#' @importFrom SPOT buildKriging
#' @importFrom SPOT selectAll
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' res <- runoptUK()
#' }
#' 
#' @export
#' 
#' 
runoptUK <- function(expName = "ukl001",
                     simData = simData,
                     fieldData = fieldData,
                     TrainFieldStartDate = Sys.Date() - 6*7,
                     TrainSimStartDate = Sys.Date() - 10*7,
                     TestFieldStartDate = Sys.Date() - 4*7,
                     TestSimStartDate = Sys.Date() - 8*7,
                     Overlap = 0,
                     verbosity = 0,
                     seed = 123,
                     repeats = 1,
                     funEvals = 35,
                     size = 30,
                     simrepeats = 2,
                     subset = 32,
                     parallel = FALSE,
                     percCores = 0.8,
                     icu = FALSE,
                     icuWeights = c(1,1,1),
                     testRepeats = 3,
                     resourceNames =  c("bed", "intensiveBed", "intensiveBedVentilation"),
                     resourceEval = c("bed", "intensiveBed", "intensiveBedVentilation"),
                     factorUK = 1,
                     factorIcuUK = 1){
  
  result.df <- data.frame(x = NULL, y = NULL)
  reslist <- list()
  
  fieldData <-
    fieldData[which(fieldData$Day >= as.Date(TrainFieldStartDate)), ]
  
  ## Sim data = RKI, infections:
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
  
  #### 20200125 TBB: UK uses no time information:
  #### simData$time <- simData$time - min(simData$time)
  rownames(fieldData) <- NULL
  rownames(simData) <- NULL
  ##
  
  TrainEndDate <- as.Date(TestFieldStartDate) + Overlap
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
  
  
  FILENAME <-  paste0(expName, ".RData")
  ### Entering optimization:
  if (verbosity > 1){
  print("Starting optimization loop:")
  print("#########################################")
  }
  for (i in 1:repeats) {
    if (verbosity > 1){
    print(paste0("Repeat: ", i, " ###############################"))
    }
    conf <- babsimToolsConf()
    trainConf <- getConfFromData(simData = trainData$simData,
                                 fieldData = trainData$fieldData,
                                 conf = conf)
    trainConf$verbosity = verbosity
    trainConf$parallel = parallel
    trainConf$simRepeats = simrepeats
    trainConf$ICU = icu
    trainConf$ResourceNames = resourceNames
    trainConf$ResourceEval = resourceEval
    trainConf$percCores = percCores
    trainConf$logLevel = 0
    trainConf$w2 = icuWeights
    trainConf$seed <- seed + i
    ## Check train config:
    if (conf$verbosity > 10){
    print( paste("trainConfSim: ", c(trainConf$simulationDates$StartDate, trainConf$simulationDates$EndDate)))
    print( paste("trainConfField: ", c(trainConf$fieldDates$StartDate, trainConf$fieldDates$EndDate)))
    }
    SIM_EQ_FIELD_TRAINCONF <- 
      (min(as.Date(trainConf$simulationDates$StartDate)) == as.Date(TrainSimStartDate)) &
      (min(as.Date(trainConf$fieldDates$StartDate)) == as.Date(TrainFieldStartDate)) &
      (max(as.Date(trainConf$simulationDates$EndDate)) == max(as.Date(trainConf$fieldDates$EndDate)))
    if(SIM_EQ_FIELD_TRAINCONF == FALSE){stop("babsim.hospital::runoptDirect: Check trainConf.")}
    set.seed(trainConf$seed)
    x0 <- getStartParameter()
    bounds <- getBounds()
    a <- bounds$lower
    b <- bounds$upper
    a[16] <- a[16] * factorUK
    a[18] <- a[18] * factorUK
    a[20] <- a[20] * factorUK 
    b[16] <- b[16] * factorUK
    b[18] <- b[18] * factorUK
    b[20] <- b[20] * factorUK
    a[6] <- a[6] * factorIcuUK
    a[7] <- a[7] * factorIcuUK
    a[8] <- a[8] * factorIcuUK 
    b[6] <- b[6] * factorIcuUK
    b[7] <- b[7] * factorIcuUK
    b[8] <- b[8] * factorIcuUK
    conf <- trainConf
    if (conf$verbosity > 1000){
      print("conf before spot optimization is started")
      printConf(conf)
    }
    data <- trainData
    g <- function(x) {
      return(
        rbind(
          a[1] - x[1],
          x[1] - b[1],
          a[2] - x[2],
          x[2] - b[2],
          a[3] - x[3],
          x[3] - b[3],
          a[4] - x[4],
          x[4] - b[4],
          a[5] - x[5],
          x[5] - b[5],
          a[6] - x[6],
          x[6] - b[6],
          a[7] - x[7],
          x[7] - b[7],
          a[8] - x[8],
          x[8] - b[8],
          a[9] - x[9],
          x[9] - b[9],
          a[10] - x[10],
          x[10] - b[10],
          a[11] - x[11],
          x[11] - b[11],
          a[12] - x[12],
          x[12] - b[12],
          a[13] - x[13],
          x[13] - b[13],
          a[14] - x[14],
          x[14] - b[14],
          a[15] - x[15],
          x[15] - b[15],
          a[16] - x[16],
          x[16] - b[16],
          a[17] - x[17],
          x[17] - b[17],
          a[18] - x[18],
          x[18] - b[18],
          a[19] - x[19],
          x[19] - b[19],
          a[20] - x[20],
          x[20] - b[20],
          a[21] - x[21],
          x[21] - b[21],
          a[22] - x[22],
          x[22] - b[22],
          a[23] - x[23],
          x[23] - b[23],
          a[24] - x[24],
          x[24] - b[24],
          a[25] - x[25],
          x[25] - b[25],
          a[26] - x[26],
          x[26] - b[26],
          a[27] - x[27],
          x[27] - b[27],
          x[15] + x[16] - 1,
          x[17] + x[18] + x[19] - 1,
          x[20] + x[21] - 1,
          x[23] + x[29] - 1
        )
      )
    }
    assign(
      expName,
      spot(
        x = x0,
        fun = funWrapOptimizeSim,
        lower = a,
        upper = b,
        control = list(
          funEvals = funEvals,
          noise = TRUE,
          designControl = list(
            # inequalityConstraint = g,
            size = size,
            retries = 1000),
          optimizer = optimNLOPTR,
          optimizerControl = list(
            ## The following global NLOPT algorithm is the only one supporting constraints
            opts = list(algorithm = "NLOPT_GN_ISRES"),
            eval_g_ineq = g
          ),
          model =  buildKriging,
          plots = FALSE,
          progress = TRUE,
          directOpt = optimNLOPTR,
          directOptControl = list(funEvals = funEvals),
          eval_g_ineq = g
        ),
        conf,
        data 
      )
    )
    # save(list = eval(parse(text = "expName")) , file = FILENAME)
    ## result from one optimization run on train data:
    res <- get(expName)
    ## add one result to list
    reslist[[length(reslist)+1]] <- res
    ## take parameter from one run on train data
    x <- as.matrix(res$xbest, 1, )
    
    if (conf$verbosity > 1){
    print(paste0("xbest: ", x))
    ### Entering test:
    print("Starting test evaluation:")
    print("#########################################")
    }
    
    ## Parameters for the test evaluation
    testPara <- mapXToPara(x)
    testPara <- checkSimPara(testPara)
    
    ## Generate test data for final evaluation of one optimization run:
    ## testFieldData
    ## testFieldData <-  getIcuBeds(regionIcuwerte)
    testFieldData <-
      fieldData[which(fieldData$Day >= as.Date(TestFieldStartDate)), ]
    rownames(testFieldData) <- NULL
    
    ## testSimData
    ## testSimData <- getRkiData(regionRkiwerte)
    testSimData <- 
      simData[which(simData$Day >= as.Date(TestSimStartDate)), ]
    
    ## v 10.3.6: 
    TestSimStartDate <- min(testSimData$Day)
    
    ## Auch mit fieldData cutten damit es immer das gleiche Datum ist
    ## Synchronize End Date for Sim and Field Data:
    testSimData <-
      testSimData[as.Date(testSimData$Day) <= max(as.Date(testFieldData$Day)),]
    testFieldData <-
      testFieldData[as.Date(testFieldData$Day) <= max(as.Date(testSimData$Day)),]

    ## v10.3.6: end days synchronized:
    syncedEndTestDate <- min( max(testSimData$Day), max(testFieldData$Day) )
    testSimData <- testSimData[which(testSimData$Day <= syncedEndTestDate), ]
    TestFieldData <- fieldData[which(fieldData$Day <= syncedEndTestDate), ]  
    ###
    
    ## time muss bei 1 starten
    #### 20200125 TBB:
    #### UK uses no time information
    #### testSimData$time <- testSimData$time - min(testSimData$time)
    rownames(testSimData) <- NULL
    
    ## Combine data
    testData <- list(simData = testSimData,
                     fieldData = testFieldData)
    
    ## Check test data:
    if (conf$verbosity > 10){
    print( paste("testDataSim: ", c(min(testData$simData$Day), max(testData$simData$Day))))
    print( paste("testDataField: ", c(min(testData$fieldData$Day), max(testData$fieldData$Day))))
    }
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
    testConf$simRepeats = simrepeats
    testConf$ICU = icu
    testConf$ResourceNames = resourceNames
    testConf$ResourceEval = resourceEval
    testConf$percCores = percCores
    testConf$logLevel = 0
    testConf$w2 = icuWeights
    testConf$seed <- seed + i + 1
    set.seed(testConf$seed)
    
    if (conf$verbosity > 10){
    ## Check test config:
    print( paste("testConfSim: ", c(testConf$simulationDates$StartDate, testConf$simulationDates$EndDate)))
    print( paste("testConfField: ", c(testConf$fieldDates$StartDate, testConf$fieldDates$EndDate)))
    }
    SIM_EQ_FIELD_TESTCONF <- ( (testConf$simulationDates$StartDate != testConf$fieldDates$StartDate) &
                                 (testConf$simulationDates$EndDate != testConf$fieldDates$EndDate))
    SIM_EQ_FIELD_TESTCONF <- 
      (min(as.Date(testConf$simulationDates$StartDate)) == as.Date(TestSimStartDate)) &
      (min(as.Date(testConf$fieldDates$StartDate)) == as.Date(TestFieldStartDate)) &
      (max(as.Date(testConf$simulationDates$EndDate)) == max(as.Date(testConf$fieldDates$EndDate)))
    if(SIM_EQ_FIELD_TESTCONF == FALSE){stop("babsim.hospital::runoptDirect: Check testConf.")}
    
    testErr <- 0
    for (j in 1:testRepeats){
      testConf$seed <- seed + i + 1 + j
      set.seed(testConf$seed)
      envs <- modelResultHospital(para = testPara,
                                  conf = testConf,
                                  data = testData)
      testConf$verbosity <- 101
      err <- getError(envs, conf = testConf)
      print(paste( "single test error:", err))
      testErr <- testErr + err
    }
    testErr <- testErr/testRepeats
    if (conf$verbosity > 1){
    print(paste0("testErr: ", testErr))
    print(paste0("babsim.hospital version: ", 
                 utils::packageVersion("babsim.hospital")))
    }
    result.df <- rbind(result.df, data.frame(y = testErr, x = x))
    FILENAMETMP <-  paste0(expName, i, ".RData")
    if (conf$verbosity > 1000){
    print(FILENAMETMP)
    }
    save(result.df, file = FILENAMETMP)
  }
  if (conf$verbosity > 1000){
  print(FILENAME)
  }
  save(result.df, file = FILENAME)
  return(list (result.df, reslist))
}

