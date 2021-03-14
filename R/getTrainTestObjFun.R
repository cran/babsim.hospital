#' @title  getTrainTestObjFun
#'
#' @description Generate objective functions (one for train, optionally one for test)
#'
#' @param rkiwerte RKI Daten
#' @param icuwerte ICU Daten
#' @param region Landkreis Id, e.g., \code{5374} fuer OBK, \code{5315} fuer Koeln,
#' \code{0} fuer Deutschland,
#' oder Bundesland ID, e.g., \code{5} fuer NRW.
#' @param TrainFieldStartDate Start (Tag), e.g., \code{'2020-06-01'}
#' @param TrainSimStartDate Start (Tag), e.g., \code{'2020-05-01'}
#' @param TestFieldStartDate Start (Day), e.g., \code{'2020-06-01'} for test field data
#' @param TestSimStartDate Start (Day), e.g., \code{'2020-05-01'} for test simulation data,
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
getTrainTestObjFun <- function(rkiwerte = babsim.hospital::rkidata,
                               icuwerte = babsim.hospital::icudata,
                               region = 5374,
                               TrainSimStartDate = Sys.Date() - 12 * 7,
                               TrainFieldStartDate = Sys.Date() - 8 * 7,
                               TestSimStartDate = Sys.Date() - 8 * 7,
                               TestFieldStartDate = Sys.Date() - 4 * 7,
                               verbosity = 0,
                               parallel = FALSE,
                               percCores = NULL,
                               icu = TRUE,
                               icuWeights = c(1, 1),
                               resourceNames = c("intensiveBed", "intensiveBedVentilation"),
                               resourceEval = c("intensiveBed",  "intensiveBedVentilation"),
                               tryOnTestSet = TRUE) {
  messageDateRange("rkiwerte: Refdatum: %s", rkiwerte$Refdatum)
  messageDateRange("icuwerte: daten_stand: %s", icuwerte$daten_stand)
  
  result.df <- data.frame(x = NULL, y = NULL)
  reslist <- list()
  
  ## General data range adaptation for train and test data: Field data = Realdata,
  ## ICU, beds:
  regionIcuwerte <- getRegionIcu(data = icuwerte, region = region)
  fieldData <- getIcuBeds(regionIcuwerte)
  fieldData <-
    fieldData[which(fieldData$Day >= as.Date(TrainFieldStartDate)),]
  
  ## Sim data = RKI, infections:
  regionRkiwerte <- getRegionRki(data = rkiwerte, region = region)
  simData <- getRkiData(regionRkiwerte)
  simData <-
    simData[which(simData$Day >= as.Date(TrainSimStartDate)),]
  
  ## v 10.3.4: TrainSimStartDate must be corrected if it is not in the RKI data.
  ## This happens, e.g., if no data are transfered on Sundays:.
  TrainSimStartDate <- min(simData$Day)
  
  EndDate <-
    min(max(as.Date(simData$Day)), max(as.Date(fieldData$Day)))
  fieldData <- fieldData[which(fieldData$Day <= EndDate),]
  simData <- simData[which(simData$Day <= EndDate),]
  simData$time <- simData$time - min(simData$time)
  rownames(fieldData) <- NULL
  rownames(simData) <- NULL
  ##
  
  if (tryOnTestSet) {
    TrainEndDate <- as.Date(TestFieldStartDate)
  } else {
    TrainEndDate <- as.Date(EndDate)
  }
  
  TrainSimData <- simData[which(simData$Day <= TrainEndDate),]
  TrainFieldData <-
    fieldData[which(fieldData$Day <= TrainEndDate),]
  ## v10.3.4: end days synchronized:
  syncedEndTrainDate <-
    min(max(TrainSimData$Day), max(TrainFieldData$Day))
  
  TrainSimData <-
    simData[which(simData$Day <= syncedEndTrainDate),]
  TrainFieldData <-
    fieldData[which(fieldData$Day <= syncedEndTrainDate),]
  
  trainData <-
    list(simData = TrainSimData, fieldData = TrainFieldData)
  
  ## Check Train Data:
  messagef("trainDataSim: %s", min(as.Date(trainData$simData$Day)))
  messagef("trainDataField: %s", as.Date(TrainSimStartDate))
  SIM_EQ_FIELD_TRAINDATA <-
    (min(as.Date(trainData$simData$Day)) == as.Date(TrainSimStartDate))
  if (SIM_EQ_FIELD_TRAINDATA == FALSE) {
    stop("babsim.hospital::runoptDirect: Check TrainSimStartDate.")
  }
  
  SIM_EQ_FIELD_TRAINDATA <-
    (min(as.Date(trainData$fieldData$Day)) == as.Date(TrainFieldStartDate))
  if (SIM_EQ_FIELD_TRAINDATA == FALSE) {
    stop("babsim.hospital::runoptDirect: Check TrainFieldStartDate.")
  }
  
  SIM_EQ_FIELD_TRAINDATA <-
    (max(as.Date(trainData$simData$Day)) == max(as.Date(trainData$fieldData$Day)))
  if (SIM_EQ_FIELD_TRAINDATA == FALSE) {
    stop(
      "babsim.hospital::runoptDirect: Check trainData:
                                             sim and field End data do not agree."
    )
  }
  
  
  conf <- babsimToolsConf()
  trainConf <- getConfFromData(
    simData = trainData$simData,
    fieldData = trainData$fieldData,
    conf = conf
  )
  trainConf$seed <- NULL
  trainConf$verbosity <- verbosity
  trainConf$parallel <- parallel
  trainConf$ICU <- icu
  trainConf$ResourceNames <- resourceNames
  trainConf$ResourceEval <- resourceEval
  trainConf$percCores <- percCores
  trainConf$logLevel <- 1
  trainConf$w2 <- icuWeights
  ## Check train config:
  messagef(
    "trainConfSim: %s - %s",
    trainConf$simulationDates$StartDate,
    trainConf$simulationDates$EndDate
  )
  messagef(
    "trainConfField:  %s - %s",
    trainConf$fieldDates$StartDate,
    trainConf$fieldDates$EndDate
  )
  SIM_EQ_FIELD_TRAINCONF <-
    (min(as.Date(trainConf$simulationDates$StartDate)) ==
       as.Date(TrainSimStartDate)) &
    (min(as.Date(trainConf$fieldDates$StartDate)) ==
       as.Date(TrainFieldStartDate)) &
    (max(as.Date(trainConf$simulationDates$EndDate)) ==
       max(as.Date(trainConf$fieldDates$EndDate)))
  if (SIM_EQ_FIELD_TRAINCONF == FALSE) {
    stop("babsim.hospital::runoptDirect: Check trainConf.")
  }
  bounds <- getBounds()
  a <- bounds$lower
  b <- bounds$upper
  conf <- trainConf
  if (conf$verbosity > 1000) {
   printConf(conf)
  }
  data <- trainData
  
  trainFun <- function(x) {
    funOptimizeSim(x, trainConf, trainData)
  }
  
  if (tryOnTestSet) {
    ## Generate test data for final evaluation of one optimization run:
    testFieldData <-
      fieldData[which(fieldData$Day >= as.Date(TestFieldStartDate)),]
    rownames(testFieldData) <- NULL
    
    ## testSimData
    testSimData <-
      simData[which(simData$Day >= as.Date(TestSimStartDate)),]
    
    ## v 10.3.6:
    TestSimStartDate <- min(testSimData$Day)
    
    testSimData <-
      testSimData[as.Date(testSimData$Day) <= max(as.Date(testFieldData$Day)),]
    testFieldData <-
      testFieldData[as.Date(testFieldData$Day) <= max(as.Date(testSimData$Day)),]
    
    ## v10.3.6: end days synchronized:
    syncedEndTestDate <-
      min(max(testSimData$Day), max(testFieldData$Day))
    testSimData <-
      testSimData[which(testSimData$Day <= syncedEndTestDate),]
    TestFieldData <-
      fieldData[which(fieldData$Day <= syncedEndTestDate),]
    ###
    
    testSimData$time <- testSimData$time - min(testSimData$time)
    rownames(testSimData) <- NULL
    
    ## Combine data
    testData <-
      list(simData = testSimData, fieldData = testFieldData)
    
    ## Check test data:
    messageDateRange("testDataSim: %s", testData$simData$Day)
    messageDateRange("testDataField: %s", testData$fieldData$Day)
    SIM_EQ_FIELD_TESTDATA <-
      (min(as.Date(testData$simData$Day)) == as.Date(TestSimStartDate))
    
    if (SIM_EQ_FIELD_TESTDATA == FALSE) {
      stop("babsim.hospital::runoptDirect: Check testData:
                 TestSimStartDate.")
    }
    
    
    SIM_EQ_FIELD_TESTDATA <-
      (min(as.Date(testData$fieldData$Day)) == as.Date(TestFieldStartDate))
    if (SIM_EQ_FIELD_TESTDATA == FALSE) {
      stop("babsim.hospital::runoptDirect: Check testData: TestFieldStartDate.")
    }
    SIM_EQ_FIELD_TESTDATA <-
      (max(as.Date(testData$simData$Day)) == max(as.Date(testData$fieldData$Day)))
    if (SIM_EQ_FIELD_TESTDATA == FALSE) {
      stop(
        "babsim.hospital::runoptDirect: Check testData:
                                                sim and field data End date differ!"
      )
    }
    conf <- babsimToolsConf()
    testConf <- getConfFromData(simData = testSimData,
                                fieldData = testFieldData,
                                conf = conf)
    testConf$verbosity <- verbosity
    testConf$parallel <- parallel
    testConf$ICU <- icu
    testConf$ResourceNames <- resourceNames
    testConf$ResourceEval <- resourceEval
    testConf$percCores <- percCores
    testConf$logLevel <- 1
    testConf$w2 <- icuWeights
    testConf$seed <- NULL
    
    ## Check test config:
    messagef(
      "testConfSim:  %s - %s",
      testConf$simulationDates$StartDate,
      testConf$simulationDates$EndDate
    )
    messagef("testConfField:  %s - %s",
             testConf$fieldDates$StartDate,
             testConf$fieldDates$EndDate)
    SIM_EQ_FIELD_TESTCONF <-
      ((
        testConf$simulationDates$StartDate != testConf$fieldDates$StartDate
      ) &
        (
          testConf$simulationDates$EndDate != testConf$fieldDates$EndDate
        )
      )
    SIM_EQ_FIELD_TESTCONF <-
      (min(as.Date(testConf$simulationDates$StartDate)) ==
         as.Date(TestSimStartDate)) &
      (min(as.Date(testConf$fieldDates$StartDate)) ==
         as.Date(TestFieldStartDate)) &
      (max(as.Date(testConf$simulationDates$EndDate)) ==
         max(as.Date(testConf$fieldDates$EndDate)))
    if (SIM_EQ_FIELD_TESTCONF == FALSE) {
      stop("babsim.hospital::runoptDirect: Check testConf.")
    }
    
    testFun <- function(x) {
      funOptimizeSim(x, testConf, testData)
    }
    return(list(trainFun = trainFun, testFun = testFun))
  }
  
  return(trainFun)
}
