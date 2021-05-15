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
#' @param tryOnTestSet Should results be tested on a separate test set?. Default \code{FALSE}.
#' @param simRepeats Number of simulation repeats on each kernel (only on unix-like machines, ignored on Win systems). Default: 1
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
                               tryOnTestSet = FALSE,
                               simRepeats = 1) {
  if (verbosity > 10) {
    messageDateRange("rkiwerte: Refdatum: %s", rkiwerte$Refdatum)
    messageDateRange("icuwerte: daten_stand: %s", icuwerte$daten_stand)
  }
  result.df <- data.frame(x = NULL, y = NULL)
  reslist <- list()
  
  ## General data range adaptation for train and test data: Field data = Realdata,
  ## ICU, beds:
  regionIcuwerte <- getRegionIcu(data = icuwerte, region = region)
  fieldData <- getIcuBeds(regionIcuwerte)
  fieldData <-
    fieldData[which(fieldData$Day >= as.Date(TrainFieldStartDate)), ]
  
  ## Sim data = RKI, infections:
  regionRkiwerte <- getRegionRki(data = rkiwerte, region = region)
  simData <- getRkiData(regionRkiwerte)
  simData <-
    simData[which(simData$Day >= as.Date(TrainSimStartDate)), ]
  
  ## Generate train data:
  trainData <- getAndCheckTrainData(
    simData = simData,
    fieldData = fieldData,
    tryOnTestSet = tryOnTestSet,
    TrainFieldStartDate = TrainFieldStartDate,
    TestFieldStartDate = TestFieldStartDate
  )
  
  ## Compile configuration:
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
  trainConf$logLevel <- 0
  trainConf$w2 <- icuWeights
  trainConf$simRepeats <- simRepeats
  
  checkTrainConfig(trainConf,
                   TrainSimStartDate,
                   TrainFieldStartDate,
                   verbosity)
  
  bounds <- getBounds()
  a <- bounds$lower
  b <- bounds$upper
  
  if (trainConf$verbosity > 1000) {
    printConf(trainConf)
  }
  
  trainFun <- function(x) {
    funOptimizeSim(x, trainConf, trainData)
  }
  
  if (tryOnTestSet) {
    testData <- getTestData(fieldData,
                            simData,
                            TestFieldStartDate,
                            TestSimStartDate)
    
    checkTestData(testData,
                  TestSimStartDate,
                  TestFieldStartDate)
    
    conf <- babsimToolsConf()
    testConf <- getConfFromData(simData = testData$simData,
                                fieldData = testData$fieldData,
                                conf = conf)
    testConf$verbosity <- verbosity
    testConf$parallel <- parallel
    testConf$ICU <- icu
    testConf$ResourceNames <- resourceNames
    testConf$ResourceEval <- resourceEval
    testConf$percCores <- percCores
    testConf$logLevel <- 0
    testConf$w2 <- icuWeights
    testConf$seed <- NULL
    conf$simRepeats <- simRepeats
    
    checkTestConfig(testConf,
                    TestSimStartDate,
                    TestFieldStartDate,
                    verbosity)
    
    testFun <- function(x) {
      funOptimizeSim(x, testConf, testData)
    }
    return(list(trainFun = trainFun, testFun = testFun))
  }
  
  return(trainFun)
}


#' @title Get and Check Train Data
#'
#' @description Generate and check train data (start and end dates) from field and simulation
#' data.
#'
#' @param fieldData field data, i.e., real-world data.
#' @param simData simulation data, i.e., data used for the simulator, e.g. infections.
#' @param tryOnTestSet logical, default FALSE.
#' @param TrainFieldStartDate Start date of the train data set.
#' @param TestFieldStartDate Start date of the test data set.
#' @param verbosity Default 0.
#' Only needed if \code{tryOnTestSet} is \code{TRUE}.
#'
#' @return traindata, i.e., list of sim data and field data, checked train data set
#'
#' @export
getAndCheckTrainData <-
  function(simData,
           fieldData,
           tryOnTestSet = FALSE,
           TrainFieldStartDate,
           TestFieldStartDate,
           verbosity = 0) {
    ## v 10.3.4: TrainSimStartDate must be corrected if it is not in the RKI data.
    ## This happens, e.g., if no data are transfered on Sundays:.
    TrainSimStartDate <- min(simData$Day)
    
    EndDate <-
      min(max(as.Date(simData$Day)), max(as.Date(fieldData$Day)))
    
    fieldData <-
      fieldData[which(fieldData$Day <= EndDate),]
    
    simData <-
      simData[which(simData$Day <= EndDate),]
    
    ## UK data uses no time information, whereas RKI/DIVI uses time
    if (!is.null(simData$time)) {
      simData$time <- simData$time - min(simData$time)
    }
    rownames(fieldData) <- NULL
    rownames(simData) <- NULL
    
    if (tryOnTestSet) {
      TrainEndDate <- as.Date(TestFieldStartDate)
    } else {
      TrainEndDate <- as.Date(EndDate)
    }
    
    TrainSimData <-
      simData[which(simData$Day <= TrainEndDate),]
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
    if (verbosity > 10) {
      messagef("trainDataSim: %s", min(as.Date(trainData$simData$Day)))
      messagef("trainDataField: %s", as.Date(TrainSimStartDate))
    }
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
    return(trainData)
  }

#' @title  Check train config
#'
#' @description Check consistency of the training configuration
#'
#' @param trainConf trainConfig
#' @param TrainSimStartDate Start data simulation train data
#' @param TrainFieldStartDate Start data simulation field data
#' @param verbosity Default 0.
#'
#' @export
checkTrainConfig <- function(trainConf,
                             TrainSimStartDate,
                             TrainFieldStartDate,
                             verbosity = 0) {
  if (verbosity > 10) {
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
  }
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
}



#' @title Get Test Data
#'
#' @description  Generate test data for final evaluation of one optimization run
#'
#' @param fieldData field data
#' @param simData sim data
#' @param TestFieldStartDate start date
#' @param TestSimStartDate start date
#'
#' @return testData test data
#'
#' @export
getTestData <- function(fieldData,
                        simData,
                        TestFieldStartDate,
                        TestSimStartDate) {
  testFieldData <-
    fieldData[which(fieldData$Day >= as.Date(TestFieldStartDate)), ]
  rownames(testFieldData) <- NULL
  
  ## testSimData
  testSimData <-
    simData[which(simData$Day >= as.Date(TestSimStartDate)), ]
  ## v 10.3.6:
  TestSimStartDate <- min(testSimData$Day)
  
  testSimData <-
    testSimData[as.Date(testSimData$Day) <= max(as.Date(testFieldData$Day)), ]
  testFieldData <-
    testFieldData[as.Date(testFieldData$Day) <= max(as.Date(testSimData$Day)), ]
  
  ## v10.3.6: end days synchronized:
  syncedEndTestDate <-
    min(max(testSimData$Day), max(testFieldData$Day))
  testSimData <-
    testSimData[which(testSimData$Day <= syncedEndTestDate), ]
  TestFieldData <-
    fieldData[which(fieldData$Day <= syncedEndTestDate), ]
  ###
  
  testSimData$time <- testSimData$time - min(testSimData$time)
  rownames(testSimData) <- NULL
  
  ## Combine data
  testData <-
    list(simData = testSimData, fieldData = testFieldData)
  
  return(testData)
}



#' @title Check Test Data
#'
#' @description Check test data
#'
#' @param testData list of test data: field and sim
#' @param TestSimStartDate test data: sim start date
#' @param TestFieldStartDate test data: field start date
#' @param verbosity default 0.
#'
#' @export
checkTestData <- function(testData,
                          TestSimStartDate,
                          TestFieldStartDate,
                          verbosity) {
  ## Check test data:
  if (verbosity > 10) {
    messageDateRange("testDataSim: %s", testData$simData$Day)
    messageDateRange("testDataField: %s", testData$fieldData$Day)
  }
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
}


#' @title Check test config
#'
#' @param testConf test configuration
#' @param TestSimStartDate test data: sim start date
#' @param TestFieldStartDate test data: field start date
#' @param verbosity default 0.
#'
#' @export
checkTestConfig <- function(testConf,
                            TestSimStartDate,
                            TestFieldStartDate,
                            verbosity) {
  if (verbosity > 10) {
    messagef(
      "testConfSim:  %s - %s",
      testConf$simulationDates$StartDate,
      testConf$simulationDates$EndDate
    )
    messagef(
      "testConfField:  %s - %s",
      testConf$fieldDates$StartDate,
      testConf$fieldDates$EndDate
    )
  }
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
}
