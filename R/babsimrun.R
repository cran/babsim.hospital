#' @title  runoptDirect Optimierung der babsim.hospital Parameter
#'
#' @description SPOT Aufruf zur Optimierung der babsim Parameter mit Lasso 
#'
#' @param expName Experiment Name
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
#' @param Overlap integer. Days, train data will be extended (overlap with test data). Default: 7
#' @param seed Seed
#' @param direct use model-free optimization. Default: \code{FALSE}
#' @param repeats Wiederholungen fuer SPOT (Optimierungslaeufe mit unterschiedlichem Seed)
#' @param funEvals Auswertungen fuer SPOT (Simulationen, 
#' die fuer einen SPOT Lauf zur Verfuegung stehen)
#' @param funEvalsFactor factor to increase function evaluations. Default: 0
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
#' @param spotEvalsParallel Should the spot repeats be evaluated in parallel?
#' @param tryOnTestSet Should results be tested on a separate test set?
#' 
#' @importFrom SPOT spot
#' @importFrom SPOT optimNLOPTR
#' @importFrom SPOT buildLasso
#' @importFrom SPOT buildKriging
#' @importFrom SPOT selectAll
#' @importFrom SPOT selectN
#' @importFrom parallel mclapply
#' 
#' @export

runoptDirect <- function(expName = "obkpara20201017",
                         rkiwerte = babsim.hospital::rkidata,
                         icuwerte = babsim.hospital::icudata,
                         region = 5374,
                         TrainFieldStartDate = NULL, #"2020-10-01",
                         TrainSimStartDate = NULL, #"2020-09-01",
                         TestFieldStartDate = NULL, #"2020-10-20",
                         TestSimStartDate = NULL, #"2020-09-20",
                         Overlap = 7,
                         verbosity = 0,
                         seed = 123,
                         direct = FALSE,
                         repeats = 1,
                         funEvals = 35,
                         funEvalsFactor = 0,
                         size = 30,
                         simrepeats = 2,
                         subset = 32,
                         parallel = FALSE,
                         percCores = NULL, #0.8,
                         icu = TRUE,
                         icuWeights = 1,
                         testRepeats = 3,
                         resourceNames =  c("intensiveBed", "intensiveBedVentilation"),
                         resourceEval = c("intensiveBed", "intensiveBedVentilation"),
                         spotEvalsParallel = FALSE,
                         tryOnTestSet = TRUE){
    
    ### start test: comment after testing is finished!!!!!
    # 
    # library("babsim.hospital")
    # library("SPOT")
    # library("simmer")
    #     expName = "test"
    #     rkiwerte = babsim.hospital::rkidata
    #     icuwerte = babsim.hospital::icudata
    #     region = 5374
    #     TrainFieldStartDate = Sys.Date() - 6*7
    #     TrainSimStartDate = Sys.Date() - 10*7
    #     TestFieldStartDate = Sys.Date() - 4*7
    #     TestSimStartDate = Sys.Date() - 8*7
    #     Overlap = 14
    #     verbosity = 1
    #     seed = 410250
    #     repeats = 2
    #     funEvals = 40
    #     funEvalsFactor = 0
    #     size = 35
    #     simrepeats = 2
    #     subset = 35
    #     parallel = TRUE
    #     percCores = 0.9
    #     icu = TRUE
    #     icuWeights = c(1,10)
    #     testRepeats = 1
    #     direct = TRUE
    #     resourceNames =  c("intensiveBed", "intensiveBedVentilation")
    #     resourceEval = c("intensiveBed", "intensiveBedVentilation")
    # # # # ### end test
    # 
    
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
    
    ### The following, commented lines should be re-considered !!!:
    
    # ## FirstDateFromData: first day for sim and field data
    # FirstDateFromData <- max( min(as.Date(simData$Day)), 
    #                           min(as.Date(fieldData$Day)) )
    # 
    # if (FirstDateFromData > TrainFieldStartDate){
    #     TrainFieldStartDate = FirstDateFromData
    #     TrainSimStartDate = FirstDateFromData -30
    #     print( paste("WARNING! TrainFieldStartDate corrected: ", TrainFieldStartDate))
    #     print( paste("WARNING! TrainSimStartDate corrected: ", TrainSimStartDate))
    # }
    # if (FirstDateFromData > TestFieldStartDate){
    #     TestFieldStartDate = FirstDateFromData + 7
    #     TestSimStartDate = TestFieldStartDate - 30
    #     print( paste("WARNING! TestFieldStartDate corrected: ", TestFieldStartDate))
    #     print( paste("WARNING! TestSimStartDate corrected: ", TestSimStartDate))
    # }
    ## EndDate: last day for sim and field data
    
    
    EndDate <- min( max(as.Date(simData$Day)), 
                    max(as.Date(fieldData$Day)) )
    fieldData <-
        fieldData[which(fieldData$Day <= EndDate), ]
    simData <-
        simData[which(simData$Day <= EndDate), ]
    ## time muss bei 1 starten 
    ## Nein, bei 0 - please check !!!
    simData$time <- simData$time - min(simData$time)
    rownames(fieldData) <- NULL
    rownames(simData) <- NULL
    ##
    
    if(tryOnTestSet){
        TrainEndDate <- as.Date(TestFieldStartDate) + Overlap
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
    
    
    FILENAME <-  paste0("results/", expName, ".RData")
    
    
    runSpotRepeat <- function(i){
        if(spotEvalsParallel){
            requireNamespace("babsim.hospital")
            requireNamespace("SPOT")
        }
        print(paste0("Repeat: ", i, " ###############################"))
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
        trainConf$logLevel = 1
        trainConf$w2 = icuWeights
        trainConf$seed <- seed + i
        ## Check train config:
        print( paste("trainConfSim: ", c(trainConf$simulationDates$StartDate, trainConf$simulationDates$EndDate)))
        print( paste("trainConfField: ", c(trainConf$fieldDates$StartDate, trainConf$fieldDates$EndDate)))
        SIM_EQ_FIELD_TRAINCONF <- 
            (min(as.Date(trainConf$simulationDates$StartDate)) == as.Date(TrainSimStartDate)) &
            (min(as.Date(trainConf$fieldDates$StartDate)) == as.Date(TrainFieldStartDate)) &
            (max(as.Date(trainConf$simulationDates$EndDate)) == max(as.Date(trainConf$fieldDates$EndDate)))
        if(SIM_EQ_FIELD_TRAINCONF == FALSE){stop("babsim.hospital::runoptDirect: Check trainConf.")}
        set.seed(trainConf$seed)
        funEvals <- funEvals + (i - 1) * funEvalsFactor
        x0 <- getStartParameter(region= region)
        bounds <- getBounds()
        a <- bounds$lower
        b <- bounds$upper
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
                x = as.matrix(x0),
                fun = funWrapOptimizeSim,
                lower = a,
                upper = b,
                control = list(
                    funEvals = funEvals,
                    noise = TRUE,
                    direct = direct,
                    designControl = list(
                        # inequalityConstraint = g,
                        size = size,
                        retries=1000),
                    optimizer = optimNLOPTR,
                    optimizerControl = list(
                        ## The following global NLOPT algorithm is the only one supporting constraints
                        opts = list(algorithm = "NLOPT_GN_ISRES"),
                        eval_g_ineq = g
                    ),
                    model =  buildKriging,
                    plots = FALSE,
                    progress = TRUE,
                    subsetSelect = selectN,
                    subsetControl = list(N = subset)
                ),
                conf,
                data 
            )
        )
        # save(list = eval(parse(text = "expName")) , file = FILENAME)
        ## result from one optimization run on train data:
        res <- get(expName)
        x <- as.matrix(res$xbest, 1, )
        if(tryOnTestSet){
            ### Entering test:
            print("Starting test evaluation:")
            print("#########################################")
            
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
            testConf$simRepeats = simrepeats
            testConf$ICU = icu
            testConf$ResourceNames = resourceNames
            testConf$ResourceEval = resourceEval
            testConf$percCores = percCores
            testConf$logLevel = 1
            testConf$w2 = icuWeights
            testConf$seed <- seed + i + 1
            set.seed(testConf$seed)
            
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
            print(paste0("testErr: ", testErr))
            print(paste0("babsim.hospital version: ", 
                         utils::packageVersion("babsim.hospital")))
            return(list( res = res,
                         best = data.frame(y = testErr, x = x)))
        }else{ # END: tryOnTestSet
            return(list(res = res,
                        best = data.frame(y = res$ybest, x = res$xbest)))
        }
    } # END: runSpotRepeat()
    
    
    ### Entering optimization:
    print("Starting optimization loop:")
    print("#########################################")
    ## Selection for parallel vs sequential evaluation of spot repeats
    if(!spotEvalsParallel){
        for (i in 1:repeats) {
            y <- runSpotRepeat(i)
            ## add one result to list
            reslist[[length(reslist)+1]] <- y$res
            ##
            result.df <- rbind(result.df, y$best)
            FILENAMETMP <-  paste0("results/", expName, i, ".RData")
            print(FILENAMETMP)
            save(result.df, file = FILENAMETMP)
        }
    }else{
        ## This parallel here means the parallel inside of spot. Therefore
        ## if the simrepeats of simmer are running in parallel or sequential
        ## If so, then global cluster which is running spot in parallel should not
        ## reserve all cores. 
        if(parallel == TRUE){
            #cl <- parallel::makeCluster(parallel::detectCores()/(percCores*32))
            results <- parallel::mclapply(X = 1:repeats, FUN = runSpotRepeat, mc.cores = parallel::detectCores()/(percCores*32)) 
        }else{
            #cl <- parallel::makeCluster(parallel::detectCores())
            results <- parallel::mclapply(X = 1:repeats, FUN = runSpotRepeat, mc.cores = parallel::detectCores()) 
        }
        #parallel::parLapply(cl, 1:repeats, runSpotRepeat)
        
        getBest <- function(result.list){
            result.list$best
        }
        getRes <- function(result.list){
            result.list$res
        }
        
        bestResults <- lapply(results,getBest)
        reslist <- lapply(results,getRes)
        result.df <- dplyr::bind_rows(bestResults)
        #parallel::stopCluster(cl)
    }
    
    print(FILENAME)
    save(result.df, file = FILENAME)
    return(list (best.df = result.df, 
                 reslist = reslist))
} # END: runoptDirect


#' @title  runOptLocal
#'
#' @description Run a local optimization on a set of parameters
#'
#' @param X matrix of parameters. Each row should contain one parameterset to which a local optimization is applied
#' @param rkiwerte RKI Daten
#' @param icuwerte ICU Daten
#' @param region Landkreis Id, e.g., \code{5374} fuer OBK, \code{5315} fuer Koeln,
#' \code{0} fuer Deutschland,
#' oder Bundesland ID, e.g., \code{5} fuer NRW.
#' @param TrainFieldStartDate Start (Tag), e.g., \code{"2020-06-01"}
#' @param TrainSimStartDate Start (Tag), e.g., \code{"2020-05-01"}
#' @param seed Seed
#' @param funEvals Auswertungen fuer SPOT (Simulationen, 
#' die fuer einen SPOT Lauf zur Verfuegung stehen)
#' @param simrepeats Sim Wdhlg
#' @param parallel logical
#' @param percCores percentage
#' @param icu ICU Daten
#' @param icuWeights Gewichtung der ICU Betten
#' @param resourceNames Name der Ressourcen
#' @param resourceEval Name der zu evaluierenden Ressourcen
#' @param verbosity verbosity (int). Default: \code{0} 
#' @param spotEvalsParallel Should the spot repeats be evaluated in parallel?
#' 
#' @importFrom SPOT optimNLOPTR
#' @importFrom SPOT selectAll
#' @importFrom SPOT selectN
#' @importFrom parallel mclapply
#' 
#' @export
runOptLocal <- function(X,
                        rkiwerte = babsim.hospital::rkidata,
                        icuwerte = babsim.hospital::icudata,
                        region = 5374,
                        TrainFieldStartDate = NULL, #"2020-10-01",
                        TrainSimStartDate = NULL, #"2020-09-01",
                        verbosity = 0,
                        seed = 123,
                        funEvals = 35,
                        simrepeats = 2,
                        parallel = FALSE,
                        percCores = NULL, #0.8,
                        icu = TRUE,
                        icuWeights = 1,
                        resourceNames =  c("intensiveBed", "intensiveBedVentilation"),
                        resourceEval = c("intensiveBed", "intensiveBedVentilation"),
                        spotEvalsParallel = FALSE){
    
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
    TrainEndDate <- as.Date(EndDate)
    
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
    
    runLocalOptim <- function(x0){
        if(spotEvalsParallel){
            requireNamespace("babsim.hospital")
            requireNamespace("SPOT")
        }
        print(paste0("StartConfig: ", x0, " ###############################"))
        x0 <- as.vector(unlist(x0))
        
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
        set.seed(seed)
        bounds <- getBounds()
        a <- bounds$lower
        b <- bounds$upper
        conf <- trainConf
        data <- trainData
        
        funWrapLocalOpt <- function(x){
            funOptimizeSim(as.vector(unlist(x)),conf,data)
        }
        
        g <- function(x) {
            x <- unlist(x)
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
                    x[20] + x[21] - 1
                )
            )
        }
        
        # save(list = eval(parse(text = "expName")) , file = FILENAME)
        ## result from one optimization run on train data:
        res <- optimNLOPTR(x = matrix(x0, nrow = 1),
                           fun = funWrapLocalOpt,
                           lower = a,
                           upper = b,
                           control = list(
                               funEvals = funEvals,
                               ## The following global NLOPT algorithm is the only one supporting constraints
                               opts = list(algorithm = "NLOPT_GN_ISRES"),
                               eval_g_ineq = g
                           ))
        ## add one result to list
        reslist[[length(reslist)+1]] <- res
        ## take parameter from one run on train data
        x <- as.matrix(res$xbest, 1, )
        print(paste0("xbest: ", x))
        
        return(data.frame(y = res$ybest, x = res$xbest))
    }
    
    
    ### Entering optimization:
    print("Starting optimization loop:")
    print("#########################################")
    ## Selection for parallel vs sequential evaluation of spot repeats
    if(!spotEvalsParallel){
        for (i in 1:nrow(X)) {
            result.df <- rbind(result.df, runLocalOptim(as.vector(unlist(X[i,]))))
        }
    }else{
        ## This parallel here means the parallel inside of spot. Therefore
        ## if the simrepeats of simmer are running in parallel or sequential
        ## If so, then global cluster which is running spot in parallel should not
        ## reserve all cores. 
        if(parallel == TRUE){
            results <- parallel::mclapply(X = suppressWarnings(split(X, row(X))), FUN = runLocalOptim, mc.cores = parallel::detectCores()/(percCores*32))
        }else{
            results <- parallel::mclapply(X = suppressWarnings(split(X, row(X))), FUN = runLocalOptim, mc.cores = parallel::detectCores()) 
        }
        
        result.df <- dplyr::bind_rows(results)
    }
    return(result.df)
}
