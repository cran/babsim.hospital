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
                        TrainFieldStartDate = NULL, # "2020-10-01",
                        TrainSimStartDate = NULL, # "2020-09-01",
                        verbosity = 0,
                        seed = 123,
                        funEvals = 35,
                        simrepeats = 2,
                        parallel = FALSE,
                        percCores = NULL, # 0.8,
                        icu = TRUE,
                        icuWeights = 1,
                        resourceNames = c("intensiveBed", "intensiveBedVentilation"),
                        resourceEval = c("intensiveBed", "intensiveBedVentilation"),
                        spotEvalsParallel = FALSE) {
  messageDateRange("Cases", rkiwerte$Refdatum)
  messageDateRange("Resources", icuwerte$daten_stand)

  result.df <- data.frame(x = NULL, y = NULL)
  reslist <- list()

  ## General data range adaptation for train and test data:
  ##  Field data = Realdata, ICU, beds:
  regionIcuwerte <- getRegionIcu(
    data = icuwerte,
    region = region
  )
  fieldData <- getIcuBeds(regionIcuwerte)
  fieldData <-
    fieldData[which(fieldData$Day >= as.Date(TrainFieldStartDate)), ]

  ## Sim data = RKI, infections:
  regionRkiwerte <- getRegionRki(
    data = rkiwerte,
    region = region
  )
  simData <- getRkiData(regionRkiwerte)
  simData <- simData[which(simData$Day >= as.Date(TrainSimStartDate)), ]

  ## v 10.3.4: TrainSimStartDate must be corrected if it is not in the RKI data.
  ## This happens, e.g., if no data are transfered on Sundays:.
  TrainSimStartDate <- min(simData$Day)

  EndDate <- min(
    max(as.Date(simData$Day)),
    max(as.Date(fieldData$Day))
  )
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
  syncedEndTrainDate <- min(max(TrainSimData$Day), max(TrainFieldData$Day))

  TrainSimData <- simData[which(simData$Day <= syncedEndTrainDate), ]
  TrainFieldData <- fieldData[which(fieldData$Day <= syncedEndTrainDate), ]

  trainData <- list(
    simData = TrainSimData,
    fieldData = TrainFieldData
  )

  ## Check Train Data:
  messageDateRange("trainDataSim", trainData$simData$Day)
  messageDateRange("trainDataField", trainData$fieldData$Day)
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
    stop("babsim.hospital::runoptDirect: Check trainData:
                                             sim and field End data do not agree.")
  }

  runLocalOptim <- function(x0) {
    if (spotEvalsParallel) {
      requireNamespace("babsim.hospital")
      requireNamespace("SPOT")
    }
    # print(paste0("StartConfig: ", x0, " ###############################"))
    x0 <- as.vector(unlist(x0))

    conf <- babsimToolsConf()
    trainConf <- getConfFromData(
      simData = trainData$simData,
      fieldData = trainData$fieldData,
      conf = conf
    )
    trainConf$verbosity <- verbosity
    trainConf$parallel <- parallel
    trainConf$simRepeats <- simrepeats
    trainConf$ICU <- icu
    trainConf$ResourceNames <- resourceNames
    trainConf$ResourceEval <- resourceEval
    trainConf$percCores <- percCores
    trainConf$logLevel <- 0
    trainConf$w2 <- icuWeights
    ## Check train config:
    messagef("trainConfSim:   %s - %s", trainConf$simulationDates$StartDate, trainConf$simulationDates$EndDate)
    messagef("trainConfField: %s - %s", trainConf$fieldDates$StartDate, trainConf$fieldDates$EndDate)
    SIM_EQ_FIELD_TRAINCONF <-
      (min(as.Date(trainConf$simulationDates$StartDate)) == as.Date(TrainSimStartDate)) &
        (min(as.Date(trainConf$fieldDates$StartDate)) == as.Date(TrainFieldStartDate)) &
        (max(as.Date(trainConf$simulationDates$EndDate)) == max(as.Date(trainConf$fieldDates$EndDate)))
    if (SIM_EQ_FIELD_TRAINCONF == FALSE) {
      stop("babsim.hospital::runoptDirect: Check trainConf.")
    }
    set.seed(seed)
    bounds <- getBounds()
    a <- bounds$lower
    b <- bounds$upper
    conf <- trainConf
    data <- trainData

    funWrapLocalOpt <- function(x) {
      funOptimizeSim(as.vector(unlist(x)), conf, data)
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
    res <- optimNLOPTR(
      x = matrix(x0, nrow = 1),
      fun = funWrapLocalOpt,
      lower = a,
      upper = b,
      control = list(
        funEvals = funEvals,
        ## The following global NLOPT algorithm is the only one supporting constraints
        opts = list(algorithm = "NLOPT_GN_ISRES"),
        eval_g_ineq = g
      )
    )
    ## add one result to list
    reslist[[length(reslist) + 1]] <- res
    ## take parameter from one run on train data
    x <- as.matrix(res$xbest, 1, )
    # print(paste0("xbest: ", x))

    return(data.frame(y = res$ybest, x = res$xbest))
  }


  ### Entering optimization:
  messagef("Starting optimization loop:")
  messagef("#########################################")
  ## Selection for parallel vs sequential evaluation of spot repeats
  if (!spotEvalsParallel) {
    for (i in 1:nrow(X)) {
      result.df <- rbind(result.df, runLocalOptim(as.vector(unlist(X[i, ]))))
    }
  } else {
    ## This parallel here means the parallel inside of spot. Therefore
    ## if the simrepeats of simmer are running in parallel or sequential
    ## If so, then global cluster which is running spot in parallel should not
    ## reserve all cores.
    if (parallel == TRUE) {
      results <- parallel::mclapply(X = suppressWarnings(split(X, row(X))), FUN = runLocalOptim, mc.cores = parallel::detectCores() / (percCores * 32))
    } else {
      results <- parallel::mclapply(X = suppressWarnings(split(X, row(X))), FUN = runLocalOptim, mc.cores = parallel::detectCores())
    }

    result.df <- dplyr::bind_rows(results)
  }
  return(result.df)
}
