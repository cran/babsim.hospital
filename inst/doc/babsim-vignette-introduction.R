## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "%>"
)

## ---- cleanStart--------------------------------------------------------------

suppressPackageStartupMessages({
library("SPOT")
library("babsim.hospital")
library("simmer")
library("simmer.plot")
})

## ----loadSPOT, eval = TRUE----------------------------------------------------
packageVersion("SPOT")

## ---- eval = FALSE------------------------------------------------------------
#  updateRkidataFile("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data")

## ---- getAndShowRkiData-------------------------------------------------------
str(babsim.hospital::rkidata)

## ---- visuRki, fig.width=8, out.width="100%"----------------------------------
p <- ggVisualizeRki(data=babsim.hospital::rkidata, region = 5374)
print(p)

## ---- preprocessRki-----------------------------------------------------------
rki <- getRkiData(rki = rkidata)
str(rki)

## ---- getDiviData, eval = FALSE-----------------------------------------------
#  updateIcudataFile("https://www.divi.de/joomlatools-files/docman-files/divi-intensivregister-tagesreports-csv/DIVI-Intensivregister_YYYY-MM-DD_12-15.csv")

## ---- getRkiData--------------------------------------------------------------
str(babsim.hospital::icudata)

## ---- viszIcu-----------------------------------------------------------------
p <- ggVisualizeIcu(region = 5374)

## ---- viszIcu1, fig.width=8, out.width="100%"---------------------------------
print(p[[1]])

## ---- viszIcu2, fig.width=8, out.width="100%"---------------------------------
print(p[[2]])

## -----------------------------------------------------------------------------
fieldData <- getIcuBeds(babsim.hospital::icudata)
str(fieldData)

## ---- selectConfig------------------------------------------------------------
region = 5374 # Germany, 5315 is Cologne, 5 is NRW
seed = 123
simrepeats = 2
parallel = FALSE
percCores = 0.8
resourceNames =  c("intensiveBed", "intensiveBedVentilation")
resourceEval = c("intensiveBed", "intensiveBedVentilation")

## ---- selectIcuData-----------------------------------------------------------
FieldStartDate = "2020-09-01"
# Felddaten (Realdaten, ICU):
icudata <- getRegionIcu(data = icudata, region = region)
fieldData <- getIcuBeds(icudata)
fieldData <- fieldData[which(fieldData$Day >= as.Date(FieldStartDate)), ]
rownames(fieldData) <- NULL
icu = TRUE
icuWeights = c(1,1)

## ---- selectRkiData-----------------------------------------------------------
SimStartDate = "2020-08-01"
rkidata <- getRegionRki(data = rkidata, region = region)
simData <- getRkiData(rkidata)
simData <- simData[which(simData$Day >= as.Date(SimStartDate)), ]
## Auch mit fieldData cutten damit es immer das gleiche Datum ist
simData <- simData[as.Date(simData$Day) <= max(as.Date(fieldData$Day)),]
## time must start at 1
simData$time <- simData$time - min(simData$time)
rownames(simData) <- NULL

## ---- combineSelectedData-----------------------------------------------------
data <- list(simData = simData, fieldData = fieldData)

## ---- getSimConf--------------------------------------------------------------
conf <- babsimToolsConf()
conf <- getConfFromData(conf = conf,
                        simData = data$simData,
                        fieldData = data$fieldData)
conf$parallel = parallel
conf$simRepeats = simrepeats
conf$ICU = icu
conf$ResourceNames = resourceNames
conf$ResourceEval = resourceEval
conf$percCores = percCores
conf$logLevel = 0
conf$w2 = icuWeights
set.seed(conf$seed)

## ---- getSimParameter---------------------------------------------------------
para <- babsimHospitalPara()
str(para)

## ---- getRiskAndArrival-------------------------------------------------------
rkiWithRisk <- getRkiRisk(data$simData, para)
head(rkiWithRisk)

## ---- runSimmer, echo = FALSE-------------------------------------------------
arrivalTimes <- data.frame(time = rkiWithRisk$time,
                             risk = rkiWithRisk$Risk)
envs <- babsimHospital(arrivalTimes = arrivalTimes,
                         conf = conf,
                         para = para)

## ---- plotSimmerThreeResources, eval = FALSE----------------------------------
#  resources <- get_mon_resources(envs)
#  resources$capacity <- resources$capacity/1e5
#  plot(resources, metric = "usage", c("bed", "intensiveBed", "intensiveBedVentilation"), items = "server")

## ---- plotBeds3, eval = FALSE-------------------------------------------------
#  plot(resources, metric = "usage", "bed", items = "server", steps = TRUE)

## ---- plotICU3, eval = FALSE--------------------------------------------------
#  plot(resources, metric = "usage", "intensiveBed", items = "server", steps = TRUE)

## ---- plotVent3, eval = FALSE-------------------------------------------------
#  plot(resources, metric = "usage", "intensiveBedVentilation", items = "server", steps = TRUE)

## ---- getErrsResults----------------------------------------------------------
fieldEvents <- getRealBeds(data = data$fieldData,
                        resource = conf$ResourceNames)
res <- getDailyMaxResults(envs = envs,  fieldEvents = fieldEvents, conf=conf)
resDefault <- getError(res, conf=conf)

## ---- plotSimAndResults, fig.caption="Output. Warning: incomplete data were used",  eval = FALSE----
#  p <- plotDailyMaxResults(res)
#  plot(p)

## ---- interactiveGGplotly, eval = FALSE---------------------------------------
#  plotly::ggplotly(p)

## ----eval=FALSE---------------------------------------------------------------
#  para <- babsimHospitalPara()

## ---- optimSpot, eval=FALSE---------------------------------------------------
#  library("babsim.hospital")
#  library("SPOT")
#  library("simmer")
#  dir.create("results")
#  res202010262 <- runoptDirect(
#    expName = paste0("test_", format(Sys.time(), "%Y_%b.%d_%H.%M_V"), utils::packageVersion("babsim.hospital"),"R"),
#    region = 5374,
#    rkiwerte = babsim.hospital::rkidata,
#    icuwerte = babsim.hospital::icudata,
#    TrainSimStartDate = Sys.Date() - 10*7,
#    TrainFieldStartDate = Sys.Date() - 6*7,
#    TestSimStartDate = Sys.Date() - 8*7,
#    TestFieldStartDate = Sys.Date() - 4*7,
#    Overlap = 0,
#    seed = 101170,
#    direct = TRUE,
#    repeats = 1,
#    funEvals = 40,
#    funEvalsFactor = 0,
#    size = 35,
#    simrepeats = 1,
#    parallel = TRUE,
#    percCores = 0.9,
#    icu = TRUE,
#    icuWeights = c(1,1),
#    verbosity=11,
#    testRepeats = 1,
#    tryOnTestSet = TRUE
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  para <- getBestParameter(getParaSet(5315))
#  res <- modelResultHospital(para=para,
#                             conf=conf,
#                             data = data)
#  resOpt <- getError(res, conf=conf)

## ---- eval = FALSE------------------------------------------------------------
#  p <- plotDailyMaxResults(res)
#  print(p)

## ---- showGgplotly, eval = FALSE----------------------------------------------
#  plotly::ggplotly(p)

## ----smooth, eval = FALSE-----------------------------------------------------
#  para <- smoothParameter(getBestParameter(getParaSet(5374)),
#                  getBestParameter(getParaSet(5)) )

## ---- showMatrixP-------------------------------------------------------------
para <- babsimHospitalPara()
getMatrixP(para = para)

## ---- visParaP, eval = FALSE--------------------------------------------------
#  visualizeGraph(para=para, option = "P")

## ---- showDurationMatrix, eval = FALSE----------------------------------------
#  visualizeGraph(para = para, option = "D")

## ---- showMatrixDurations-----------------------------------------------------
getMatrixD(para = para)

## ---- extendRkiDemo, eval = TRUE----------------------------------------------
data <- getRkiData(babsim.hospital::rkidata)
n <-  as.integer( max(data$Day)-min(data$Day) )
StartDay <- min(data$Day) + round(n*0.9)  
data <- data[which(data$Day >=  StartDay), ]
EndDate <- max(data$Day) + 2
dataExt <- extendRki(data = data, 
                     EndDate = EndDate,
                     R0 = c(0.1, 0.2))

## ---- eval = FALSE------------------------------------------------------------
#  visualizeRkiEvents(data = data, region=5374)

## ---- eval = FALSE------------------------------------------------------------
#  visualizeRkiEvents(data = dataExt, region = 5374)

## ---- quickAnalysis1, eval = FALSE--------------------------------------------
#  library("rpart")
#  library("rpart.plot")
#  library("babsim.hospital")
#  library("SPOT")
#  param <- getParaSet(5374)
#  n <- dim(param)[2] - 1
#  y <- param[,1]
#  x <- param[,2:dim(param)[2]]
#  fitTree <- buildTreeModel(x=x,
#                   y=y,
#                   control = list(xnames = paste0('x', 1:n)))
#  rpart.plot(fitTree$fit)

## ---- eval = FALSE------------------------------------------------------------
#  getParameterNameList(c(24, 25, 3, 10))

## ---- quickAnalysisKoeln, eval = FALSE----------------------------------------
#  library("rpart")
#  library("rpart.plot")
#  param <- getParaSet(5315)
#  n <- dim(param)[2] - 1
#  y <- param[,1]
#  x <- param[,2:dim(param)[2]]
#  fitTree <- buildTreeModel(x=x,
#                   y=y,
#                   control = list(xnames = paste0('x', 1:n)))
#  rpart.plot(fitTree$fit)

## ---- eval = FALSE------------------------------------------------------------
#  res <- res202010262[[2]][[1]]
#  xBest <- res$xbest
#  n <- length(xBest)
#  print(xBest)

## ---- eval =FALSE-------------------------------------------------------------
#  n <- n-1
#  t(getParameterNameList(1:n))

## ---- eval = FALSE------------------------------------------------------------
#  SPOT::plotModel(res$modelFit, which = c(16,2) ,xlab = c("Modellierungsparameter (Varianz), GammaShapeParameter", "x2: Normalstation zu Genesen (AmntDaysNormalToHealthy)"))

## ---- eval = FALSE------------------------------------------------------------
#  fitLm <- SPOT::buildLM(x=res$x,
#                   y=res$y,
#                   control = list(useStep=TRUE))
#  summary(fitLm$fit)

## ---- eval = FALSE------------------------------------------------------------
#  getParameterName(7)

## ---- eval = FALSE------------------------------------------------------------
#  library("rpart")
#  library("rpart.plot")
#  fitTree <- buildTreeModel(x=res$x,
#                   y=res$y,
#                   control = list(xnames = paste0('x', 1:n)))
#  rpart.plot(fitTree$fit)

## ---- estimateAB--------------------------------------------------------------
age <- c(2,10,25,47,70,90)
risk <- c(0.01,0.07,0.15,0.65,3,12.64)
fit <- nls(risk ~ a * exp( b * age), 
           start = list(a = 1, b = 0),
           control= nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/1024,
                                printEval = FALSE, warnOnly = FALSE))
print(coef(fit))

## ---- plotEstimatedAB, eval = FALSE-------------------------------------------
#  {plot(age,2*risk)
#      # female:
#      lines(age, 1* predict(fit, list(x = age)))
#      # male:
#      lines(age, 2* predict(fit, list(x = age) ), col ="red")}

## ---- dimRkidataFull, eval=FALSE----------------------------------------------
#  dim(babsim.hospital::rkidataFull)

## ---- dimRkidata, eval=TRUE---------------------------------------------------
str(babsim.hospital::rkidata)

## ---- getSimdataRki-----------------------------------------------------------
rkiSimData <- getRkiData(babsim.hospital::rkidata)
str(rkiSimData)

## ---- addRisktoRki------------------------------------------------------------
para <- babsimHospitalPara()
print(para$RiskFactorA)
print(para$RiskFactorB)
print(para$RiskMale)
rkiRiskSimData <- getRkiRisk(rkiSimData, para)
str(rkiRiskSimData)

## ---- generateArrivalEvent----------------------------------------------------
arrivalTimes <- data.frame(time = rkiRiskSimData$time, risk = rkiRiskSimData$Risk)
str(arrivalTimes)

## ---- eval = FALSE------------------------------------------------------------
#  getParameterDataFrame()

## ----getBounds----------------------------------------------------------------
bounds <- getBounds()
print(bounds)

## ---- eval=FALSE--------------------------------------------------------------
#  library("babsim.hospital")
#  library("SPOT")
#  library("simmer")
#  
#  runoptDirect(
#    expName = paste0("obk_", format(Sys.time(), "%Y_%b.%d_%H.%M_V"), utils::packageVersion("babsim.hospital"),"R"),
#    region = 5374,
#    rkiwerte = babsim.hospital::rkidata,
#    icuwerte = babsim.hospital::icudata,
#    TrainSimStartDate = Sys.Date() - 10*7, # 11*7, #10*7, # "2020-09-03",
#    TrainFieldStartDate = Sys.Date() - 6*7, # 8*7, # "2020-10-03",
#    #TestSimStartDate = Sys.Date() - 8*7, # 6*7 , #"2020-09-23",
#    #TestFieldStartDate = Sys.Date() - 4*7, #"2020-10-23",
#    Overlap = 0,
#    seed = 101170,
#    direct = TRUE,
#    repeats = 1000,
#    funEvals = 1000,
#    funEvalsFactor = 0,
#    size = 250,
#    simrepeats = 10,
#    parallel = TRUE,
#    percCores = 0.9,
#    icu = TRUE,
#    icuWeights = c(1,1),
#    verbosity=11,
#    testRepeats = 10,
#    tryOnTestSet = FALSE
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  library("babsim.hospital")
#  library("SPOT")
#  library("simmer")
#  
#  runoptDirect(
#    expName = paste0("koeln_", format(Sys.time(), "%Y_%b.%d_%H.%M_V"), utils::packageVersion("babsim.hospital"),"R"),
#    region = 5315,
#    rkiwerte = babsim.hospital::rkidata,
#    icuwerte = babsim.hospital::icudata,
#    TrainSimStartDate = Sys.Date() - 10*7, # 10*7, # "2020-09-03",
#    TrainFieldStartDate = Sys.Date() - 6*7, # "2020-10-03",
#    # TestSimStartDate = Sys.Date() - 8*7, # 6*7 , #"2020-09-23",
#    # TestFieldStartDate = Sys.Date() - 4*7, #"2020-10-23",
#    Overlap = 0,
#    seed = 101170,
#    direct = TRUE,
#    repeats = 1000,
#    funEvals = 1000,
#    funEvalsFactor = 0,
#    size = 250,
#    simrepeats = 10,
#    parallel = TRUE,
#    percCores = 0.9,
#    icu = TRUE,
#    icuWeights = c(1,1),
#    verbosity=11,
#    testRepeats = 10,
#    tryOnTestSet = FALSE
#  )

## ---- eval = FALSE------------------------------------------------------------
#  library("babsim.hospital")
#  library("SPOT")
#  library("simmer")
#  
#  runoptDirect(
#    expName = paste0("nrw_", format(Sys.time(), "%Y_%b.%d_%H.%M_V"), utils::packageVersion("babsim.hospital"),"R"),
#    region = 5374,
#    rkiwerte = babsim.hospital::rkidata,
#    icuwerte = babsim.hospital::icudata,
#    TrainSimStartDate = Sys.Date() - 10*7, #10*7, # "2020-09-03",
#    TrainFieldStartDate = Sys.Date() - 6*7, # "2020-10-03",
#    # TestSimStartDate = Sys.Date() - 8*7, #6*7 , #"2020-09-23",
#    # TestFieldStartDate = Sys.Date() - 4*7, #"2020-10-23",
#    Overlap = 0,
#    seed = 101170,
#    direct = TRUE,
#    repeats = 1000,
#    funEvals = 1000,
#    funEvalsFactor = 0,
#    size = 250,
#    simrepeats = 10,
#    parallel = TRUE,
#    percCores = 0.9,
#    icu = TRUE,
#    icuWeights = c(1,1),
#    verbosity=11,
#    testRepeats = 10,
#    tryOnTestSet = FALSE
#  )

