## install.packages("devtools")
## devtools::install_github("r-lib/devtools")

## url <- "http://owos.gm.fh-koeln.de:8055/bartz/spot.git"
## devtools::install_git(url = url)

## url <- "http://owos.gm.fh-koeln.de:8055/bartz/babsim.hospital"
## devtools::install_git(url = url, subdir = "babsim.hospital")

rm(list = ls())
suppressPackageStartupMessages({
library("SPOT")
library("babsim.hospital")
library("simmer")
library("simmer.plot")
library("plotly")
})

## packageVersion("SPOT")

library(readxl)
### X20201111UKdata <- read_excel("/Users/bartz/workspace/Lehre.d/IDEA-Master-AIT-WS2020-2021/Numerical-Methods/Datasets/CovidDataGroup1.xlsx")
X20201111UKdata <- read_excel("~bartz/workspace/Lehre.d/IDEA-Master-AIT-WS2020-2021/Numerical-Methods/Datasets/CovidDataGroup1.xlsx")
ukdataRawFull1 <- X20201111UKdata

str(ukdataRawFull1)

ukdataRaw <- ukdataRawFull1[as.Date(ukdataRawFull1$Date) > "2020-09-01",]
str(ukdataRaw)

simData <- data.frame(Day = as.Date(ukdataRaw$Date),
                      Infected = ukdataRaw$NewCases)

# plot(simData$Infected ~ simData$Day, type="b")

Day <- as.Date(ukdataRaw$Date)
bed <- ukdataRaw$TotalCOVID19Inpatient - ukdataRaw$COVID19NonInvasiveCPAP - ukdataRaw$COVID19VentilatedICU
intensiveBed <-  ukdataRaw$COVID19NonInvasiveCPAP
intensiveBedVentilation <- ukdataRaw$COVID19VentilatedICU
fieldData <- data.frame(Day = Day,
                        bed = bed,
                        intensiveBed = intensiveBed,
                        intensiveBedVentilation = intensiveBedVentilation)
str(fieldData)
max(fieldData$intensiveBed)

seed = 123
simrepeats = 2
parallel = FALSE
percCores = 0.8
resourceNames =  c("bed", "intensiveBed", "intensiveBedVentilation")
resourceEval = c("bed", "intensiveBed", "intensiveBedVentilation")

FieldStartDate = as.Date(min(fieldData$Day))
rownames(fieldData) <- NULL
icu = FALSE
icuWeights = c(1,2,10)

SimStartDate = FieldStartDate

data <- list(simData = simData,
 fieldData = fieldData)


# visualizeUK(data=data)

UKdata <- data
### usethis::use_data(UKdata, overwrite = TRUE)

conf <- babsimToolsConf()
conf$ResourceEval <- conf$ResourceNames
conf <- getConfFromData(conf = conf,
                        simData = data$simData,
                        fieldData = data$fieldData)
  conf$parallel = parallel
  conf$simRepeats = simrepeats
  conf$ICU = FALSE
  conf$ResourceNames = resourceNames
  conf$ResourceEval = resourceEval
  conf$percCores = percCores
  conf$logLevel = 0
  conf$w2 = icuWeights
  set.seed(conf$seed)

para <- babsimHospitalPara()
str(para)

arrivalTimes <- data.frame(time = getArrivalTimes(data$simData$Infected))

envs <- babsimHospital(arrivalTimes = arrivalTimes,
                         conf = conf,
                         para = para)

resources <- get_mon_resources(envs)
resources$capacity <- resources$capacity/1e5
plot(resources, metric = "usage", c("bed", "intensiveBed", "intensiveBedVentilation"), items = "server",steps= TRUE)

plot(resources, metric = "usage", "bed", items = "server", steps = TRUE)

plot(resources, metric = "usage", "intensiveBed", items = "server", steps = TRUE)

plot(resources, metric = "usage", "intensiveBedVentilation", items = "server", steps = TRUE)

fieldEvents <- getRealBeds(data = data$fieldData,
                        resource = conf$ResourceNames)
res <- getDailyMaxResults(envs = envs,  fieldEvents = fieldEvents, conf=conf)
resDefault <- getError(res, conf=conf)

# p <- plotDailyMaxResults(res, showBeds = TRUE)
# plot(p)

## ggplotly(p)

para <- babsimHospitalPara()
print(para)

## conf$simulationDates$StartDate
## conf$simulationDates$EndDate
## conf$fieldDates$StartDate
## conf$fieldDates$EndDate

library("babsim.hospital")
library("SPOT")
library("simmer")
studyDate <- as.Date( min(conf$simulationDates$EndDate, conf$fieldDates$EndDate ))
resUK <- runoptUK(
  expName = paste0("UK-", format(Sys.time(), "%Y-%b.%d-%H.%M-V"), utils::packageVersion("babsim.hospital")),
  simData = data$simData,
  fieldData = data$fieldData,
  TrainSimStartDate = studyDate - 10*7, # "2020-09-02",
  TrainFieldStartDate = studyDate - 8*7, # "2020-09-15",
  TestSimStartDate = studyDate - 6*7 , #"2020-09-30",
  TestFieldStartDate = studyDate - 4*7, #"2020-10-23",
  Overlap = 0,
  seed = 101170,
  direct = TRUE,
  repeats = 10,
  funEvals = 250,
  size = 50,
  simrepeats = 10,
  parallel = TRUE,
  percCores = 0.9,
  icu = FALSE,
  icuWeights = c(1,2,10),
  verbosity = 0,
  resourceNames =  c("bed", "intensiveBed", "intensiveBedVentilation"),
  resourceEval = c("bed", "intensiveBed", "intensiveBedVentilation")
)

## ukpara <- resUK[[1]]
## usethis::use_data(ukpara, overwrite = TRUE)

## print(ukpara)

## para <- getBestParameter(resUK[[1]])

# para <- getBestParameter(ukpara)
# print(para)
# 
# conf$simRepeats = 10
# res <- modelResultHospital(para=para, 
#                            conf=conf,
#                            data = data)
# resOpt <- getError(res, conf=conf)
# 
# p <- plotDailyMaxResults(res, showBeds = TRUE)
# print(p)
# 
# ggplotly(p)
# 
# visualizeGraph(para=para, option = "P")
# 
# getMatrixP(para = para )
# 
# visualizeGraph(para = para, option = "D")
# 
# getMatrixD(para = para)
