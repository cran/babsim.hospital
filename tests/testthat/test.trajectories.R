
context("babsim.hospital trajectories")

test_that("scenario from ex2 in the babsim.hospital-vignette: everybody is sent home, no beds required", {
  require("simmer")
  require("dplyr")
  ex2I <- ex1InfectedDf
  ex2A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected) )
  conf <- babsimToolsConf()
  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 0.0
  y <- babsimHospital(arrivalTimes = ex2A
                      , conf = conf
                      , para = para)
  #
  resources <- get_mon_resources(y)
	expect_equal(nrow(resources), 0)
 })

test_that("scenario from ex3 in the babsim.hospital-vignette: $N$ infected patients arrive.
Everybody goes to the normal station and recovers. Only $N$ beds are occupied.", {
  set.seed(123)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N = 5
  conf <- babsimToolsConf()
  ex3I <- ex1InfectedDf
  ex3A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected) )
  ex3A <- data.frame(time = ex3A[1:N,])
  
  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 1.0
  #
  para$FactorPatientsNormalToIntensive <- 0.0
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 1.0
  
  y <- babsimHospital(arrivalTimes = ex3A
                      , conf = conf
                      , para = para)
  #
  resources <- get_mon_resources(y)
  s <- resources$server
  s <- c(0,s)
  d <- diff(s)
  expect_equal(N , sum(d[d > 0]) )
})

test_that("scenario from ex4 in the babsim.hospital-vignette: $N$ infected patients arrive.
Everybody goes to the 1. normal station, then 2. intensive station 3. ventilation 
4. intensiveAfter 5. aftercare and is healed.",{
N = 3
conf <- babsimToolsConf()
ex4I <- ex1InfectedDf
ex4A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected) )
ex4A <- data.frame(time = ex4A[1:N,])

para <- babsimHospitalPara()
para$FactorPatientsInfectedToHospital <- 1.0
#
para$FactorPatientsHospitalToIntensive <- 0.0
para$FactorPatientsHospitalToVentilation <- 0.0
para$FactorPatientsHospitalToNormal <- 1.0
#
para$FactorPatientsNormalToIntensive <- 1.0
para$FactorPatientsNormalToVentilation <- 0.0
para$FactorPatientsNormalToDeath <- 0.0
para$FactorPatientsNormalToHealthy <- 0.0
#
para$FactorPatientsIntensiveToVentilation <- 1.0
para$FactorPatientsIntensiveToAftercare <- 0.0
para$FactorPatientsIntensiveToDeath <- 0.0
para$FactorPatientsIntensiveToHealthy <- 0.0
#
para$FactorPatientsVentilationToIntensiveAfter <- 1.0
para$FactorPatientsVentilationToAftercare <- 0.0
para$FactorPatientsVentilationToDeath <- 0.0
#
para$FactorPatientsIntensiveAfterToHealthy <- 0.0
para$FactorPatientsIntensiveAfterToDeath <- 0.0

conf$logLevel = 0
y <- babsimHospital(arrivalTimes = ex4A
                    , conf = conf
                    , para = para)
#
resources <- get_mon_resources(y)
s <- resources[resources$resource=="bed",]$server
s <- c(0,s)
d <- diff(s)
expect_equal(2*N , sum(d[d > 0]) )
})

test_that("ICU data based scenario: $N$ infected patients arrive.
Everybody goes to the 1. normal station, then 2. intensive station 3. ventilation 
4. intensiveAfter 5. aftercare and is healed.",{
  conf <- babsimToolsConf()
  conf$parallel = FALSE
  # conf$maxCapacity = 1e6
  rkiwerte <- babsim.hospital::rkidata
  #rki <- rki[as.Date(rki$Refdatum) > as.Date("2020-05-01") & as.Date(rki$Refdatum) < as.Date("2020-06-01"), ]
  #arrivalTimes <- rkiToBabsimArrivals(rki) 
  ## for CRAN use : region = 0 
  region = 5374
  rkiwerte <- getRegionRki(data = rkiwerte,
                           region = region)
  
  simData <- getRkiData(rkiwerte)
  ## simData <- simData[which(simData$Day >= as.Date(SimStartDate)), ]
  ## Auch mit fieldData cutten damit es immer das gleiche Datum ist
  ## simData <-
  ##  simData[as.Date(simData$Day) <= max(as.Date(fieldData$Day)),]
  
  ## time muss bei 1 starten
  simData$time <- simData$time - min(simData$time)
  rownames(simData) <- NULL
  para <- babsimHospitalPara()
  rkiWithRisk <- getRkiRisk(simData, para)
  arrivalTimes <- data.frame(time = rkiWithRisk$time,
                             risk = rkiWithRisk$Risk)
  
  N <- nrow(arrivalTimes)
  # check for unique values (not necessary any more):
  ### expect_equal( sum(duplicated(arrivalTimes$time)), 0)
  #
  # Minus 1877 works:
  # time
  # 230181 217.9953
  # 230182 217.9963
  # 230183 217.9980
  # 230184 217.9985
  # 230185 217.9987
  # 230186 217.9997
  #N <- nrow(arrivalTimes) - 1877
  # Minus 1876 does not work:
  # N <- nrow(arrivalTimes) - 1876
  # 230182 217.9936
  # 230183 217.9943
  # 230184 217.9956
  # 230185 217.9976
  # 230186 217.9986
  # 230187 217.9995
  # N <- 232063
  # arrivalTimes <- data.frame(time = arrivalTimes[1:N,])
  
  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 1.0
  #
  para$FactorPatientsNormalToIntensive <- 1.0
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.0
  #
  para$FactorPatientsIntensiveToVentilation <- 1.0
  para$FactorPatientsIntensiveToAftercare <- 0.0
  para$FactorPatientsIntensiveToDeath <- 0.0
  para$FactorPatientsIntensiveToHealthy <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 1.0
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 0.0
  #
  para$FactorPatientsIntensiveAfterToHealthy <- 0.0
  para$FactorPatientsIntensiveAfterToDeath <- 0.0
  
  conf$logLevel = 0
  y <- babsimHospital(arrivalTimes = arrivalTimes
                      , conf = conf
                      , para = para)
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource=="bed",]$server
  s <- c(0,s)
  d <- diff(s)
  # 2*N == sum(d[d > 0])
  expect_equal(2*N , sum(d[d > 0]) )
})

