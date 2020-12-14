context("combine and evaluate result data")

test_that("getDailyMaxResults works with rki data", {
  conf = babsimToolsConf()
  conf$ResourceNames <- c("intensiveBed", "intensiveBedVentilation")
  conf$ResourceEval <- c("intensiveBed", "intensiveBedVentilation")
  conf$ICU = TRUE
  set.seed(conf$seed)
  
  # RKI Data:
  RKI = rkidata
  DIVI = icudata
  #SIMSTART=  as.Date("2020-09-01")
  #SIMEND = as.Date("2020-09-30")
  SIMSTART = as.Date(max( min(DIVI$daten_stand),   as.Date(min(RKI$Refdatum))))
  SIMEND = as.Date(min( max(DIVI$daten_stand), as.Date(max(RKI$Refdatum))))
  
  # DIVI Data
  
  FIELDSTART = min(DIVI$daten_stand) # as.Date("2020-09-15")
  FIELDEND = max(DIVI$daten_stand) # as.Date("2020-09-30")
  
  # Sim data:
  simData <- getRkiData(RKI)
  simData <- simData[which(simData$Day >= SIMSTART & simData$Day <= SIMEND),]
  simData <- simData[which(simData$Day >= FIELDSTART & simData$Day <= FIELDEND),]
  simData$time <- simData$time-min(simData$time)
  
  ## Field data:  
  fieldData <- getIcuBeds(DIVI)
  fieldData <- fieldData[which(fieldData$Day >= FIELDSTART & fieldData$Day <= FIELDEND),]
  fieldData <- fieldData[which(fieldData$Day >= SIMSTART & fieldData$Day <= SIMEND),]
  
  data <- list(simData = simData,
               fieldData = fieldData)
  
  fieldEvents <- getRealBeds(data=fieldData,
                             resource=conf$ResourceNames)
  
  ## Config
  conf <- getConfFromData(conf = conf,
                          simData = data$simData,
                          fieldData = data$fieldData) 
  
  ## Parameters:
  para = babsimHospitalPara()
  para <- checkSimPara(para)
  
  ## ArrivalTimes
  if (conf$ICU){
    rkiWithRisk <- getRkiRisk(data$simData, para)
    arrivalTimes <- data.frame(time = rkiWithRisk$time,
                               risk = rkiWithRisk$Risk)
  } else{
    arrivalTimes <- getArrivalTimes(data$simData$Infected)
  }
  data$arrivalTimes <- arrivalTimes
  
  ## Simulation:  
  envs <- babsimHospital(arrivalTimes = data$arrivalTimes,
                         conf = conf,
                         para = para)
  res <- getDailyMaxResults(envs = envs,  
                            fieldEvents = fieldEvents,
                            conf = conf) 
  
  subRes <- res %>% filter(source == "babsim")
  expect_true(   ( max(fieldEvents$time) - min(fieldEvents$time)) >= ( max(subRes$time) - min(subRes$time)))
}
)

test_that("getDailyMaxResults for OBK Data",{
  para = babsimHospitalPara()
  conf = babsimToolsConf()
  ICU = FALSE
  set.seed(conf$seed)
  # 1. Generate simulation data based on number of infected persons per day: 
  data <- dataCovidBeds20200624
  conf$simulationDates$StartDate <- min(data$Day)
  ## Trifft jemand am ersten Tag ein, dann ist die arrivalTime < 1, z.B.:
  ## 0.5, wenn dies um 12 Uhr mittags passiert.
  arrivalTimes <- getArrivalTimes(data$Infected) 
  # simStartDate <- min(data$Day)
  envs <- babsimHospital(arrivalTimes = arrivalTimes, 
                         conf = conf,
                         para = para)
  # 2. Make real data compatible with the simmer simulation environment:
  fieldData <- getRealBeds(data = data, 
                           resource=c("bed", "intensiveBed", "intensiveBedVentilation"))
  # 3. Combine simulated and real data:
  res <- getDailyMaxResults(envs = envs,  
                            fieldEvents = fieldData, 
                            conf = conf)
  ## testing:
  resources <- get_mon_resources(envs)
  resources$time <- round(resources$time) 
  min(resources$time)
  max(resources$time)
  min(fieldData$time)
  max(fieldData$time)
  StartDate <- as.Date(min(fieldData$date))
  EndDate <- as.Date(max(fieldData$date))
  data <-  babsim.hospital::dataCovidBeds20200624
  conf$simulationDates$StartDate <- min(data$Day)
  ## offset should be zero for OBK data:
  amntDaysLateStart <- as.numeric(StartDate-conf$simulationDates$StartDate)
  resources <- resources %>% dplyr::filter(time >= amntDaysLateStart)
  ## Check: are these days? EndDate-StartDate might be one day?
  observedPeriod <- as.numeric(EndDate-StartDate)
  ## StartDate is not needed:
  ## StartDate-simStartDate + EndDate-StartDate = EndDate -simStartDate
  finalTimeICU <- amntDaysLateStart + observedPeriod
  resources <- resources %>% dplyr::filter(time <= finalTimeICU)
  
  ## Resource requirement of a day is set to the maximum of that day
  ### not needed: 
  ### resourcesMaxSystem1 <- resources %>% group_by(resource, time, replication) %>% slice(which.max(system))
  ## If there are replicates, look for median, worst and best case scenario
  resourcesMaxSystem <- resources %>% dplyr::group_by(resource, time) %>%  dplyr::mutate(upper = max(system)) %>% dplyr::mutate(lower = min(system)) %>% dplyr::mutate(med = median(system))
  unique(resources$time == resourcesMaxSystem$time)
  expect_true( unique( resources$time == resourcesMaxSystem$time) )
  
  # resourcesMaxSystem$date <- as.Date( as.POSIXct((resourcesMaxSystem$time)*24*60*60,origin=StartDate) ) 
  resourcesMaxSystem$date <- as.Date( as.POSIXct((resourcesMaxSystem$time)*24*60*60,origin= conf$simulationDates$StartDate) ) 
  
  expect_true ( StartDate <= min(resourcesMaxSystem$date) )
  expect_true ( EndDate >= max(resourcesMaxSystem$date) )
  
  # resourcesMaxSystem$rwdate<-round_date(resourcesMaxSystem$date,unit="week")
  n <- dim(fieldData)[1]
  fieldData$server <- fieldData$med
  fieldData$queue <- rep(0,n)
  fieldData$capacity <- rep(Inf,n)
  fieldData$queue_size <- rep(Inf,n)
  fieldData$system <- fieldData$med
  fieldData$limit <- rep(Inf, n)
  fieldData$replication <- rep(1,n)
  fieldData$upper <- fieldData$med
  fieldData$lower <- fieldData$med
  ## Add simulations from other sources to plots
  resourcesMaxSystem$source = "babsim"
  # resourcesMaxSystem <- resourcesMaxSystem %>% filter(resource != "nurse")
  resourcesMaxSystem <- dplyr::bind_rows(resourcesMaxSystem, fieldData)
  expect_true ( StartDate <= min(resourcesMaxSystem$date) )
  expect_true ( EndDate >= max(resourcesMaxSystem$date) )
})

test_that("getDailyMaxResults for ICU Data",{
  para = babsimHospitalPara()
  conf = babsimToolsConf()
  conf$ICU = TRUE
  set.seed(conf$seed)
  # 1. Generate simulation data based on number of infected persons per day: 
  data <- tail(rkidata,1000)  
  # simStartDate <- min(as.Date(data$Refdatum))
  conf$simulationDates$StartDate = min(as.Date(data$Refdatum))
  ## Trifft jemand am ersten Tag ein, dann ist die arrivalTime < 1, z.B.:
  ## 0.5, wenn dies um 12 Uhr mittags passiert.
  arrivalTimes <- rkiToBabsimArrivals(data)
  envs <- babsimHospital(arrivalTimes = arrivalTimes, 
                         conf = conf,
                         para = para)
  # 2. Make real data compatible with the simmer simulation environment:
  # data <- babsim.hospital::dataICUBeds20200821
  data <- getIcuBeds(babsim.hospital::icudata)
  names(data) <- c("intensiveBed", "intensiveBedVentilation", "Day")
  fieldData <- getRealBeds(data = data, 
                           resource=c("intensiveBed", "intensiveBedVentilation"))
  # 3. Combine simulated and real data:
  res <- getDailyMaxResults(envs = envs,  
                            fieldEvents = fieldData,
                            conf = conf)
  ## testing:
  resources <- get_mon_resources(envs)
  resources$time <- round(resources$time) 
  min(resources$time)
  max(resources$time)
  min(fieldData$time)
  max(fieldData$time)
  StartDate <- as.Date(min(fieldData$date))
  EndDate <- as.Date(max(fieldData$date))
  
  ## RKI data based simulations start earlier (when no ICU test data are available).
  ## These days without corresponding test/real world data will be cut off, e.g.,
  ## they are considered as an offset:
  amntDaysLateStart <- as.numeric(StartDate-conf$simulationDates$StartDate)
  resources <- resources %>% dplyr::filter(time >= amntDaysLateStart)
  ## Check: are these days? EndDate-StartDate might be one day?
  observedPeriod <- as.numeric(EndDate-StartDate)
  ## StartDate is not needed:
  ## StartDate-simStartDate + EndDate-StartDate = EndDate -simStartDate
  finalTimeICU <- amntDaysLateStart + observedPeriod
  resources <- resources %>% dplyr::filter(time <= finalTimeICU)
  
  ## ICU data treat intensiveBed as bed:
  # if(conf$ICU) resources$resource <- gsub('intensiveBed', 'bed', resources$resource)
  # if(conf$ICU) resources$resource <- gsub('bedVentilation', 'intensiveBedVentilation', resources$resource)
  
  ## Resource requirement of a day is set to the maximum of that day
  ### not needed: 
  ### resourcesMaxSystem1 <- resources %>% group_by(resource, time, replication) %>% slice(which.max(system))
  ## If there are replicates, look for median, worst and best case scenario
  resourcesMaxSystem <- resources %>% dplyr::group_by(resource, time) %>%  dplyr::mutate(upper = max(system)) %>% dplyr::mutate(lower = min(system)) %>% dplyr::mutate(med = median(system))
  unique(resources$time == resourcesMaxSystem$time)
  expect_true( unique( resources$time == resourcesMaxSystem$time) )
  
  # resourcesMaxSystem$date <- as.Date( as.POSIXct((resourcesMaxSystem$time)*24*60*60,origin=StartDate) )
  resourcesMaxSystem$date <- as.Date( as.POSIXct((resourcesMaxSystem$time)*24*60*60,origin=conf$simulationDates$StartDate) )
  
  expect_true ( StartDate <= min(resourcesMaxSystem$date) )
  expect_true ( EndDate >= max(resourcesMaxSystem$date) )
  
  # resourcesMaxSystem$rwdate<-round_date(resourcesMaxSystem$date,unit="week")
  n <- dim(fieldData)[1]
  fieldData$server <- fieldData$med
  fieldData$queue <- rep(0,n)
  fieldData$capacity <- rep(Inf,n)
  fieldData$queue_size <- rep(Inf,n)
  fieldData$system <- fieldData$med
  fieldData$limit <- rep(Inf, n)
  fieldData$replication <- rep(1,n)
  fieldData$upper <- fieldData$med
  fieldData$lower <- fieldData$med
  ## Add simulations from other sources to plots
  resourcesMaxSystem$source = "babsim"
  # resourcesMaxSystem <- resourcesMaxSystem %>% filter(resource != "nurse")
  resourcesMaxSystem <- dplyr::bind_rows(resourcesMaxSystem, fieldData)
  expect_true( StartDate <= min(resourcesMaxSystem$date) )
  expect_true( EndDate >= max(resourcesMaxSystem$date) )
})

test_that("getError for OBK Data",{
  obk <- dataCovidBeds20200624
  GAData <- data.frame(bed=obk$bed, 
                       intensiveBed=obk$intensiveBed, 
                       intensiveBedVentilation=obk$intensiveBedVentilation,
                       Day = obk$Day)
  GABeds <- getRealBeds(data = GAData,  
                        resource=c("bed", "intensiveBed", "intensiveBedVentilation"))
  para <- babsimHospitalPara()
  conf <- babsimToolsConf()
  conf <- getConfFromData(conf = conf,
                          simData = obk,
                          fieldData = GAData)
  conf$parallel <- FALSE
  arrivalTimes <- getArrivalTimes(obk$Infected) 
  
  y <- babsimHospital(arrivalTimes = arrivalTimes
                      , conf = conf
                      , para = para
  )
  
  res <- getDailyMaxResults(
    envs = y,  
    fieldEvents = GABeds,
    conf = conf)
  getError(res = res, conf = conf)
  ## Error should be positive 
  expect_true( getError(res=res, conf=conf) >= 0 )
  ## Use babsim as simulation and test data => error should be zero:
  resBab <- res[res$source == "babsim", ]
  resGA <- resBab
  resGA$source <- "GA"
  resIdentical <- dplyr::bind_rows(resBab, resGA)
  expect_true( getError(res = resIdentical, conf = conf) == 0 )
})


test_that("getIcuError for ICU Data",{
  set.seed(123)
  conf = babsimToolsConf()
  conf$ResourceNames <- c("intensiveBed", "intensiveBedVentilation")
  conf$ICU = TRUE
  
  # RKI Data:
  RKI = rkidata
  SIMSTART=  as.Date("2020-09-01")
  SIMEND = as.Date("2020-09-30")
  
  # DIVI Data
  DIVI = icudata
  FIELDSTART = as.Date("2020-09-15")
  FIELDEND = as.Date("2020-09-30")
  
  # Sim data:
  simData <- getRkiData(RKI)
  simData <- simData[which(simData$Day >= SIMSTART & simData$Day <= SIMEND),]
  simData$time <- simData$time-min(simData$time)
  
  ## Field data:  
  fieldData <- getIcuBeds(DIVI)
  fieldData <- fieldData[which(fieldData$Day >= FIELDSTART & fieldData$Day <= FIELDEND),]
  
  fieldEvents <- getRealBeds(data=fieldData,
                             resource=conf$ResourceNames)
  data <- list(simData = simData,
               fieldData = fieldData)
  
  ## Config
  conf <- getConfFromData(conf = conf,
                          simData = data$simData,
                          fieldData = data$fieldData) 
  
  ## Parameters:
  para = babsimHospitalPara()
  para <- checkSimPara(para)
  
  ## ArrivalTimes
  if (conf$ICU){
    rkiWithRisk <- getRkiRisk(data$simData, para)
    arrivalTimes <- data.frame(time = rkiWithRisk$time,
                               risk = rkiWithRisk$Risk)
  } else{
    arrivalTimes <- getArrivalTimes(data$simData$Infected)
  }
  data$arrivalTimes <- arrivalTimes
  
  ## Simulation:  
  envs <- babsimHospital(arrivalTimes = data$arrivalTimes,
                         conf = conf,
                         para = para)
  res <- getDailyMaxResults(envs = envs,  
                            fieldEvents = fieldEvents,
                            conf = conf) 
  ## Error should be positive 
  # expect_true( getIcuError(res) >= 0 )
  expect_true( getError(res = res, conf = conf) >= 0 )
  ## Use babsim as simulation and test data => error should be zero:
  resBab <- res[res$source == "babsim", ]
  resICU <- resBab
  resICU$source <- "GA"
  resIdentical <- dplyr::bind_rows(resBab, resICU)
  expect_true( getError(res= resIdentical, conf = conf) == 0 )
})



