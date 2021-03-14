test_that("getDailyMaxResults for ICU Data", {
  para <- babsimHospitalPara()
  conf <- babsimToolsConf()
  conf$ICU <- TRUE
  set.seed(conf$seed)
  # 1. Generate simulation data based on number of infected persons per day:
  data <- tail(rkidata, 1000)
  # simStartDate <- min(as.Date(data$Refdatum))
  conf$simulationDates$StartDate <- min(as.Date(data$Refdatum))
  ## Trifft jemand am ersten Tag ein, dann ist die arrivalTime < 1, z.B.:
  ## 0.5, wenn dies um 12 Uhr mittags passiert.
  arrivalTimes <- rkiToBabsimArrivals(data)
  envs <- babsimHospital(
    arrivalTimes = arrivalTimes,
    conf = conf,
    para = para
  )
  # 2. Make real data compatible with the simmer simulation environment:
  # data <- babsim.hospital::dataICUBeds20200821
  data <- getIcuBeds(babsim.hospital::icudata)
  names(data) <- c("intensiveBed", "intensiveBedVentilation", "Day")
  fieldData <- getRealBeds(
    data = data,
    resource = c("intensiveBed", "intensiveBedVentilation")
  )
  # 3. Combine simulated and real data:
  res <- getDailyMaxResults(
    envs = envs,
    fieldEvents = fieldData,
    conf = conf
  )
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
  amntDaysLateStart <- as.numeric(StartDate - conf$simulationDates$StartDate)
  resources <- resources %>% dplyr::filter(time >= amntDaysLateStart)
  ## Check: are these days? EndDate-StartDate might be one day?
  observedPeriod <- as.numeric(EndDate - StartDate)
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
  resourcesMaxSystem <- resources %>%
    dplyr::group_by(resource, time) %>%
    dplyr::mutate(upper = max(system)) %>%
    dplyr::mutate(lower = min(system)) %>%
    dplyr::mutate(med = median(system))
  unique(resources$time == resourcesMaxSystem$time)
  expect_true(unique(resources$time == resourcesMaxSystem$time))

  # resourcesMaxSystem$date <- as.Date( as.POSIXct((resourcesMaxSystem$time)*24*60*60,origin=StartDate) )
  resourcesMaxSystem$date <- as.Date(as.POSIXct((resourcesMaxSystem$time) * 24 * 60 * 60, origin = conf$simulationDates$StartDate))

  expect_true(StartDate <= min(resourcesMaxSystem$date))
  expect_true(EndDate >= max(resourcesMaxSystem$date))

  # resourcesMaxSystem$rwdate<-round_date(resourcesMaxSystem$date,unit="week")
  n <- dim(fieldData)[1]
  fieldData$server <- fieldData$med
  fieldData$queue <- rep(0, n)
  fieldData$capacity <- rep(Inf, n)
  fieldData$queue_size <- rep(Inf, n)
  fieldData$system <- fieldData$med
  fieldData$limit <- rep(Inf, n)
  fieldData$replication <- rep(1, n)
  fieldData$upper <- fieldData$med
  fieldData$lower <- fieldData$med
  ## Add simulations from other sources to plots
  resourcesMaxSystem$source <- "babsim"
  # resourcesMaxSystem <- resourcesMaxSystem %>% filter(resource != "nurse")
  resourcesMaxSystem <- dplyr::bind_rows(resourcesMaxSystem, fieldData)
  expect_true(StartDate <= min(resourcesMaxSystem$date))
  expect_true(EndDate >= max(resourcesMaxSystem$date))
})

