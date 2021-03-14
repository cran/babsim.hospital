test_that("getIcuError for ICU Data", {
  set.seed(123)
  conf <- babsimToolsConf()
  conf$ResourceNames <- c("intensiveBed", "intensiveBedVentilation")
  conf$ICU <- TRUE

  # RKI Data:
  RKI <- rkidata
  SIMSTART <- as.Date("2020-09-01")
  SIMEND <- as.Date("2020-09-30")

  # DIVI Data
  DIVI <- icudata
  FIELDSTART <- as.Date("2020-09-15")
  FIELDEND <- as.Date("2020-09-30")

  # Sim data:
  simData <- getRkiData(RKI)
  simData <- simData[which(simData$Day >= SIMSTART & simData$Day <= SIMEND), ]
  simData$time <- simData$time - min(simData$time)

  ## Field data:
  fieldData <- getIcuBeds(DIVI)
  fieldData <- fieldData[which(fieldData$Day >= FIELDSTART & fieldData$Day <= FIELDEND), ]

  fieldEvents <- getRealBeds(
    data = fieldData,
    resource = conf$ResourceNames
  )
  data <- list(
    simData = simData,
    fieldData = fieldData
  )

  ## Config
  conf <- getConfFromData(
    conf = conf,
    simData = data$simData,
    fieldData = data$fieldData
  )

  ## Parameters:
  para <- babsimHospitalPara()
  para <- checkSimPara(para)

  ## ArrivalTimes
  if (conf$ICU) {
    rkiWithRisk <- getRkiRisk(data$simData, para)
    arrivalTimes <- data.frame(
      time = rkiWithRisk$time,
      risk = rkiWithRisk$Risk
    )
  } else {
    arrivalTimes <- getArrivalTimes(data$simData$Infected)
  }
  data$arrivalTimes <- arrivalTimes

  ## Simulation:
  envs <- babsimHospital(
    arrivalTimes = data$arrivalTimes,
    conf = conf,
    para = para
  )
  res <- getDailyMaxResults(
    envs = envs,
    fieldEvents = fieldEvents,
    conf = conf
  )
  ## Error should be positive
  # expect_true( getIcuError(res) >= 0 )
  expect_true(getError(res = res, conf = conf) >= 0)
  ## Use babsim as simulation and test data => error should be zero:
  resBab <- res[res$source == "babsim", ]
  resICU <- resBab
  resICU$source <- "GA"
  resIdentical <- dplyr::bind_rows(resBab, resICU)
  expect_true(getError(res = resIdentical, conf = conf) == 0)
})
