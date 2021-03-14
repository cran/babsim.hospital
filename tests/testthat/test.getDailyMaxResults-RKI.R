test_that("getDailyMaxResults works with rki data", {
  skip_if_quicktest()
  conf <- babsimToolsConf()
  conf$ResourceNames <- c("intensiveBed", "intensiveBedVentilation")
  conf$ResourceEval <- c("intensiveBed", "intensiveBedVentilation")
  conf$ICU <- TRUE
  set.seed(conf$seed)

  # RKI Data:
  RKI <- rkidata
  DIVI <- icudata
  # SIMSTART=  as.Date("2020-09-01")
  # SIMEND = as.Date("2020-09-30")
  SIMSTART <- as.Date(max(min(DIVI$daten_stand), as.Date(min(RKI$Refdatum))))
  SIMEND <- as.Date(min(max(DIVI$daten_stand), as.Date(max(RKI$Refdatum))))

  # DIVI Data

  FIELDSTART <- min(DIVI$daten_stand) # as.Date("2020-09-15")
  FIELDEND <- max(DIVI$daten_stand) # as.Date("2020-09-30")

  # Sim data:
  simData <- getRkiData(RKI)
  simData <- simData[which(simData$Day >= SIMSTART & simData$Day <= SIMEND), ]
  simData <- simData[which(simData$Day >= FIELDSTART & simData$Day <= FIELDEND), ]
  simData$time <- simData$time - min(simData$time)

  ## Field data:
  fieldData <- getIcuBeds(DIVI)
  fieldData <- fieldData[which(fieldData$Day >= FIELDSTART & fieldData$Day <= FIELDEND), ]
  fieldData <- fieldData[which(fieldData$Day >= SIMSTART & fieldData$Day <= SIMEND), ]

  data <- list(
    simData = simData,
    fieldData = fieldData
  )

  fieldEvents <- getRealBeds(
    data = fieldData,
    resource = conf$ResourceNames
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

  subRes <- res %>% filter(source == "babsim")
  expect_true((max(fieldEvents$time) - min(fieldEvents$time)) >= (max(subRes$time) - min(subRes$time)))
})
