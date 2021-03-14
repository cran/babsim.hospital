test_that("getError for OBK Data", {
  obk <- dataCovidBeds20200624
  GAData <- data.frame(
    bed = obk$bed,
    intensiveBed = obk$intensiveBed,
    intensiveBedVentilation = obk$intensiveBedVentilation,
    Day = obk$Day
  )
  GABeds <- getRealBeds(
    data = GAData,
    resource = c("bed", "intensiveBed", "intensiveBedVentilation")
  )
  para <- babsimHospitalPara()
  conf <- babsimToolsConf()
  conf <- getConfFromData(
    conf = conf,
    simData = obk,
    fieldData = GAData
  )
  conf$parallel <- FALSE
  arrivalTimes <- getArrivalTimes(obk$Infected)

  y <- babsimHospital(
    arrivalTimes = arrivalTimes,
    conf = conf,
    para = para
  )

  res <- getDailyMaxResults(
    envs = y,
    fieldEvents = GABeds,
    conf = conf
  )
  getError(res = res, conf = conf)
  ## Error should be positive
  expect_true(getError(res = res, conf = conf) >= 0)
  ## Use babsim as simulation and test data => error should be zero:
  resBab <- res[res$source == "babsim", ]
  resGA <- resBab
  resGA$source <- "GA"
  resIdentical <- dplyr::bind_rows(resBab, resGA)
  expect_true(getError(res = resIdentical, conf = conf) == 0)
})
