


test_that("check that babsim function works without problems", {
  set.seed(123)
  require("simmer")
  require("dplyr")
  # load("data/hospitalCovid19_20200609.rda")
  x <- dataCovidBeds20200624
  arrivalTimes <- getArrivalTimes(x$Infected)
  y <- babsimHospital(
    arrivalTimes = arrivalTimes,
    conf = babsimToolsConf(),
    para = babsimHospitalPara()
  )
  resources <- get_mon_resources(y)
  resources <- resources %>% filter(resource != "nurse")
  expect_equal(resources$replication[1], 1)
})
