test_that("everybody is sent home, no beds required", {
  skip_if_quicktest()
  require("simmer")
  require("dplyr")
  ex0I <- ex1InfectedDf
  ex0A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  conf <- babsimToolsConf()
  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 0.0
  y <- babsimHospital(
    arrivalTimes = ex0A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  expect_equal(nrow(resources), 0)
})

#########################################################################

test_that("$N$ infected patients arrive.
normal station -> healthy", {
  skip_if_quicktest()
  set.seed(123)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 5
  conf <- babsimToolsConf()
  ex2I <- ex1InfectedDf
  ex2A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex2A <- data.frame(time = ex2A[1:N, ])

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

  y <- babsimHospital(
    arrivalTimes = ex2A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
})

########################################################################

test_that("$N$ infected patients arrive.
normal station -> intensive station -> ventilation -> intensiveafter -> death", {
  skip_if_quicktest()
  N <- 7
  conf <- babsimToolsConf()
  ex3I <- ex1InfectedDf
  ex3A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex3A <- data.frame(time = ex3A[1:N, ])

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
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.0
  para$FactorPatientsIntensiveAfterToHealthy <- 0.0
  para$FactorPatientsIntensiveAfterToDeath <- 1.0

  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex3A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(2 * N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
})

########################################################################

test_that("$N$ infected patients arrive.
normal station -> intensive station -> ventilation -> intensiveafter -> aftercare", {
  skip_if_quicktest()
  N <- 4
  conf <- babsimToolsConf()
  ex4I <- ex1InfectedDf
  ex4A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex4A <- data.frame(time = ex4A[1:N, ])

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
  # para$FactorPatientsIntensiveAfterToAftercare <- 1.0
  para$FactorPatientsIntensiveAfterToHealthy <- 0.0
  para$FactorPatientsIntensiveAfterToDeath <- 0.0

  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex4A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(2 * N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(2 * N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
})

########################################################################

test_that("$N$ infected patients arrive.
normal station -> intensive station -> ventilation -> intensiveafter -> aftercare", {
  skip_if_quicktest()
  N <- 4
  conf <- babsimToolsConf()
  ex4.1I <- ex1InfectedDf
  ex4.1A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex4.1A <- data.frame(time = ex4.1A[1:N, ])

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
  # para$FactorPatientsIntensiveAfterToAftercare <- 1.0
  para$FactorPatientsIntensiveAfterToHealthy <- 0.0
  para$FactorPatientsIntensiveAfterToDeath <- 0.0

  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex4.1A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(2 * N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(2 * N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
})

########################################################################

test_that("$N$ infected patients arrive.
normal station -> intensive station -> aftercare", {
  skip_if_quicktest()
  N <- 7
  conf <- babsimToolsConf()
  ex5I <- ex1InfectedDf
  ex5A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex5A <- data.frame(time = ex5A[1:N, ])

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
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 1.0
  para$FactorPatientsIntensiveToDeath <- 0.0
  para$FactorPatientsIntensiveToHealthy <- 0.0


  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex5A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(2 * N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
})

########################################################################

########################################################################

test_that("$N$ infected patients arrive.
normal station -> intensive station -> death", {
  skip_if_quicktest()
  N <- 12
  conf <- babsimToolsConf()
  ex7I <- ex1InfectedDf
  ex7A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex7A <- data.frame(time = ex7A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  #
  para$FactorPatientsNormalToIntensive <- 1.0
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 0.0
  para$FactorPatientsIntensiveToDeath <- 1.0


  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex7A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
})

########################################################################

test_that("$N$ infected patients arrive.
normal station -> intensive station -> ventilation -> death", {
  skip_if_quicktest()
  N <- 3
  conf <- babsimToolsConf()
  ex8I <- ex1InfectedDf
  ex8A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex8A <- data.frame(time = ex8A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  #
  para$FactorPatientsNormalToIntensive <- 1.0
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  #
  para$FactorPatientsIntensiveToVentilation <- 1.0
  para$FactorPatientsIntensiveToDeath <- 0.0
  para$FactorPatientsIntensiveToHealthy <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 0.0

  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex8A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
})



########################################################################

test_that("$N$ infected patients arrive.
normal station -> death", {
  skip_if_quicktest()
  N <- 4
  conf <- babsimToolsConf()
  ex10I <- ex1InfectedDf
  ex10A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex10A <- data.frame(time = ex10A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 1.0
  #
  para$FactorPatientsNormalToIntensive <- 0.0
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 1.0
  para$FactorPatientsNormalToHealthy <- 0.0

  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex10A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
})

########################################################################

test_that("$N$ infected patients arrive.
normal station -> ventilation -> death", {
  skip_if_quicktest()
  N <- 2
  conf <- babsimToolsConf()
  ex11I <- ex1InfectedDf
  ex11A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex11A <- data.frame(time = ex11A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 1.0
  #
  para$FactorPatientsNormalToIntensive <- 0.0
  para$FactorPatientsNormalToVentilation <- 1.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 0.0
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 1.0

  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex11A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
})

########################################################################

########################################################################

########################################################################


########################################################################

test_that("$N$ infected patients arrive.
normal station -> ventilation -> intensiveafter -> death", {
  skip_if_quicktest()
  N <- 2
  conf <- babsimToolsConf()
  ex15I <- ex1InfectedDf
  ex15A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex15A <- data.frame(time = ex15A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 1.0
  #
  para$FactorPatientsNormalToIntensive <- 0.0
  para$FactorPatientsNormalToVentilation <- 1.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 1.0
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 0.0
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.0
  para$FactorPatientsIntensiveAfterToHealthy <- 0.0
  para$FactorPatientsIntensiveAfterToDeath <- 1.0

  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex15A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
})


########################################################################

########################################################################

test_that("$N$ infected patients arrive.
intensive station -> aftercare", {
  skip_if_quicktest()
  N <- 5
  conf <- babsimToolsConf()
  ex17I <- ex1InfectedDf
  ex17A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex17A <- data.frame(time = ex17A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 1.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 0.0
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 1.0
  para$FactorPatientsIntensiveToDeath <- 0.0
  para$FactorPatientsIntensiveToHealthy <- 0.0
  #


  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex17A,
    conf = conf,
    para = para
  )

  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
})


########################################################################

test_that("$N$ infected patients arrive.
intensive station -> ventilation -> intensiveafter -> aftercare", {
  skip_if_quicktest()
  N <- 8
  conf <- babsimToolsConf()
  ex18I <- ex1InfectedDf
  ex18A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex18A <- data.frame(time = ex18A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 1.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 0.0
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
  # para$FactorPatientsIntensiveAfterToAftercare <- 1.0
  para$FactorPatientsIntensiveAfterToHealthy <- 0.0
  para$FactorPatientsIntensiveAfterToDeath <- 0.0

  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex18A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(2 * N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
})

########################################################################

test_that("$N$ infected patients arrive.
intensive station -> ventilation -> intensiveafter -> death", {
  skip_if_quicktest()
  N <- 4
  conf <- babsimToolsConf()
  ex20I <- ex1InfectedDf
  ex20A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex20A <- data.frame(time = ex20A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 1.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 0.0
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
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.0
  para$FactorPatientsIntensiveAfterToHealthy <- 0.0
  para$FactorPatientsIntensiveAfterToDeath <- 1.0

  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex20A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(2 * N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
})

########################################################################

test_that("$N$ infected patients arrive.
intensive station -> ventilation -> death", {
  skip_if_quicktest()
  N <- 7
  conf <- babsimToolsConf()
  ex21I <- ex1InfectedDf
  ex21A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex21A <- data.frame(time = ex21A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 1.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 0.0
  #
  para$FactorPatientsIntensiveToVentilation <- 1.0
  para$FactorPatientsIntensiveToAftercare <- 0.0
  para$FactorPatientsIntensiveToDeath <- 0.0
  para$FactorPatientsIntensiveToHealthy <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 0.0
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 1.0


  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex21A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
})



########################################################################

test_that("$N$ infected patients arrive.
intensive station -> death ", {
  skip_if_quicktest()
  N <- 2
  conf <- babsimToolsConf()
  ex23I <- ex1InfectedDf
  ex23A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex23A <- data.frame(time = ex23A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 1.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 0.0
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 0.0
  para$FactorPatientsIntensiveToDeath <- 1.0
  para$FactorPatientsIntensiveToHealthy <- 0.0


  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex23A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
})

########################################################################



test_that("$N$ infected patients arrive.
ventilation -> intensiveafter -> death", {
  skip_if_quicktest()
  N <- 3
  conf <- babsimToolsConf()
  ex26I <- ex1InfectedDf
  ex26A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex26A <- data.frame(time = ex26A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 1.0
  para$FactorPatientsHospitalToNormal <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 1.0
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 0.0
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.0
  para$FactorPatientsIntensiveAfterToHealthy <- 0.0
  para$FactorPatientsIntensiveAfterToDeath <- 1.0

  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex26A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
})


########################################################################

test_that("$N$ infected patients arrive.
ventilation -> death", {
  skip_if_quicktest()
  N <- 4
  conf <- babsimToolsConf()
  ex28I <- ex1InfectedDf
  ex28A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex28A <- data.frame(time = ex28A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 1.0
  para$FactorPatientsHospitalToNormal <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 0.0
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 1.0


  conf$logLevel <- 0
  y <- babsimHospital(
    arrivalTimes = ex28A,
    conf = conf,
    para = para
  )
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(N, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
  #
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(0, sum(d[d > 0]))
})
