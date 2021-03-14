#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 50%
        normal station -> healthy: 50%
        normal station -> intensive station: 50%
    intensive station: 30% 
        intensive station -> ventilation: 20%
        intensive station -> aftercare: 80%
    ventilation: 20%
        ventilation -> intensiveafter: 100%
            intensiveafter -> healthy: 70%
            intensiveafter -> aftercare: 30%
  ", {
  skip_if_quicktest()
  set.seed(115)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex34I <- ex1InfectedDf
  ex34A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex34A <- data.frame(time = ex34A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.3
  para$FactorPatientsHospitalToVentilation <- 0.2
  para$FactorPatientsHospitalToNormal <- 0.5
  #
  para$FactorPatientsNormalToIntensive <- 0.5
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.5
  #
  para$FactorPatientsIntensiveToVentilation <- 0.2
  para$FactorPatientsIntensiveToAftercare <- 0.8
  para$FactorPatientsIntensiveToDeath <- 0.0
  para$FactorPatientsIntensiveToHealthy <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 1.0
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 0.0
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.3
  para$FactorPatientsIntensiveAfterToHealthy <- 0.7
  para$FactorPatientsIntensiveAfterToDeath <- 0.0

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex34A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  # less than 1300
  # more than 550
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 1.3 * N)
  expect_gt(sum(d[d > 0]), 0.4 * N)
  # expectations ventilation beds:
  # less than 500
  # more than 150
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 0.5 * N)
  expect_gt(sum(d[d > 0]), 0.15 * N)
  # expectations beds:
  # less than 1600
  # more than 500
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 1.6 * N)
  expect_gt(sum(d[d > 0]), 0.5 * N)
})

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 100%
        normal station -> healthy: 10%
        normal station -> intensive station: 60%
            intensive station -> intensiveafter: 100%
        normal station -> death: 10%
        normal station -> ventilation: 20%
            ventilation -> death: 10%
            ventilation -> intafter: 90%
                intafter -> healthy: 90%
                intafter -> aftercare: 10%
    
  ", {
  skip_if_quicktest()
  set.seed(116)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex35I <- ex1InfectedDf
  ex35A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex35A <- data.frame(time = ex35A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 1.0
  #
  para$FactorPatientsNormalToIntensive <- 0.6
  para$FactorPatientsNormalToVentilation <- 0.2
  para$FactorPatientsNormalToDeath <- 0.1
  para$FactorPatientsNormalToHealthy <- 0.1
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 1.0
  para$FactorPatientsIntensiveToDeath <- 0.0
  para$FactorPatientsIntensiveToHealthy <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 0.9
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 0.1
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.1
  para$FactorPatientsIntensiveAfterToHealthy <- 0.9
  para$FactorPatientsIntensiveAfterToDeath <- 0.0

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex35A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  # less than 600
  # more than 150
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 1.2 * N)
  expect_gt(sum(d[d > 0]), 0.3 * N)
  # expectations ventilation beds:
  # less than 150
  # more than 50
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 0.3 * N)
  expect_gt(sum(d[d > 0]), 0.1 * N)
  # expectations beds:
  # less than 1250
  # more than 400
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 2.5 * N)
  expect_gt(sum(d[d > 0]), 0.8 * N)
})

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 40%
        normal station -> healthy: 50%
        normal station -> intensive station: 50%
            intensive station -> aftercare: 50%
            intensive station -> healthy: 50%
    intensive station: 30%
        intensive station -> aftercare: 10%
        intensive station -> healthy: 90%
    ventilation: 30%
        ventilation -> death: 10%
        ventilation -> intensiveafter: 90%
            intensiveafter -> healthy: 10%
            intensiveafter -> aftercare: 90%
    
  ", {
  skip_if_quicktest()
  set.seed(116)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex36I <- ex1InfectedDf
  ex36A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex36A <- data.frame(time = ex36A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.3
  para$FactorPatientsHospitalToVentilation <- 0.3
  para$FactorPatientsHospitalToNormal <- 0.4
  #
  para$FactorPatientsNormalToIntensive <- 0.5
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.5
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 0.5
  para$FactorPatientsIntensiveToDeath <- 0.0
  para$FactorPatientsIntensiveToHealthy <- 0.5
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 0.9
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 0.1
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.9
  para$FactorPatientsIntensiveAfterToHealthy <- 0.1
  para$FactorPatientsIntensiveAfterToDeath <- 0.0

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex36A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  # less than 600
  # more than 150
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 1.2 * N)
  expect_gt(sum(d[d > 0]), 0.3 * N)
  # expectations ventilation beds:
  # less than 250
  # more than 50
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 0.5 * N)
  expect_gt(sum(d[d > 0]), 0.1 * N)
  # expectations beds:
  # less than 600
  # more than 150
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 1.2 * N)
  expect_gt(sum(d[d > 0]), 0.3 * N)
})

#########################################################################

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 70%
        normal station -> healthy: 40%
        normal station -> intensive station: 60%
    intensive station: 30%
        intensive station -> death: 30%
        intensive station -> healthy: 20%
        intensive station -> aftercare: 50%
    
  ", {
  skip_if_quicktest()
  set.seed(118)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex38I <- ex1InfectedDf
  ex38A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex38A <- data.frame(time = ex38A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.3
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 0.7
  #
  para$FactorPatientsNormalToIntensive <- 0.6
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.4
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 0.5
  para$FactorPatientsIntensiveToDeath <- 0.2
  para$FactorPatientsIntensiveToHealthy <- 0.3


  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex38A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  # less than 550
  # more than 150
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 1.1 * N)
  expect_gt(sum(d[d > 0]), 0.3 * N)
  # expectations ventilation beds:
  # 0
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0)
  # expectations beds:
  # less than 800
  # more than 250
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 1.6 * N)
  expect_gt(sum(d[d > 0]), 0.5 * N)
})

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 40%
        normal station -> healthy: 40%
        normal station -> intensive station: 60%
    intensive station: 40%
        intensive station -> death: 30%
        intensive station -> healthy: 20%
        intensive station -> aftercare: 50%
    ventilation: 20%
        ventilation -> aftercare: 50%
        ventilation -> intensiveafter: 50%
    
  ", {
  skip_if_quicktest()
  set.seed(119)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex39I <- ex1InfectedDf
  ex39A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex39A <- data.frame(time = ex39A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.4
  para$FactorPatientsHospitalToVentilation <- 0.2
  para$FactorPatientsHospitalToNormal <- 0.4
  #
  para$FactorPatientsNormalToIntensive <- 0.6
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.4
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 0.5
  para$FactorPatientsIntensiveToDeath <- 0.2
  para$FactorPatientsIntensiveToHealthy <- 0.3
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 0.5
  para$FactorPatientsVentilationToAftercare <- 0.5
  para$FactorPatientsVentilationToDeath <- 0.0
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.5
  para$FactorPatientsIntensiveAfterToHealthy <- 0.2
  para$FactorPatientsIntensiveAfterToDeath <- 0.3

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex39A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  # less than 600
  # more than 150
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 1.2 * N)
  expect_gt(sum(d[d > 0]), 0.3 * N)
  # expectations ventilation beds:
  # less than 150
  # more than 50
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 0.3 * N)
  expect_gt(sum(d[d > 0]), 0.1 * N)
  # expectations beds:
  # less than 700
  # more than 200
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_lt(sum(d[d > 0]), 1.4 * N)
  expect_gt(sum(d[d > 0]), 0.4 * N)
  # expect_equal(sum(d[d > 0] , 0.9*N, tolerance=1))
})


#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 100%
        normal station -> healthy: 40%
        normal station -> intensive station: 60%
            intensive station -> death: 30%
            intensive station -> healthy: 20%
            intensive station -> aftercare: 50%

    
  ", {
  skip_if_quicktest()
  set.seed(119)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex40I <- ex1InfectedDf
  ex40A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex40A <- data.frame(time = ex40A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 1.0
  #
  para$FactorPatientsNormalToIntensive <- 0.6
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.4
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 0.5
  para$FactorPatientsIntensiveToDeath <- 0.3
  para$FactorPatientsIntensiveToHealthy <- 0.2


  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex40A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.6 * N, tolerance = 0.3 * N)
  # expectations beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 1.3 * N, tolerance = 0.65 * N)
  # expectations ventilation beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0)
})

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 100%
        normal station -> healthy: 40%
        normal station -> intensive station: 60%
            intensive station -> death: 30%
            intensive station -> healthy: 20%
            intensive station -> aftercare: 50%

    
  ", {
  skip_if_quicktest()
  set.seed(120)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex40I <- ex1InfectedDf
  ex40A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex40A <- data.frame(time = ex40A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 1.0
  #
  para$FactorPatientsNormalToIntensive <- 0.6
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.4
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 0.5
  para$FactorPatientsIntensiveToDeath <- 0.3
  para$FactorPatientsIntensiveToHealthy <- 0.2


  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex40A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.6 * N, tolerance = 0.3 * N)
  # expectations beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 1.3 * N, tolerance = 0.65 * N)
  # expectations ventilation beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0)
})

#########################################################################


test_that("$N$ infected patients arrive.
  probability for
    intensive station: 100%
            intensive station -> aftercare: 10%
            intensive station -> healthy: 10%
            intensive station -> ventilation: 80%
                ventilation -> intensiveafter: 100%
                    intensiveafter -> aftercare: 50%
                    intensiveafter -> healthy: 50%
                

    
  ", {
  skip_if_quicktest()
  set.seed(121)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex41I <- ex1InfectedDf
  ex41A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex41A <- data.frame(time = ex41A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 1.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 0.0
  #
  para$FactorPatientsIntensiveToVentilation <- 0.8
  para$FactorPatientsIntensiveToAftercare <- 0.1
  para$FactorPatientsIntensiveToDeath <- 0.0
  para$FactorPatientsIntensiveToHealthy <- 0.1
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 1.0
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 0.0
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.5
  para$FactorPatientsIntensiveAfterToHealthy <- 0.5
  para$FactorPatientsIntensiveAfterToDeath <- 0.0

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex41A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 1.8 * N, tolerance = 0.9 * N)
  # expectations beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.5 * N, tolerance = 0.25 * N)
  # expectations ventilation beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.8 * N, tolerance = 0.4 * N)
})

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    ventilation: 100%
        ventilation -> intensiveafter: 80%
            intensiveafter -> aftercare: 30%
            intensiveafter -> healthy: 70%
        ventilation -> death: 20%
                

    
  ", {
  skip_if_quicktest()
  set.seed(122)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex42I <- ex1InfectedDf
  ex42A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex42A <- data.frame(time = ex42A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 1.0
  para$FactorPatientsHospitalToNormal <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 0.8
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 0.2
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.3
  para$FactorPatientsIntensiveAfterToHealthy <- 0.7
  para$FactorPatientsIntensiveAfterToDeath <- 0.0

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex42A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.8 * N, tolerance = 0.4 * N)
  # expectations beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.24 * N, tolerance = 0.12 * N)
  # expectations ventilation beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), N, tolerance = 0.5 * N)
})

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 50%
        normal station -> intensive station: 100%
            intensive station -> aftercare: 60%
            intensive station -> healthy: 40%
    intensive station: 50%
                

    
  ", {
  skip_if_quicktest()
  set.seed(123)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex43I <- ex1InfectedDf
  ex43A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex43A <- data.frame(time = ex43A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.5
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 0.5
  #
  para$FactorPatientsNormalToIntensive <- 1.0
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.0
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 0.6
  para$FactorPatientsIntensiveToDeath <- 0.0
  para$FactorPatientsIntensiveToHealthy <- 0.4

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex43A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), N, tolerance = 0.5 * N)
  # expectations beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 1.3 * N, tolerance = 0.65 * N)
  # expectations ventilation beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0)
})

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 20%
        normal station -> intensive station: 100%
            intensive station -> aftercare: 20%
            intensive station -> healthy: 60%
            intensive station -> death: 20%
    ventilation: 80%
        ventilation -> death: 50%
        ventilation -> aftercare: 50%
  ", {
  skip_if_quicktest()
  set.seed(124)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex44I <- ex1InfectedDf
  ex44A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex44A <- data.frame(time = ex44A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.8
  para$FactorPatientsHospitalToNormal <- 0.2
  #
  para$FactorPatientsNormalToIntensive <- 1.0
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.0
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 0.2
  para$FactorPatientsIntensiveToDeath <- 0.2
  para$FactorPatientsIntensiveToHealthy <- 0.6
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 0.0
  para$FactorPatientsVentilationToAftercare <- 0.5
  para$FactorPatientsVentilationToDeath <- 0.5

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex44A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.2 * N, tolerance = 0.2 * N)
  # expectations beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.64 * N, tolerance = 0.32 * N)
  # expectations ventilation beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.2 * N, tolerance = 0.1 * N)
})

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 30%
        normal station -> intensive station: 100%
    ventilation: 40%
        ventilation -> afterintensive: 100%
            afterintensive -> death: 20%
            afterintensive -> aftercare: 80%
    intensive station: 30%
        intensive station -> aftercare: 90%
        intensive station -> death: 10%
                

    
  ", {
  skip_if_quicktest()
  set.seed(125)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex45I <- ex1InfectedDf
  ex45A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex45A <- data.frame(time = ex45A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.3
  para$FactorPatientsHospitalToVentilation <- 0.4
  para$FactorPatientsHospitalToNormal <- 0.3
  #
  para$FactorPatientsNormalToIntensive <- 1.0
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.0
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 0.9
  para$FactorPatientsIntensiveToDeath <- 0.1
  para$FactorPatientsIntensiveToHealthy <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 1.0
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 0.0
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.8
  para$FactorPatientsIntensiveAfterToHealthy <- 0.0
  para$FactorPatientsIntensiveAfterToDeath <- 0.2

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex45A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), N, tolerance = 0.5 * N)
  # expectations beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 1.16 * N, tolerance = 0.58 * N)
  # expectations ventilation beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.4 * N, tolerance = 0.2 * N)
})

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 40%
        normal station -> intensive station: 100%
    ventilation: 10%
        ventilation -> afterintensive: 90%
        ventilation -> death: 10%
            afterintensive -> aftercare: 100%
    intensive station: 50%
        intensive station -> aftercare: 100%
        
    
  ", {
  skip_if_quicktest()
  set.seed(126)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex46I <- ex1InfectedDf
  ex46A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex46A <- data.frame(time = ex46A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.5
  para$FactorPatientsHospitalToVentilation <- 0.1
  para$FactorPatientsHospitalToNormal <- 0.4
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
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 0.9
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 0.1
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 1.0
  para$FactorPatientsIntensiveAfterToHealthy <- 0.0
  para$FactorPatientsIntensiveAfterToDeath <- 0.0

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex46A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), N, tolerance = 0.5 * N)
  # expectations beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 1.4 * N, tolerance = 0.7 * N)
  # expectations ventilation beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.1 * N, tolerance = 0.05 * N)
})

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    ventilation: 100%
        ventilation -> intensiveAfter: 80%
        ventilation -> death: 20%
            afterintensive -> aftercare: 50%
            afterintensive -> healthy: 50%
        
    
  ", {
  skip_if_quicktest()
  set.seed(127)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex47I <- ex1InfectedDf
  ex47A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex47A <- data.frame(time = ex47A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 1.0
  para$FactorPatientsHospitalToNormal <- 0.0
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 0.8
  para$FactorPatientsVentilationToDeath <- 0.2
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 0.5
  para$FactorPatientsIntensiveAfterToHealthy <- 0.5
  para$FactorPatientsIntensiveAfterToDeath <- 0.25

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex47A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.4 * N, tolerance = 0.2 * N)
  # expectations beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.25 * N, tolerance = 0.3 * N)
  # expectations ventilation beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), N, tolerance = 0.5 * N)
})

#########################################################################

test_that("$N$ infected patients arrive.
  probability for
    normal station: 100%
        normal station -> intensive station: 80%
            intensive station -> aftercare: 50%
            intensive station -> healthy: 50%
        normal station -> ventilation: 20%
            ventilation -> afterintensive: 100%
                afterintensive -> aftercare: 100%
        
    
  ", {
  skip_if_quicktest()
  set.seed(128)
  require("simmer")
  require("dplyr")
  # N is the number of patients
  N <- 500
  conf <- babsimToolsConf()
  ex48I <- ex1InfectedDf
  ex48A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected))
  ex48A <- data.frame(time = ex48A[1:N, ])

  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  para$FactorPatientsHospitalToNormal <- 1.0
  #
  para$FactorPatientsNormalToIntensive <- 0.8
  para$FactorPatientsNormalToVentilation <- 0.2
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToHealthy <- 0.0
  #
  para$FactorPatientsIntensiveToVentilation <- 0.0
  para$FactorPatientsIntensiveToAftercare <- 0.5
  para$FactorPatientsIntensiveToDeath <- 0.0
  para$FactorPatientsIntensiveToHealthy <- 0.5
  #
  para$FactorPatientsVentilationToIntensiveAfter <- 1.0
  para$FactorPatientsVentilationToAftercare <- 0.0
  para$FactorPatientsVentilationToDeath <- 0.0
  #
  # para$FactorPatientsIntensiveAfterToAftercare <- 1.0
  para$FactorPatientsIntensiveAfterToHealthy <- 0.0
  para$FactorPatientsIntensiveAfterToDeath <- 0.0

  conf$logLevel <- 1
  y <- babsimHospital(
    arrivalTimes = ex48A,
    conf = conf,
    para = para
  )
  # expectations intensive beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), N, tolerance = 0.5 * N)
  # expectations beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "bed", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 1.6 * N, tolerance = 0.8 * N)
  # expectations ventilation beds:
  resources <- get_mon_resources(y)
  s <- resources[resources$resource == "intensiveBedVentilation", ]$server
  s <- c(0, s)
  d <- diff(s)
  expect_equal(sum(d[d > 0]), 0.2 * N, tolerance = 0.1 * N)
})
