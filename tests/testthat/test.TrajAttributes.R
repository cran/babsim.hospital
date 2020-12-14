
context("Test attributes in trajectories")

if(!is.null(getOption("babsim.hospital.test.level"))){
    skip_if(getOption("babsim.hospital.test.level")==0)
}

test_that("$N$ patients can stay home",{
  set.seed(1111)
  require("simmer")
  require("dplyr")
  N = 2
  conf <- babsimToolsConf()
  ex50I <- ex1InfectedDf
  ex50A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected) )
  ex50A <- data.frame(time = ex50A[1:N,])
  
  para <- babsimHospitalPara()
  para$FactorPatientsInfectedToHospital <- 0.0
  
  
  
  y <- babsimHospital(arrivalTimes = ex50A
                      , conf = conf
                      , para = para)
  
  
  # value of attribute "No Hospital Required" should be equal with number of patients
  
  attributes <- get_mon_attributes(y)
  s <- attributes[attributes$key=="No Hospital Required",]
  expect_equal(N , nrow(s))
  
})

########################################################################################

test_that("$N$ patients arrive. Everybody goes to the normal station and is healed",{
  set.seed(1112)
  require("simmer")
  require("dplyr")
  N = 4
  conf <- babsimToolsConf()
  ex51I <- ex1InfectedDf
  ex51A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected) )
  ex51A <- data.frame(time = ex51A[1:N,])
  
  para <- babsimHospitalPara()
  #
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  para$FactorPatientsHospitalToIntensive <- 0.0
  para$FactorPatientsHospitalToVentilation <- 0.0
  
  #
  para$FactorPatientsNormalToDeath <- 0.0
  para$FactorPatientsNormalToVentilation <- 0.0
  para$FactorPatientsNormalToIntensive <- 0.0
  
  y <- babsimHospital(arrivalTimes = ex51A
                      , conf = conf
                      , para = para)
  
  
  # value of attribute "Healed" should be equal with number of patients
  
  attributes <- get_mon_attributes(y)
  s <- attributes[attributes$key=="Healed",]
  expect_equal(N , nrow(s))
  
})



#######################################################################################

test_that("$N$ patients arrive. Everybody goes to the normal station and dies",{
  set.seed(1113)
  require("simmer")
  require("dplyr")
  N = 4
  conf <- babsimToolsConf()
  ex52I <- ex1InfectedDf
  ex52A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected) )
  ex52A <- data.frame(time = ex52A[1:N,])
  
  para <- babsimHospitalPara()
  #
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  # everybody goes to normal:
  para$FactorPatientsHospitalToIntensive <- 0
  para$FactorPatientsHospitalToVentilation <- 0
  #
  para$FactorPatientsNormalToDeath <- 1.0
  para$FactorPatientsNormalToVentilation <- 0
  para$FactorPatientsNormalToIntensive <- 1.0
  
  y <- babsimHospital(arrivalTimes = ex52A
                      , conf = conf
                      , para = para)
  
  
  # value of attribute "Dead" should be equal with number of patients
  
  attributes <- get_mon_attributes(y)
  s <- attributes[attributes$key=="Dead",]
  expect_equal(N , nrow(s))
  
})
#######################################################################################

test_that("$N$ patients arrive. Everybody goes to the normal station. 
              normal station -> death: 50%
              normal station -> healed: 50%
          ",{
  set.seed(1114)
  require("simmer")
  require("dplyr")
  N = 8
  conf <- babsimToolsConf()
  ex53I <- ex1InfectedDf
  ex53A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected) )
  ex53A <- data.frame(time = ex53A[1:N,])
  
  para <- babsimHospitalPara()
  #
  para$FactorPatientsInfectedToHospital <- 1.0
  #
  # everybody goes to normal:
  para$FactorPatientsHospitalToIntensive <- 0
  para$FactorPatientsHospitalToVentilation <- 0
  #
  # 50% healthy, 50% death 
  para$FactorPatientsNormalToDeath <- 0.5
  para$FactorPatientsNormalToVentilation <- 0
  para$FactorPatientsNormalToIntensive <- 0
  
  
  y <- babsimHospital(arrivalTimes = ex53A
                      , conf = conf
                      , para = para)
  
  
  # value of attribute "Healed" should be less than N
  # value of attribute "Dead" should be lass than N
  
  attributes <- get_mon_attributes(y)
  s <- attributes[attributes$key=="Healed",]
  expect_lt(nrow(s), N)
  
  attributes <- get_mon_attributes(y)
  s <- attributes[attributes$key=="Dead",]
  expect_lt(nrow(s), N)
})

#######################################################################################

test_that("$N$ patients generated. Real simulation factors",{
  set.seed(1115)
  require("simmer")
  require("dplyr")
  N = 400
  conf <- babsimToolsConf()
  ex54I <- ex1InfectedDf
  ex54A <- data.frame(getArrivalTimes(ex1InfectedDf$Infected) )
  ex54A <- data.frame(time = ex54A[1:N,])
  
  para <- babsimHospitalPara()
  
  
  
  
  y <- babsimHospital(arrivalTimes = ex54A
                      , conf = conf
                      , para = para)
  
  
  # sum of attributes should be equal with number of patients
  
  attributes <- get_mon_attributes(y)
  expect_equal(N , nrow(attributes))
  
})