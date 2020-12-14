context("babsim.hospital parallel Runs")

test_that("parallel runs work as planned", {
    set.seed(123)
    arrival1 <- rkiToBabsimArrivals(babsim.hospital::rkidata)
    
    arrival1 <- data.frame("time"=arrival1[1:1000,])
    
    para = babsimHospitalPara()
    conf = babsimToolsConf()
    expect_error({babsimHospital(arrivalTimes = arrival1,
                                 conf = conf,
                                 para = para)},NA)
})


