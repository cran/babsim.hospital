library(babsim.hospital)

set.seed(7)

#args = commandArgs(trailingOnly=TRUE)
#params <- as.numeric(strsplit(args[1],",", fixed = TRUE)[[1]])
#res <- sum(params^2)
#cat(res)

x <- hospitalCovid19_20200609
arrivalTimes <- getArrivalTimes(x$Infected) 
y <- babsimHospital(arrivalTimes = arrivalTimes
                    , StartDate = x$Days[1], EndDate = x$Days[length(x$Days)]
                    , control = babsimHospitalPara())
resources <- get_mon_resources(y)
resources <- resources %>% filter(resource != "nurse")
mean(resources$server)
