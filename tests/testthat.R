library(testthat)
library(babsim.hospital)

options(babsim.hospital.run.full.test = TRUE)

test_check("babsim.hospital")

options(babsim.hospital.run.full.test = NULL)
