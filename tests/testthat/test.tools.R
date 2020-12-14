context("babsim.hospital tools valuation")

test_that("test rtgamma", {
  y1 <- mean( rtgamma(n=1e3, shape=1, rate=1, shift=1e6, alpha=1) )
  y2 <- mean( rgamma(n=1e3, shape=1, rate=1) )
 	expect_true(y1 > y2)
 })

test_that("test ensureRangeOpen", {
  set.seed(123)
  x = runif(1)
  expect_true(ensureRangeOpen(x=x, a=0, b=1) == x)
  expect_true(ensureRangeOpen(x= (x+1), a=0, b=1) == 1)
  expect_true(ensureRangeOpen(x= (x-1), a=0, b=1) == 0)
  })


test_that("test ensureRangeOpen", {
  x0 <- babsimHospitalPara()
  # Should be corrected:
  x0$FactorPatientsInfectedToHospital = 10
  x <- checkSimPara(x0)
  expect_true(x$FactorPatientsInfectedToHospital == 1)
})


test_that("test getDecision", {
  p <- c(1, 0, 0)
  expect_true(getDecision(p) == 0)
  p <- c(0, 0, 1)
  expect_true(getDecision(p) == 2)
  p <- c(0.6, 0.4, 0)
  expect_true(getDecision(p) < 2)
  p <- c(0, 0.6, 0.4)
  expect_true(getDecision(p) > 0)
})
