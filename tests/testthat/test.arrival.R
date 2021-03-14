


test_that("test getArrivalTimes", {
  n <- 10
  a <- getArrivalTimes(1:n)
  ## Anzahl der Arrivals:
  expect_true(dim(a)[1] == sum(1:n))
  ## Untere und obere Grenze:
  expect_true(min(a) >= 0)
  expect_true(max(a) <= n)
  ## Anzahl Arrivals an einem Tag:
  expect_true(length(a [(a$time >= n - 1) & (a$time <= n), ]) == n)
})
