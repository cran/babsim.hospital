

test_that("test 0 resources leads to 0 error", {
  conf <- list("w2" = c(1, 1), "verbosity" = 0, "ResourceEval" = c("intensiveBed", "intensiveBedVentilation"))
  res <- data.frame(resource = "bed", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1))
  res <- rbind(res, data.frame(resource = "intensiveBed", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  res <- rbind(res, data.frame(resource = "intensiveBedVentilation", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  resSim <- res
  resSim$source <- "babsim"
  res <- rbind(res, resSim)
  expect_equal(getError(res, conf), 0)
})

test_that("test being 1 bed over the real value is still 0 error", {
  conf <- list("w2" = c(1, 1), "verbosity" = 0, "ResourceEval" = c("intensiveBed", "intensiveBedVentilation"))
  res <- data.frame(resource = "bed", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1))
  res <- rbind(res, data.frame(resource = "intensiveBed", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  res <- rbind(res, data.frame(resource = "intensiveBedVentilation", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  resSim <- res
  resSim$source <- "babsim"

  resSim$med <- 1

  res <- rbind(res, resSim)
  expect_equal(getError(res, conf), 0)
})

test_that("test being 1 bed under the real value leads to some error", {
  conf <- list("w2" = c(1, 1), "verbosity" = 0, "ResourceEval" = c("intensiveBed", "intensiveBedVentilation"))
  res <- data.frame(resource = "bed", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1))
  res <- rbind(res, data.frame(resource = "intensiveBed", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  res <- rbind(res, data.frame(resource = "intensiveBedVentilation", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  resSim <- res
  resSim$source <- "babsim"

  res$med <- 1

  res <- rbind(res, resSim)
  expect_true(getError(res, conf) > 0)
})

test_that("test that underestimating the amount of beds is worse then overestimating", {
  conf <- list("w2" = c(1, 1), "verbosity" = 0, "ResourceEval" = c("intensiveBed", "intensiveBedVentilation"))
  res <- data.frame(resource = "bed", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1))
  res <- rbind(res, data.frame(resource = "intensiveBed", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  res <- rbind(res, data.frame(resource = "intensiveBedVentilation", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  resSim <- res
  resSim$source <- "babsim"
  res$med <- 10
  res <- rbind(res, resSim)
  errorUnderestimated <- getError(res, conf)

  res <- data.frame(resource = "bed", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1))
  res <- rbind(res, data.frame(resource = "intensiveBed", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  res <- rbind(res, data.frame(resource = "intensiveBedVentilation", med = 0, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  resSim <- res
  resSim$source <- "babsim"
  resSim$med <- 10
  res <- rbind(res, resSim)
  errorOverestimated <- getError(res, conf)

  expect_true(errorUnderestimated > errorOverestimated)
})

test_that("test that errors in the past are less important than errors today", {
  conf <- list("w2" = c(1, 1), "verbosity" = 0, "ResourceEval" = c("intensiveBed", "intensiveBedVentilation"))
  res <- data.frame(resource = "bed", med = 1:10, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1))
  res <- rbind(res, data.frame(resource = "intensiveBed", med = 1:10, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  res <- rbind(res, data.frame(resource = "intensiveBedVentilation", med = 1:10, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  resSim <- res
  resSim$source <- "babsim"

  res$med <- 0

  res <- rbind(res, resSim)
  largeErrorToday <- getError(res, conf)


  res <- data.frame(resource = "bed", med = 10:1, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1))
  res <- rbind(res, data.frame(resource = "intensiveBed", med = 10:1, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  res <- rbind(res, data.frame(resource = "intensiveBedVentilation", med = 10:1, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  resSim <- res
  resSim$source <- "babsim"

  res$med <- 0

  res <- rbind(res, resSim)
  largeErrorInPast <- getError(res, conf)

  expect_true(largeErrorToday > largeErrorInPast)
})

test_that("test that order in the data.frame does not play a role", {
  conf <- list("w2" = c(1, 1), "verbosity" = 0, "ResourceEval" = c("intensiveBed", "intensiveBedVentilation"))
  res <- data.frame(resource = "bed", med = 1:10, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1))
  res <- rbind(res, data.frame(resource = "intensiveBed", med = 1:10, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  res <- rbind(res, data.frame(resource = "intensiveBedVentilation", med = 1:10, source = "GA", date = seq(as.Date("2020-10-01"), as.Date("2020-10-10"), 1)))
  resSim <- res
  resSim$source <- "babsim"

  res$med <- 0

  res <- rbind(res, resSim)
  errorSorted <- getError(res, conf)

  res <- res[sample(1:nrow(res), nrow(res)), ]
  errorShuffled <- getError(res, conf)

  expect_true(errorSorted == errorShuffled)
})
