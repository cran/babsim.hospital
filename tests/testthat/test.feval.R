


test_that("test funWrapOptimizeSim", {
  require("simmer")
  require("dplyr")
  x <- getStartParameter()
  data <- getObkData()
  conf <- babsimToolsConf()
  conf$seed <- 123
  y1 <- funWrapOptimizeSim(x, conf = conf, data = data)
  conf$seed <- 123
  y2 <- funWrapOptimizeSim(x, conf = conf, data = data)
  expect_equal(y1, y2)
})
