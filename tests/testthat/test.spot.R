

test_that("check that babsim function works with spot", {
  set.seed(123)
  require("SPOT")
  ## n = number of function evaluations
  n <- 30
  bounds <- getBounds()
  conf <- babsimToolsConf()
  data <- getObkData()
  para <- getStartParameter()
  resTest <- spot(
    x = para,
    fun = funWrapOptimizeSim,
    lower = bounds$lower,
    upper = bounds$upper,
    control = list(
      funEvals = n,
      noise = TRUE,
      model = buildTreeModel,
      plots = FALSE,
      progress = FALSE
    ),
    conf,
    data
  )
  expect_equal(resTest$count, n)
})
