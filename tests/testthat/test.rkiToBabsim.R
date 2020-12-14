context("preprocessing rki data")

test_that("zero days are not ignored, correct amount of cases are produced", {
    arrivals <- rkiToBabsimArrivals(rkidata)
    expect_equal(sum(rkidata$AnzahlFall), nrow(arrivals))
})