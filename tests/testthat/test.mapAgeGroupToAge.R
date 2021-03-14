

test_that("mapAgeGroupToAge works", {
  Altersgruppe <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+", "unbekannt")
  Age <- c(2, 10, 25, 47, 70, 90, 47)
  expect_equal(mapAgeGroupToAge(Altersgruppe), Age)

  o <- sample(length(Age))
  expect_equal(mapAgeGroupToAge(Altersgruppe[o]), Age[o])
})
