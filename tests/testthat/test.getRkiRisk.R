

test_that("getRkiRisk works", {
  rki <- data.frame(Age       =c( 10,  20,  30,  40,  50, 60,  70,   80),
                    Geschlecht=c("M", "W", "M", "M", "W", "W", "M", "M"))

  expect_equal(getRkiRisk(rki, list(RiskFactorA=0, RiskFactorB=0, RiskMale=1))$Risk,
               rep(0, nrow(rki)))

  expect_equal(getRkiRisk(rki, list(RiskFactorA=1, RiskFactorB=0, RiskMale=2))$Risk,
               ifelse(rki$Geschlecht == "M", 2, 1))

  expect_equal(getRkiRisk(rki, list(RiskFactorA=0.2, RiskFactorB=0.1, RiskMale=2))$Risk,
               0.2 * exp(0.1 * rki$Age) * ifelse(rki$Geschlecht == "M", 2, 1))
  
  expect_equal(getRkiRisk(rki, list(RiskFactorA=0.1, RiskFactorB=0.2, RiskMale=0.3))$Risk,
               0.1 * exp(0.2 * rki$Age) * ifelse(rki$Geschlecht == "M", 0.3, 1))
})
