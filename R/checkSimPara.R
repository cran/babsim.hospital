#' @title checkSimPara
#'
#' @description check (and correct) parameter list
#'
#' @param para list: optimization parameters, e.g., generated via \code{\link{babsimHospitalPara}}
#'
#' @return corrected parameter list
#'
#' @examples
#'
#' x0 <- babsimHospitalPara()
#' x <- checkSimPara(x0)
#' @export
checkSimPara <- function(para = babsimHospitalPara()) {
  minDays <- 1e-06
  maxDays <- 365
  minVal <- 1e-06
  maxVal <- 1e+06
  minProb <- 0
  maxProb <- 1
  # x1:
  para$AmntDaysInfectedToHospital <- ensureRangeOpen(
    para$AmntDaysInfectedToHospital,
    minDays, maxDays
  )
  # x2:
  para$AmntDaysNormalToHealthy <- ensureRangeOpen(
    para$AmntDaysNormalToHealthy,
    minDays, maxDays
  )
  # x3:
  para$AmntDaysNormalToIntensive <- ensureRangeOpen(
    para$AmntDaysNormalToIntensive,
    minDays, maxDays
  )
  # x4:
  para$AmntDaysNormalToVentilation <- ensureRangeOpen(
    para$AmntDaysNormalToVentilation,
    minDays, maxDays
  )
  # x5:
  para$AmntDaysNormalToDeath <- ensureRangeOpen(
    para$AmntDaysNormalToDeath, minDays,
    maxDays
  )
  # x6:
  para$AmntDaysIntensiveToAftercare <- ensureRangeOpen(
    para$AmntDaysIntensiveToAftercare,
    minDays, maxDays
  )
  # x7:
  para$AmntDaysIntensiveToVentilation <- ensureRangeOpen(
    para$AmntDaysIntensiveToVentilation,
    minDays, maxDays
  )
  # x8:
  para$AmntDaysIntensiveToDeath <- ensureRangeOpen(
    para$AmntDaysIntensiveToDeath,
    minDays, maxDays
  )
  # x9:
  para$AmntDaysVentilationToIntensiveAfter <- ensureRangeOpen(
    para$AmntDaysVentilationToIntensiveAfter,
    minDays, maxDays
  )
  # x10:
  para$AmntDaysVentilationToDeath <- ensureRangeOpen(
    para$AmntDaysVentilationToDeath,
    minDays, maxDays
  )
  # x11:
  para$AmntDaysIntensiveAfterToAftercare <- ensureRangeOpen(
    para$AmntDaysIntensiveAfterToAftercare,
    minDays, maxDays
  )
  # x12:
  para$AmntDaysIntensiveAfterToDeath <- ensureRangeOpen(
    para$AmntDaysIntensiveAfterToDeath,
    minDays, maxDays
  )
  # x13:
  para$GammaShapeParameter <- ensureRangeOpen(
    para$GammaShapeParameter, minVal,
    maxVal
  )
  # x14:
  para$FactorPatientsInfectedToHospital <- ensureRangeOpen(
    para$FactorPatientsInfectedToHospital,
    minProb, maxProb
  )
  # x15:
  para$FactorPatientsHospitalToIntensive <- ensureRangeOpen(
    para$FactorPatientsHospitalToIntensive,
    minProb, maxProb
  )
  # x16:
  para$FactorPatientsHospitalToVentilation <- ensureRangeOpen(
    para$FactorPatientsHospitalToVentilation,
    minProb, maxProb
  )
  # x17:
  para$FactorPatientsNormalToIntensive <- ensureRangeOpen(
    para$FactorPatientsNormalToIntensive,
    minProb, maxProb
  )
  # x18:
  para$FactorPatientsNormalToVentilation <- ensureRangeOpen(
    para$FactorPatientsNormalToVentilation,
    minProb, maxProb
  )
  # x19:
  para$FactorPatientsNormalToDeath <- ensureRangeOpen(
    para$FactorPatientsNormalToDeath,
    minProb, maxProb
  )
  # x20:
  para$FactorPatientsIntensiveToVentilation <- ensureRangeOpen(
    para$FactorPatientsIntensiveToVentilation,
    minProb, maxProb
  )
  # x21:
  para$FactorPatientsIntensiveToDeath <- ensureRangeOpen(
    para$FactorPatientsIntensiveToDeath,
    minProb, maxProb
  )
  # x22:
  para$FactorPatientsVentilationToIntensiveAfter <- ensureRangeOpen(
    para$FactorPatientsVentilationToIntensiveAfter,
    minProb, maxProb
  )
  # x23:
  para$FactorPatientsIntensiveAfterToDeath <- ensureRangeOpen(
    para$FactorPatientsIntensiveAfterToDeath,
    minProb, maxProb
  )
  # x24:
  para$AmntDaysAftercareToHealthy <- ensureRangeOpen(
    para$AmntDaysAftercareToHealthy,
    minDays, maxDays
  )
  # x25:
  para$RiskFactorA <- ensureRangeOpen(para$RiskFactorA, minVal, maxVal)
  # x26:
  para$RiskFactorB <- ensureRangeOpen(para$RiskFactorB, minVal, maxVal)
  # Risk based on gender x27:
  para$RiskMale <- ensureRangeOpen(para$RiskMale, minVal, maxVal)
  # x28:
  para$AmntDaysIntensiveAfterToHealthy <- ensureRangeOpen(
    para$AmntDaysIntensiveAfterToHealthy,
    minDays, maxDays
  )
  # x29:
  para$FactorPatientsIntensiveAfterToHealthy <- ensureRangeOpen(
    para$FactorPatientsIntensiveAfterToHealthy,
    minProb, maxProb
  )
  ## Repair probabilities:
  P <- getMatrixP(para = para)
  m <- rep(1, nrow(P))
  Q <- P %*% diag(m)
  S <- diag(1 / rowSums(Q))
  R <- S %*% Q
  para <- mapPToPara(P = R, para = para)
  return(para)
}
