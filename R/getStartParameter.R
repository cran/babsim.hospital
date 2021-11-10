#' @title getStartParameter
#'
#' @description Returns parameter for babsim runs
#'
#' @param para parameter vector, e.g., generated via
#' \code{\link{babsimHospitalPara}}. Default: \code{\link{babsimHospitalPara}}
#' @param region (int) use region specific start parameter, e.g., \code{5374} for OBK.
#' If \code{region} is negative (default), a generic start parameter is chosen.
#' The selection is based on the obkpara, koelnpara, and nrwpara parameter values,
#' which are the best known values found so far.
#'
#' @return This function returns a (1,n) dim matrix
#' @examples
#' para <- getStartParameter(region = 5374)
#' @export
getStartParameter <- function(para = babsimHospitalPara(), region = -1) {
  if (region != -1) {
    localPara <- getParaSet(region)
    if (nrow(localPara) == 0) {
      ## Switch to default (koeln)
      localPara <- getParaSet(16077)
    }
    m <- as.matrix(localPara)
    m <- m[, -1]
    return(m)
  } else {
    x0 <- c(
      para$AmntDaysInfectedToHospital, para$AmntDaysNormalToHealthy, para$AmntDaysNormalToIntensive,
      para$AmntDaysNormalToVentilation, para$AmntDaysNormalToDeath, para$AmntDaysIntensiveToAftercare,
      para$AmntDaysIntensiveToVentilation, para$AmntDaysIntensiveToDeath, para$AmntDaysVentilationToIntensiveAfter,
      para$AmntDaysVentilationToDeath, para$AmntDaysIntensiveAfterToAftercare,
      para$AmntDaysIntensiveAfterToDeath, para$GammaShapeParameter, para$FactorPatientsInfectedToHospital,
      para$FactorPatientsHospitalToIntensive, para$FactorPatientsHospitalToVentilation,
      para$FactorPatientsNormalToIntensive, para$FactorPatientsNormalToVentilation,
      para$FactorPatientsNormalToDeath, para$FactorPatientsIntensiveToVentilation,
      para$FactorPatientsIntensiveToDeath, para$FactorPatientsVentilationToIntensiveAfter,
      para$FactorPatientsIntensiveAfterToDeath, para$AmntDaysAftercareToHealthy,
      para$RiskFactorA, para$RiskFactorB, para$RiskMale, para$AmntDaysIntensiveAfterToHealthy,
      para$FactorPatientsIntensiveAfterToHealthy
    )
    return(matrix(x0, 1, length(x0)))
  }
}
