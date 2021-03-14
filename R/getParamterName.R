#' @title getParameterName
#'
#' @description Returns the name (chr) of the babsim \code{x} parameter vector.
#'
#' @param n int: position

#' @return This function returns a  character value, which represents
#' the name of the n-th \code{x} variable.

#' @examples
#' getParameterName(16)
#' @export


getParameterName <- function(n) {
  param <- list(
    "AmntDaysInfectedToHospital", "AmntDaysNormalToHealthy", "AmntDaysNormalToIntensive",
    "AmntDaysNormalToVentilation", "AmntDaysNormalToDeath", "AmntDaysIntensiveToAftercare",
    "AmntDaysIntensiveToVentilation", "AmntDaysIntensiveToDeath", "AmntDaysVentilationToIntensiveAfter",
    "AmntDaysVentilationToDeath", "AmntDaysIntensiveAfterToAftercare", "AmntDaysIntensiveAfterToDeath",
    "GammaShapeParameter", "FactorPatientsInfectedToHospital", "FactorPatientsHospitalToIntensive",
    "FactorPatientsHospitalToVentilation", "FactorPatientsNormalToIntensive",
    "FactorPatientsNormalToVentilation", "FactorPatientsNormalToDeath", "FactorPatientsIntensiveToVentilation",
    "FactorPatientsIntensiveToDeath", "FactorPatientsVentilationToIntensiveAfter",
    "FactorPatientsIntensiveAfterToDeath", "AmntDaysAftercareToHealthy", "RiskFactorA",
    "RiskFactorB", "RiskMale", "AmntDaysIntensiveAfterToHealthy", "FactorPatientsIntensiveAfterToHealthy"
  )
  return(param[[n]])
}
