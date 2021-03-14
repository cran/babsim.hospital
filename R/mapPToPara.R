#' @title mapPToPara
#'
#' @description  \code{mapPToPara} accepts
#' a nxn matrix. Its values will be mapped onto
#' the probability entries of a \code{\link{babsimHospitalPara}}
#' list.
#'
#' @param P (num) nxn-dim matrix. Values will be mapped onto the probabilities in
#' \code{babsimHospitalPara}.
#' Names of these parameters can be obtained via \code{\link{getParameterName}}.
#' @param para Parameter list, e.g., generated via \code{\link{babsimHospitalPara}}
#'
#' @return This function returns a parameter list.
#'
#' @examples
#' para <- babsimHospitalPara()
#' P <- getMatrixP()
#' para <- mapPToPara(
#'   P = P,
#'   para = para
#' )
#' @export

mapPToPara <- function(P = getMatrixP(), para) {
  para$FactorPatientsInfectedToHospital <- P[1, 3] # x[14]
  para$FactorPatientsHospitalToIntensive <- P[3, 5] # x[15]
  para$FactorPatientsHospitalToVentilation <- P[3, 6] # x[16]
  para$FactorPatientsNormalToIntensive <- P[4, 5] # x[17]
  para$FactorPatientsNormalToVentilation <- P[4, 6] # x[18]
  para$FactorPatientsNormalToDeath <- P[4, 9] # x[19]
  para$FactorPatientsIntensiveToVentilation <- P[5, 6] # x[20]
  para$FactorPatientsIntensiveToDeath <- P[5, 9] # x[21]
  para$FactorPatientsVentilationToIntensiveAfter <- P[6, 7] # x[22]
  para$FactorPatientsIntensiveAfterToDeath <- P[7, 9] # x[23]
  para$FactorPatientsIntensiveAfterToHealthy <- P[7, 10] # x[29]
  return(para)
}
