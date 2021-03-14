#' @title mapXToPara
#'
#' @description  \code{mapXToPara} accepts
#' a n-dim vector. Its values will be mapped onto a \code{\link{babsimHospitalPara}}
#' list.
#'
#' @details This function will replaced hte function \code{simulateHospital} in versions >=  1.2.8.
#'
#' @param x (num) n-dim vector. Values will be mapped onto \code{babsimHospitalPara}.
#' Names of these parameters can be obtained via \code{\link{getParameterName}}.
#'
#' @return This function returns an env data frame (tibble [560 × 15] (S3: grouped_df/tbl_df/tbl/data.frame))
#' with the following entries:
#' \describe{
#'     \item{\code{resource} (chr)}{name of the seized resource: 'bed' 'bed' 'bed' 'bed' ...}
#'     \item{\code{time} (num)}{time step:  3 10 12 13 14 15 15 15 15 16 ...}
#'     \item{\code{server} (int)}{server: 1 2 3 2 3 4 3 4 5 6 ...}
#'     \item{\code{limit} (num)}{limit: Inf Inf Inf Inf Inf ...}
#'     \item{\code{replication} (int)}{replication:  1 1 1 1 1 1 1 1 1 1 ...}
#'     \item{\code{upper} (int)}{upper: 1 2 3 2 3 5 5 5 5 7 ...}
#'     \item{\code{lower} (int)}{lower: 1 2 3 2 3 3 3 3 3 5 ...}
#'     \item{\code{med} (num)}{med: 1 2 3 2 3 4 4 4 4 6 ...}
#'     \item{\code{date} (POSIXct)}{time, format: \code{yyyy-mm-dd hh:mm.ss}}
#'     \item{\code{rwdate} (POSIXct)}{format: '2020-03-01' '2020-03-08' '2020-03-15' '2020-03-15' ...}
#'     \item{\code{source} (chr)}{name of the simulation that was used: 'babsim' 'babsim' 'babsim' 'babsim' ...}
#'     }
#'
#' @examples
#' x <- rep(0.2, 29)
#' para <- mapXToPara(x)
#' conf <- babsimToolsConf()
#' data <- getObkData()
#' res <- modelResultHospital(para = para, conf = conf, data = data)
#' getError(res = res, conf = conf)
#' p <- plotDailyMaxResults(res)
#' @export

mapXToPara <- function(x) {
  para <- babsimHospitalPara()
  para$AmntDaysInfectedToHospital <- x[1]
  para$AmntDaysNormalToHealthy <- x[2]
  para$AmntDaysNormalToIntensive <- x[3]
  para$AmntDaysNormalToVentilation <- x[4]
  para$AmntDaysNormalToDeath <- x[5]
  para$AmntDaysIntensiveToAftercare <- x[6]
  para$AmntDaysIntensiveToVentilation <- x[7]
  para$AmntDaysIntensiveToDeath <- x[8]
  para$AmntDaysVentilationToIntensiveAfter <- x[9]
  para$AmntDaysVentilationToDeath <- x[10]
  para$AmntDaysIntensiveAfterToAftercare <- x[11]
  para$AmntDaysIntensiveAfterToDeath <- x[12]
  para$GammaShapeParameter <- x[13]
  para$FactorPatientsInfectedToHospital <- x[14]
  para$FactorPatientsHospitalToIntensive <- x[15]
  para$FactorPatientsHospitalToVentilation <- x[16]
  para$FactorPatientsNormalToIntensive <- x[17]
  para$FactorPatientsNormalToVentilation <- x[18]
  para$FactorPatientsNormalToDeath <- x[19]
  para$FactorPatientsIntensiveToVentilation <- x[20]
  para$FactorPatientsIntensiveToDeath <- x[21]
  para$FactorPatientsVentilationToIntensiveAfter <- x[22]
  para$FactorPatientsIntensiveAfterToDeath <- x[23]
  para$AmntDaysAftercareToHealthy <- x[24]
  para$RiskFactorA <- x[25]
  para$RiskFactorB <- x[26]
  para$RiskMale <- x[27]
  para$AmntDaysIntensiveAfterToHealthy <- x[28]
  para$FactorPatientsIntensiveAfterToHealthy <- x[29]
  return(para)
}
