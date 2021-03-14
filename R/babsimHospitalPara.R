#' @title babsimHospitalPara
#'
#' @description Default Control list for babsimHospital
#' This function returns the default controls for the functions \code{\link{babsimHospital}}.
#' Control is a list of the following settings.
#' Note: dependent parameters that are based on other parameters (e.g., probabilities that add to 1.0) are marked with an asterisk (*).
#' Note: parameters that are currently not used, are marked with a double asterisk (**).
#' \describe{
#'   \item{\code{logLevel}}{if larger than 10, shown detailed simmer output. simmer \code{log_} level, default is \code{0}.}
#' 1
#'   \item{\code{FactorPatientsInfectedToHealthy*}}{Z1: Infected -> Healthy: percentage of patients that move from state infected to healthy, default is \code{0.831}.
#'   Note: not used. Value is internally calculated as: \code{1 - FactorPatientsInfectedToHospital}}
#'   \item{\code{AmntDaysInfectedToHealthy**}}{Z1: Infected -> Healthy: duration (in days) if patients move from state infected to healthy, default is \code{20.5}.
#'   Note: not used, because not modeled.}
#' 2
#'   \item{\code{FactorPatientsInfectedToHospital}}{Z2: Infected -> Hospital: percentage of patients that move from state infected to hospital, default is \code{0.169}.}
#'   \item{\code{AmntDaysInfectedToHospital}}{Z2: Infected -> Hospital: duration (in days) if patients move from state infected to hospital, default is \code{8.4}.}
#'
#' 3
#'   \item{\code{FactorPatientsHospitalToNormal*}}{Z3: Hospital -> Normal: percentage of patients that move from state hospital to normal, default is \code{0}.
#'   Note: not used. Value is internally calculated as: \code{1 - FactorPatientsHospitalToIntensive - FactorPatientsHospitalToVentilation} }
#'   \item{\code{AmntDaysHospitalToNormal*}}{Z3: Hospital -> Normal: duration (in days) if patients move from state hospital to normal, default is \code{1e6}.
#'   Note: not used. Patients move from hospital to normal immediately}
#' 4
#'   \item{\code{FactorPatientsHospitalToIntensive}}{Z4: Hospital -> Intensive: percentage of patients that move from state hospital to intensive, default is \code{0.012}.}
#'   \item{\code{AmntDaysHospitalToIntensiv*}}{Z4: Hospital -> Intensive: duration (in days) if patients move from state hospital to intensive, default is \code{1e6}. Note: not used.
#'   Patients move from hospital to intensive immediately}
#' 5
#'   \item{\code{FactorPatientsHospitalToVentilation}}{Z5: Hospital -> Ventilation: percentage of patients that move from state hospital to ventilation, default is \code{0.036}.}
#'   \item{\code{AmntDaysHospitalToVentilation*}}{Z5: Hospital -> Ventilation: duration (in days) if patients move from state hospitel to ventilation, default is \code{1e6}. Note: not used.
#'   Patients move from hospital to intensive immediately}
#' 6
#'   \item{\code{FactorPatientsNormalToHealthy*}}{Z6: Normal -> Healthy: percentage of patients that move from state normal to healthy, default is \code{0}. Note: not used.
#'   Value is internally calculated as: \code{1 - FactorPatientsNormalToIntensive - FactorPatientsNormalToVentilation - FactorPatientsNormalToDeath }}
#'   \item{\code{AmntDaysNormalToHealthy}}{Z6: Normal -> Healthy: duration (in days) if patients move from state normal to healthy, default is \code{11.6}.}
#' 7
#'   \item{\code{FactorPatientsNormalToIntensive}}{Z7: Normal -> Intensive: percentage of patients that move from state normal to intensive, default is \code{0.0506}.}
#'   \item{\code{AmntDaysNormalToIntensive}}{Z7: Normal -> Intensive: duration (in days) if patients move from state normal to intensive, default is \code{1.25}.}
#' 8
#'   \item{\code{FactorPatientsNormalToVentilation}}{Z8: Normal -> Ventilation: percentage of patients that move from state normal to ventilation, default is \code{0.1013}.}
#'   \item{\code{AmntDaysNormalToVentilation}}{Z8: Normal -> Ventilation: duration (in days) if patients move from state normal to ventilation, default is \code{3.63}.}
#' 9
#'   \item{\code{FactorPatientsNormalToDeath}}{Z9: Normal -> Death: percentage of patients that move from state normal to death, default is \code{0.139}.}
#'   \item{\code{AmntDaysNormalToDeath}}{Z9: Normal -> Death: duration (in days) if patients move from state normal to death, default is \code{11.4}.}
#' 10
#'   \item{\code{FactorPatientsIntensiveToAftercare*}}{Z10: Intensive -> Aftercare: percentage of patients that move from state intensive to aftercare, default is \code{0.25}. Note: not used.
#'   Value is internally calculated as: \code{1 - FactorPatientsIntensiveToVentilation - FactorPatientsIntensiveToDeath - FactorPatientsIntensiveToHealthy}}
#'   \item{\code{AmntDaysIntensiveToAftercare}}{Z10: Intensive -> Aftercare: duration (in days) if patients move from state intensive to aftercare, default is \code{7.0}.}
#' 11
#'   \item{\code{FactorPatientsIntensiveToVentilation}}{Z11: Intensive > Ventilation: percentage of patients that move from state intensive to ventilation, default is \code{0.25}.}
#'   \item{\code{AmntDaysIntensiveToVentilation}}{Z11: Intensive > Ventilation: duration (in days) if patients move from state intensive to ventilation, default is \code{2.0}.}
#' 12
#'   \item{\code{FactorPatientsIntensiveToDeath}}{Z12: Intensive -> Death: percentage of patients that move from state intensive to death, default is \code{0.25}.}
#'   \item{\code{AmntDaysIntensiveToDeath}}{Z12: Intensive -> Death: duration (in days) if patients move from state intensive to death, default is \code{2.0}.}
#' 12a
#'   \item{Removed in v11: \code{FactorPatientsIntensiveToHealthy}}{Z12a: Intensive -> Healthy: percentage of patients that move from state intensive  to healthy, default is \code{0.25}.}
#'   \item{\code{AmntDaysIntensiveToHealthy}}{Z12a: Intensive -> Healthy: duration (in days) if patients move from state intensive to healthy, default is \code{13.0}.}
#' 13
#'   \item{Removed in v11: \code{FactorPatientsVentilationToAftercare*}}{Z13: Ventilation -> Aftercare: percentage of patients that move from state ventilation to aftercare, default is \code{0.08}. Note: not used.
#'   Value is internally calculated as: \code{1 - FactorPatientsVentilationToIntensiveAfter - FactorPatientsVentilationToDeath}}
#'   \item{Removed in v11: \code{AmntDaysVentilationToAftercare}}{Z13: Ventilation -> Aftercare: duration (in days) if patients move from state ventilation to aftercare, default is \code{9.0}.}
#' 14
#'   \item{\code{FactorPatientsVentilationToIntensiveAfter}}{Z14: Ventilation -> IntensiveAfter: percentage of patients that move from state ventilation to intensiveAfter, default is \code{0.42}.}
#'   \item{\code{AmntDaysVentilationToIntensiveAfter}}{Z14: Ventilation -> IntensiveAfter: duration (in days) if patients move from state ventilation to intensiveAfter, default is \code{23.0}.}
#' 15
#'   \item{Removed in v11: \code{FactorPatientsVentilationToDeath}}{Z15: Ventilation -> Death: percentage of patients that move from state ventilation to death, default is \code{0.5}.}
#'   \item{\code{AmntDaysVentilationToDeath}}{Z15: Ventilation -> Death: duration (in days) if patients move from state  ventilation to death, default is \code{16.0}.}
#' 16
#'   \item{\code{FactorPatientsAftercareToHealthy*}}{Z16: Aftercare -> Healthy: percentage of patients that move from state aftercare to healthy, default is \code{1.0}. Note: not used.
#'   Value is \code{1}. No branching required, because there is no alternative.}
#'   \item{\code{AmntDaysAftercareToHealthy}}{Z16: Aftercare -> Healthy: duration (in days) if patients move from state aftercare to healthy, default is \code{21.0}.}
#' 17 I
#'   \item{\code{FactorPatientsIntensiveAfterToAftercare*}}{Z17I: IntensiveAfter -> Aftercare: percentage of patients that move from state intensiveAfter to aftercare, default is \code{0.5}. Note: not used.
#'   Value is internally calculated as: \code{1 - FactorPatientsIntensiveAfterToHealthy - FactorPatientsIntensiveAfterToDeath - FactorPatientsIntensiveAfterToHealthy}}
#'   \item{\code{AmntDaysIntensiveAfterToAftercare}}{Z17I: IntensiveAfter -> Aftercare: duration (in days) if patients move from state intensiveAfter to aftercare, default is \code{7.0}.}
#' 17 II
#'   \item{Removed in v11: \code{AmntDaysIntensiveAfterToHealthy}}{Z17II: IntensiveAfter -> Healthy: duration (in days) if patients move from state intensiveAfter to healthy, default is \code{18.0}.}
#' 18
#'   \item{\code{FactorPatientsIntensiveAfterToDeath}}{IntensiveAfter -> Death: percentage of patients that move from state intensiveAfter to death, default is \code{0.0}.}
#'   \item{\code{AmntDaysIntensiveAfterToDeath}}{IntensiveAfter -> Death: duration (in days) if patients move from state intensiveAfter to death, default is \code{1e-6}.}
#'   \item{\code{GammaShapeParameter}}{Gamma shape parameter, default is \code{1} (exponential distribution).}
#'   \item{\code{RiskFactorA}}{Parameter a in the exponential function r(x) = a exp(b x) that models the risk r as a function of the age x, default is \code{0.02048948}.}
#'   \item{\code{RiskFactorB}}{Parameter b in the exponential function r(x) = a exp(b x) that models the risk r as a function of the age x, default is \code{0.07138200}.}
#'   \item{\code{RiskMale}}{Death risk of male patients compared to female , default is \code{2}.}
#'  \item{\code{AmntDaysIntensiveAfterToHealthy}}{IntensiveAfter -> Healthy: duration (in days) if patients move from state intensiveAfter to death, default is \code{3}.}
#'   \item{\code{FactorPatientsIntensiveAfterToHealthy}}{IntensiveAfter -> Healthy: percentage of patients that move from state intensiveAfter to healthy, default is \code{0.67}.}
#'   }
#' @return a list
#'
#' @examples
#'
#' # change Gamma parameter
#' x <- babsimHospitalPara()
#' x$GammaShapeParameter <- 1.0
#' @export
babsimHospitalPara <- function() {
  minVal <- 1e-6
  list(
    AmntDaysInfectedToHospital = 9.5, # 8.4, # x1
    AmntDaysNormalToHealthy = 10, # 11.6, #x2
    AmntDaysNormalToIntensive = 5, # 1.25, # x3
    AmntDaysNormalToVentilation = 3.63, # x4
    AmntDaysNormalToDeath = 5, # 11.4, # x5
    AmntDaysIntensiveToAftercare = 7.0, # x6
    AmntDaysIntensiveToVentilation = 4, # 2.0, # x7
    AmntDaysIntensiveToDeath = 5, # 6 , # 2.0, # x8
    AmntDaysVentilationToIntensiveAfter = 30, # 23.0, #  x9
    AmntDaysVentilationToDeath = 20, # 16, # x10
    AmntDaysIntensiveAfterToAftercare = 3.0, # x11
    AmntDaysIntensiveAfterToDeath = 4, # 1, # x12
    GammaShapeParameter = 1.0, # x13
    FactorPatientsInfectedToHospital = 0.1, # 0.169, # x14
    FactorPatientsHospitalToIntensive = 0.09, # 0.04,  #0.012, # x15
    FactorPatientsHospitalToVentilation = 0.01, # 0.036, #x16
    FactorPatientsNormalToIntensive = 0.1, # 0.0506, #x17
    FactorPatientsNormalToVentilation = 0.001, # 0.1013, #x18
    FactorPatientsNormalToDeath = 0.1, # 0.139, # x19
    FactorPatientsIntensiveToVentilation = 0.3, # 0.25, # x20
    FactorPatientsIntensiveToDeath = 0.1, # 0.25, # x21
    FactorPatientsVentilationToIntensiveAfter = 0.7, # 0.42, # x22
    FactorPatientsIntensiveAfterToDeath = 1e-5, # x23
    AmntDaysAftercareToHealthy = 3, # 21, # x24
    RiskFactorA = 0.02048948, # x25
    RiskFactorB = 0.01, # 26
    RiskMale = 1.5, # x27
    # Added again in v11.5:
    AmntDaysIntensiveAfterToHealthy = 3.0, # x28
    # Added in v11.5:
    FactorPatientsIntensiveAfterToHealthy = 0.67 # x29
  )
}
