#' @title getRkiRisk
#'
#' @description Calculate risk for RKI data
#' @param rki data.frame of downloaded rki data
#' @param para parameter
#'
#' @return a data.frame
#'
#' @examples
#'
#' rki <- getRkiData(rkidata[1:10, ])
#' para <- babsimHospitalPara()
#' # get risk for the first 10 entries:
#' rkiWithRisk <- getRkiRisk(rki = rki, para)
#' @export

getRkiRisk <- function(rki, para) {
  rki$Risk <- para$RiskFactorA * exp(para$RiskFactorB * rki$Age) * ifelse(rki$Geschlecht == "M", para$RiskMale, 1)
  rki
}
