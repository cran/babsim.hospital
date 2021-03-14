#' @title Calculate risk scores
#'
#' @description Calculate a risk score based on age and sex of patient for a set of cases.
#'
#' @details
#' The risk score is calculated according to the following formula:
#' \deqn{\code{RiskScore} = \code{RiskFactorA}  exp(\code{RiskFactorB} \code{age}) (1 + I_{male} (\code{RiskMale} - 1)}
#' Here the \code{RiskFactorA}, \code{RiskFactorB} and \code{RiskMale} are taken from the parameter list \code{par} and the \code{age} and \code{sex} are taken from the \code{cases} .
#' 
#' @param cases [\code{object}] \cr table of cases, one per row.
#' @param par   [\code{list()}] \cr parameters of risk model.
#'
#' @return Vector of risk scores.
#'
#' @importFrom checkmate assertNames
#' @importFrom checkmate assertList
#' @importFrom checkmate assertDataTable
#' @export
RiskScore <- function(cases, par) {
  assertList(par)
  assertNames(names(par), must.include=c("RiskFactorA", "RiskFactorB", "RiskMale"))
  assertDataTable(cases)

  par$RiskFactorA * exp(par$RiskFactorB * cases$age) * ifelse(cases$sex == "male", par$RiskMale, 1)
}
