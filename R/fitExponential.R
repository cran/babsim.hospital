#' @title Fit a exponential function to data.
#'
#' @description Fit the model $y = a e^{bx}$ to the provided data.
#'
#' @param x x data
#' @param y y data
#' @param a0 start value (default: 1)
#' @param b0 start value (default: 1)
#'
#' @return Named vector of coefficients ('a', 'b')
#'
#' @importFrom stats nls
#' @importFrom stats coef
#'
#' @examples
#' age <- c(2, 10, 25, 47, 70, 90)
#' risk <- c(0.01, 0.07, 0.15, 0.65, 3, 12.64)
#' plot(age, risk)
#' ab <- fitExponential(x = age, y = risk, a0 = 1, b0 = 0)
#' y <- ab[1] * exp(ab[2] * age)
#' lines(age, y)
#' @export

fitExponential <- function(x, y, a0 = 0, b0 = 0) {
  data <- data.frame(x = x, y = y)
  fit <- nls(y ~ a * exp(b * x), data = data, start = list(a = a0, b = b0))
  return(coef(fit))
}
