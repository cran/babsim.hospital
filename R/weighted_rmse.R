#' weighted_rmse
#'
#' Calculate a weighted RMSE. Weights are based on 'time' in the case of the weights variable.
#' (E.g. older errors weigh less). And also based on the 'direction' e.g. predicting to few used ressources
#' is worse than predicting a few ressources used too much.
#'
#' @param actual Real Data, vector of observations
#' @param predicted Predicted Data, vector of observations
#' @param weights Time based decay. Default is an exponential decay
#' @param worsenGoodExpections Factor by how much predicting too few used ressources should be punished more.
#'
#' @return weighted RMSE
#' @export
weighted_rmse <- function(actual, predicted, weights = exp(-(length(actual):1) / 14),
                          worsenGoodExpections = 1.5) {
  scaledweights <- weights / sum(weights)
  error <- predicted - actual
  error[error > 0] <- error[error > 0] - 1
  error[error < 0] <- error[error < 0] * worsenGoodExpections
  sqrt(sum((error)^2 * scaledweights))
}
