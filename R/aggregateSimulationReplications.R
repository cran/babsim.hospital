#' Summarize replications of a simulation
#'
#' @param results Raw simulation results. 
#'        The result of \code{\link{extractSimulationResults}}.
#'
#' @importFrom stats quantile
#' @importFrom checkmate assertDataTable
#' @export
aggregateSimulationReplications <- function(results) {
  # Make the R CMD check gods happy...
  .SD <- NULL

  if (!inherits(results, "data.table")) {
    results <- extractSimulationResults(results)
  }
  # Check after potential call to extractSimulationResults!
  assertDataTable(results)

  agg <- data.table::dcast(results, date + resource ~ .,
                           value.var="count", fill=0,
                           fun.aggregate=list(min=min,
                                              lq=function(x) quantile(x, 0.25),
                                              median=median,
                                              uq=function(x) quantile(x, 0.75),
                                              max=max))
  # dcast() prefixes each new column with the name of value.var. 
  # We don't want that, so fixup the column names
  oldNames <- grep("^count_", colnames(agg), value=TRUE)
  newNames <- gsub("count_", "", oldNames)
  setnames(agg, oldNames, newNames)

  # Since we are counting discrete resources, we want to round them the nearest
  # integer
  agg[, (newNames) := lapply(.SD, round), .SDcols=newNames]
  agg
}
