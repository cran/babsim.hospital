#' Extract and summarize BaBSim results
#'
#' @param envs List of simulation environments
#' @param conf Configuration for simulation runs in \code{envs}
#'
#' @description
#' Given a list of result environments returned by \code{\link{babsimHospital}}, extract and summarize resource usage for each day.
#' If the resource usage fluctuates during the day, the maximum usage is reported.
#'
#' @return
#' A \code{data.table} with columns
#' \describe{
#'   \item{\code{date}}{Day on which the resource is required.}
#'   \item{\code{resource}}{The resource required.}
#'   \item{\code{replication}}{From which replication the result stems.}
#'   \item{\code{count}}{Number of units of \code{resource} in use.}
#' }
#' @importFrom data.table :=
#' @importFrom checkmate assertList
#' @importFrom checkmate assertNames
#' @export
extractSimulationResults <- function(envs, conf) {
  assertList(envs)
  assertList(conf)
  assertNames(names(conf), must.include=c("simulationDates", "ResourceNames"))
  assertList(conf$simulationDates)
  assertNames(names(conf$simulationDates), must.include=c("StartDate", "EndDate"))

  simStartDate <- conf$simulationDates$StartDate
  simEndDate <- conf$simulationDates$EndDate
  # Time is the synthetic simulation time starting at 0 and going up by 1 for
  # each day. Because we don't have simulation events on every day, we generate
  # a vector containing all days for which we want to return results (i.e. each
  # day in [simStartDate, simEndDate]).
  time <- as.integer(seq(simStartDate, to=simEndDate, by=1) - simStartDate)
  resource <- conf$ResourceNames

  # Extract raw results of simulation
  res <- simmer::get_mon_resources(envs)

  data.table::setDT(res)
  # Remove columns we don't need
  res[, c("queue", "capacity", "queue_size", "system", "limit") := NULL]

  # Round time down to next integer since we are only interested in daily
  # changes in resource consumption 
  res[, time := as.integer(floor(time))]

  # Aggregate by time and resource, one column for each replication.
  #
  # Becasue max() in base R is "unique" in that max(c()) == -Inf, we have to supply our
  # own version of max that returns NA for zero length inputs. 
  wide <- data.table::dcast(res, time + resource ~ replication, value.var="server", fun.aggregate=aggMax)

  # From now on we don't need res anymore but res might be large (many events
  # per day), so we remove it expliticly to free up memory.
  rm(res)

  # Now make sure we have one row for every day and resource by merging with
  # the cross join of the two.
  wide <- merge(data.table::CJ(time, resource), wide, all.x=TRUE)

  # Some days will not have any change in resource consumption. To fill these
  # NAs, we need to carry forward the last valid observation (LOCF).
  data.table::setnafill(wide, "locf", cols=colnames(wide)[-2:-1])

  # Any remaining NAs are for the first few days where we haven't observed any
  # resource consumption. Fill these with 0 explicitly.
  data.table::setnafill(wide, fill=0, cols=colnames(wide)[-2:-1])

  # Now that we have fixed up the results, melt() the wide data into long
  # format with an additional column indicating which replication each row
  # corresponds to.
  long <- data.table::melt(wide, id.vars=c("time", "resource"),
                           variable.name="replication", value.name="count")

  # Finally replace artificial (0 based) `time` with true `date`:
  long[, date := simStartDate + time]
  long[, time := NULL]
  long
}

#' @title Sane max for aggregation
#'
#' @details R's \code{max} returns \sQuote{-Inf} for empty lists. 
#' This is undesirable for aggregation where we would rather have \sQuote{NA}.
#'
#' @param x [\code{vector(n)}] \cr
#'        numerical or character argument
#'
#' @return Maximum value in \sQuote{x} or \sQuote{NA} if \sQuote{x} is empty.
aggMax <- function(x) {
    if (length(x) > 0) {
          max(x)
  } else {
        NA
    }
}

