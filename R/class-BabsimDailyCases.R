#' Visualize new daily cases
#'
#' @param object [\code{BabsimCases}] \cr
#'        new daily cases.
#' @param xlab [\code{character(1)}] \cr
#'        a label for the x axis.
#' @param ylab [\code{character(1)}] \cr
#'        a label for the y axis.
#' @param clab [\code{character(1)}] \cr
#'        a label for the fill legend.
#' @param droplevels [\code{logical(1)}] \cr
#'        drop unused levels from \sQuote{ageGroup} and \sQuote{sex} before plotting.
#' @param drop.NA [\code{logical(1)}] \cr
#'        drop rows with missing or \sQuote{NA} values before plotting.
#' @param ... [] \cr
#'        additional arguments. Ignored.
#' 
#' @importFrom stats na.omit
#' @importFrom checkmate assertDataTable
#' @importFrom checkmate assertLogical
#' @importFrom checkmate assertCharacter
#' @export
autoplot.BabsimDailyCases <- function(object, 
                                      xlab="Date", 
                                      ylab="Cases", 
                                      clab="Age", 
                                      droplevels=TRUE,
                                      drop.NA=TRUE,
                                      ...) {
  assertDataTable(object)
  assertCharacter(xlab, len=1)
  assertCharacter(ylab, len=1)
  assertCharacter(clab, len=1)
  assertLogical(droplevels, len=1)
  assertLogical(drop.NA, len=1)

  # Private copy so we can modify it inplace
  object <- data.table::copy(object)
  
  # Drop any rows containing NAs
  if (drop.NA)
    object <- na.omit(object)

  # Drop unused levels if requested.
  if (droplevels) {
    ageGroup <- sex <- NULL
    object[, sex := droplevels(sex)]
    object[, ageGroup := droplevels(ageGroup)]
  }

  plt <- if (nlevels(object[["ageGroup"]]) > 1) {
    # Reverse levels so that the older groups stack above younger ones.
    object[, ageGroup := factor(ageGroup, levels=rev(levels(ageGroup)))]
    ggplot2::ggplot(object, ggplot2::aes_string(x="date", y="cases", fill="ageGroup")) + 
      scale_fill_brewer(clab, palette="Reds", direction=-1)
  } else {
    ggplot2::ggplot(object, ggplot2::aes_string(x="date", y="cases"))
  }

  if (nlevels(object[["sex"]]) > 1) 
    plt <- plt + ggplot2::facet_grid(rows=ggplot2::vars(sex))

  plt + 
    ggplot2::geom_area() + 
    ggplot2::xlab(xlab) + 
    ggplot2::ylab(ylab)
}
