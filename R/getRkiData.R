#' @title getRkiData
#'
#' @description Transforms the freshly downloaded rki data into the babsim data frame.
#'  Also imputes mising dates as zero frequency in the data.
#'
#' @param rki data.frame of downloaded rki data before preprocessing
#'
#' @return a data.frame of aggregated data
#'  \describe{
#'     \item{\code{Day}}{Date, format: '2020-01-01' '2020-01-02' '2020-01-03' '2020-01-04' ...}
#'     \item{\code{...}}{ ...}
#'     }
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table dcast
#' @importFrom data.table setnames
#' @importFrom data.table setorder
#' @importFrom data.table setcolorder
#' @importFrom data.table :=
#'
#' @examples
#'
#' data <- getRkiData(rkidata[1:100, ])
#' @export
getRkiData <- function(rki) {
  ## the following variable are local, see
  ## https://www.r-bloggers.com/no-visible-binding-for-global-variable:
  Age <- Altersgruppe <- Day <- IdBundesland <- IdLandkreis <- Refdatum <- time <- NULL

  rki <- as.data.table(rki)
  rki[, `:=`(Day, as.Date(Refdatum))]

  # Aggregate to daily counts of cases per subgroup
  DailyCases <- dcast(rki, Day + Altersgruppe + Geschlecht + IdBundesland + IdLandkreis ~
  ., value.var = "AnzahlFall", fun.aggregate = sum)
  setnames(DailyCases, ".", "Cases")

  if (any(DailyCases$Cases < 0)) {
    # This shouldn't happen, but YOLO!
    message("getRkiData(): Found days with negative number of cases. Ignoring them.")
  }
  # Drop rows with zero or fewer number of cases.
  DailyCases <- DailyCases[Cases > 0, ]

  # Resolve Altersgruppe to average Age
  DailyCases[, `:=`(Age, mapAgeGroupToAge(Altersgruppe))]

  # Add 'linear' time column starting at 0
  StartDate <- min(DailyCases$Day)
  DailyCases[, `:=`(time, as.numeric(Day - StartDate))]

  # Expand into one case per row:
  Cases <- DailyCases[rep.int(1:nrow(DailyCases), times = DailyCases$Cases), ]

  # Drop Cases column
  Cases[, `:=`(Cases, NULL)]
  # Order by Date and regions
  setorder(Cases, Day, IdBundesland, IdLandkreis)

  # FIXME: Is the column order important? If not, remove this line.
  setcolorder(Cases, c(
    "Altersgruppe", "Geschlecht", "Day", "IdBundesland", "IdLandkreis",
    "time", "Age"
  ))

  # FIXME: Can we return the data.table instead?
  as.data.frame(Cases)
}
