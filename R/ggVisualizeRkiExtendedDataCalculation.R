#' @title  Data precalculation for ggVisualizeRkiExtended
#'
#' @description Data for ggplot RKI data as result from \code{\link{extendRki}}
#'
#' @seealso \code{\link{rkidata}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 ggtitle
#'
#' @param data rki data as preprocessed by \code{\link{extendRki}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{'2020-05-01'}
#' @param simplify logical. Simplify presentation, Default: TRUE.
#'
#' @examples
#' data <- getRkiData(babsim.hospital::rkidata[1:10000, ])
#' # data size sufficient?: 
#' if (dim(data)[1]> 1e6){
#' p <- ggVisualizeRkiExtended(
#'   data = extendRki(data),
#'   region = 5374, StartDate = "2020-10-01"
#' )
#' }
#' @export
ggVisualizeRkiExtendedDataCalculation <- function(data = extendRki(), region = 5374,
                                                  StartDate = "2020-10-01", simplify = TRUE) {
  Day <- Freq <- Age <- Infected <- InfectedWeekly <- value <- variable <- NULL
  data$time <- NULL
  data$Altersgruppe <- apply(as.matrix(data$Age), 1, FUN = mapAgeToAgeGroup)

  data$Age <- NULL
  data$Altersgruppe <- as.factor(data$Altersgruppe)
  data$Geschlecht <- as.factor(data$Geschlecht)
  region <- as.integer(region)
  ## Landkreis
  if (region > 0 & region > 100) {
    data <- data[data$IdLandkreis == region, ]
  }
  ## Bundesland
  if (region > 0 & region < 100) {
    data <- data[data$IdBundesland == region, ]
  }
  data$IdLandkreis <- NULL
  data$IdBundesland <- NULL
  data <- data[which(as.Date(data$Day) >= as.Date(StartDate)), ]
  data$AnzahlFall <- rep(1, nrow(data))
  rki <- data
  ## Aggregate
  rki$Refdatum <- as.Date(rki$Day)
  rki$Weiblich <- as.integer((rki$Geschlecht == "W") * (rki$AnzahlFall))
  rki$Maennlich <- as.integer((rki$Geschlecht == "M") * (rki$AnzahlFall))
  rki$GUnbekannt <- as.integer((rki$Geschlecht == "unbekannt") * (rki$AnzahlFall))
  # 'A00-A04' 'A05-A14' 'A15-A34' 'A35-A59' 'A60-A79' 'A80+' 'unbekannt'
  rki$A00A04 <- as.integer((rki$Altersgruppe == "A00-A04") * (rki$AnzahlFall))
  rki$A05A14 <- as.integer((rki$Altersgruppe == "A05-A14") * (rki$AnzahlFall))
  rki$A15A34 <- as.integer((rki$Altersgruppe == "A15-A34") * (rki$AnzahlFall))
  rki$A35A59 <- as.integer((rki$Altersgruppe == "A35-A59") * (rki$AnzahlFall))
  rki$A60A79 <- as.integer((rki$Altersgruppe == "A60-A79") * (rki$AnzahlFall))
  rki$A80 <- as.integer((rki$Altersgruppe == "A80+") * (rki$AnzahlFall))
  rki$AUnbekannt <- as.integer((rki$Altersgruppe == "unbekannt") * (rki$AnzahlFall))

  ## Order Data by date
  rki <- rki[order(rki$Refdatum), ]

  ## Aggregate
  rkiAgg <- as.data.frame(xtabs(AnzahlFall ~ Refdatum, rki))
  rkiAgg$Weiblich <- as.integer(xtabs(Weiblich ~ Refdatum, rki))
  rkiAgg$Maennlich <- as.integer(xtabs(Maennlich ~ Refdatum, rki))
  rkiAgg$GUnbekannt <- as.integer(xtabs(GUnbekannt ~ Refdatum, rki))
  rkiAgg$A00A04 <- as.integer(xtabs(A00A04 ~ Refdatum, rki))
  rkiAgg$A05A14 <- as.integer(xtabs(A05A14 ~ Refdatum, rki))
  rkiAgg$A15A34 <- as.integer(xtabs(A15A34 ~ Refdatum, rki))
  rkiAgg$A35A59 <- as.integer(xtabs(A35A59 ~ Refdatum, rki))
  rkiAgg$A60A79 <- as.integer(xtabs(A60A79 ~ Refdatum, rki))
  rkiAgg$A80 <- as.integer(xtabs(A80 ~ Refdatum, rki))
  rkiAgg$AUnbekannt <- as.integer(xtabs(AUnbekannt ~ Refdatum, rki))

  rkiAgg$Refdatum <- as.Date(rkiAgg$Refdatum)

  colnames(rkiAgg)[which(names(rkiAgg) == "Refdatum")] <- "Day"
  colnames(rkiAgg)[which(names(rkiAgg) == "Freq")] <- "Infected"

  if (simplify == TRUE) {
    rkiAgg$AgeYoung <- rkiAgg$A00A04 + rkiAgg$A05A14 + rkiAgg$A15A34 + rkiAgg$A35A59
    # rkiAgg$MittelA59 <- rkiAgg$A35A59
    rkiAgg$AgeOld60 <- rkiAgg$A60A79 + rkiAgg$A80
    rkiAgg$A00A04 <- rkiAgg$A05A14 <- rkiAgg$A15A34 <- rkiAgg$A35A59 <- rkiAgg$A60A79 <- rkiAgg$A80 <- NULL
  }


  data <- data.frame(rkiAgg)
  k <- colnames(data)
  k2 <- k[!k %in% c("Day", "GUnbekannt", "AUnbekannt")]
  getWeekly <- function(x) {
    slide_dbl(x, ~ mean(.x), .before = (7 - 1))
  }

  varNames <- c()
  for (i in k2) {
    var1 <- paste0(i, "Weekly")
    var2 <- paste0("data$", i)
    y <- getWeekly(eval(parse(text = var2)))
    data <- data.frame(data, assign(var1, y))
    varNames <- c(varNames, var1)
  }

  colnames(data) <- c(k, varNames)
  weeklyData <- data[, varNames]
  xy <- cbind(Day = data[, c("Day")], weeklyData)
  data.table::setDT(xy)
  xymelt <- data.table::melt(xy, id.vars = "Day")
  return(xymelt)
}
