#' @title Download German daily case summary from RKI
#'
#' @param dir [\code{character(1)}] \cr 
#'        directory where downloaded data is stored. Defaults to \code{tempdir()}.
#' @param force [\code{bool(1)}] \cr
#'        if true, force download of data even if a previous download is found in \code{dir}.
#'
#' @return An object of class \sQuote{BabsimCasesRki}.
#'
#' @seealso 
#' \itemize{
#'   \item{\code{\link{GermanStates}} for a list of German states and their state id.}
#'   \item{\code{\link{GermanCounties}} for a list of German counties and theirs county id.}
#' }
#'
#' @importFrom checkmate assertCharacter
#' @importFrom checkmate assertDirectory
#' @importFrom checkmate assertLogical
#' @importFrom checkmate assertSubset
#' @export
fetchRkiCases <- function(dir=tempdir(), force=FALSE) {
  # Make R CMD check happy...
  .SD <- Altersgruppe <- GermanCounties <-  GermanStates <- 
    Geschlecht<-  IdBundesland <- IdLandkreis <- Refdatum <- 
    ageGroup <- assertSubset <- sex <- 
    NeuerFall <- countyId <- stateId <- Meldedatum <- Datenstand <- NULL

  # Check all parameters early on to catch any errors before 
  # we do any expensive downloads or transformations.
  assertDirectory(dir, "rw")
  assertLogical(force, len=1)
  
  URL <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
  csvFilename <- file.path(tempdir(),  strftime(Sys.time(), "rki-%Y-%m-%d-%H-UTC.csv"))
  rdsFilename <- file.path(tempdir(), strftime(Sys.time(), "rki-%Y-%m-%d-%H-UTC.rds"))

  # Remove any cached data if force is TRUE
  if (force) {
    if (file.exists(csvFilename)) {
      messagef("Removing cached download '%s' and forcing new download.", csvFilename)
      unlink(csvFilename)
    }
    if (file.exists(rdsFilename)) {
      messagef("Removing cached parsed data file  '%s'.", rdsFilename)
      unlink(rdsFilename)
    }
  }

  # Download and read data
  if (file.exists(rdsFilename)) {
    messagef("Reading cached data file '%s'. Pass 'force=TRUE' to use fresh data.", rdsFilename)
    return(readRDS(rdsFilename))
  }

  if (file.exists(csvFilename)) {
    messagef("Reusing cached download '%s'. Pass 'force=TRUE' to use fresh data.", csvFilename)
  } else {
    messagef("Downloading current RKI dataset to '%s'.", csvFilename)
    curl::curl_download(URL, csvFilename, quiet=FALSE)
  }
  raw <- data.table::fread(file=csvFilename,
                           colClasses=c(IdBundesland="character", IdLandkreis="character"))
  
  # Drop "old" cases which are not part of the "current" reporting. 
  # See https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0
  # for an explanation of the logic behind this.
  raw <- raw[NeuerFall >= 0] 

  if (any(raw$Datenstand != raw$Datenstand[2]))
    warning("Reporting date is different for some cases!", call.=FALSE)
  
  raw[, countyId := IdLandkreis]
  # Thank you RKI for botching this. The 'Region' are the first two digits of
  # the AGS. Don't drop leading zeros!
  raw[, stateId := substr(countyId, 2, 2)]

  raw[Altersgruppe == "unbekannt", Altersgruppe := NA]
  raw[, ageGroup := factor(Altersgruppe, 
                           levels=c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+"),
                           labels=c("0 - 4", "5 - 14", "15 - 34", "35 - 59", "60 - 79", "80+"))]

  raw[, sex := rkiGeschlechtToSex(Geschlecht)]
  
  raw[, date := data.table::as.IDate(Meldedatum)]

  # Remove variables we don't need anymore to save space.
  junkVariables <- c("IdBundesland", "Bundesland", "IdLandkreis", "Landkreis",
                     "Geschlecht", "Altersgruppe", "Altersgruppe2", "Meldedatum",
                     "Refdatum", "ObjectId")
  raw[, (junkVariables) := NULL]

  data.table::setattr(raw,
                      "reportingDate",
                      strptime(raw[1, Datenstand], "%d.%m.%Y, %H:%M Uhr"))
  data.table::setattr(raw, "class", c("BabsimRkiCases", class(raw)))


  # Cache processed data.table for future calls
  saveRDS(raw, rdsFilename)
  raw
}

DailyCases <- function(cases,  from, to, stateId, countyId, ...) {
  if (missing(from))
    from <- min(cases$date)
  if (missing(to))
    to <- max(cases$date)

  daily <- data.table::dcast(subset(cases, from=from, to=to),
                             date + ageGroup + sex ~ .,
                             value.var="AnzahlFall",
                             fun.aggregate=sum)
  data.table::setnames(daily, ".", "cases") 

  # Make sure we have one row for each day, ageGroup and sex combination.  Any
  # missing combinations are added with zero cases.
  allDates <- data.table::CJ(date=seq(from, to, by=1),
                             ageGroup=levels(daily$ageGroup),
                             sex=levels(daily$sex))
  data.table::setkeyv(allDates, c("date", "ageGroup", "sex"))

  daily <- merge(allDates, daily, all.x=TRUE)
  data.table::setnafill(daily, fill=0, cols="cases")
  data.table::setattr(daily, "class", c("BabsimDailyCases", class(daily)))
  daily
}

#{
#  res <- data.table::dcast(raw, date + ageGroup + sex ~ .,
#                           value.var="AnzahlFall",
#                           fun.aggregate=sum)
#}

#' @importFrom checkmate assertDate
#' @export
subset.BabsimRkiCases <- function(x, countyId, stateId, from, to, ...) {
  IdBundesland <- IdLandkreis <- date <- NULL
  ss <- data.table::copy(x)
  
  if (!missing(stateId) && !missing(countyId)) {
    stop("Can only subset by 'stateId' or 'countyId'.")
  } else if (!missing(stateId)) {
    assertCharacter(stateId)
    assertSubset(stateId, babsim.hospital::GermanStates[["stateId"]])

    # Apparently the RKi does not understand the semantics of the German AGS.
    # The state is identified by a two digit code, the RKI "helpfully" drops
    # the leading digit if it is 0. Good job guys!
    ss[, IdBundesland := substr(IdLandkreis, 1, 2)]
    ss <- ss[IdBundesland %in% stateId, ]
  } else if (!missing(countyId)) {
    assertCharacter(countyId)
    assertSubset(countyId, babsim.hospital::GermanCounties[["countyId"]])
    ss <- ss[IdLandkreis %in% countyId, ]
  }

  if (!missing(from)) {
    assertDate(from, len=1)
    ss <- ss[date >= from]
  }

  if (!missing(to)) {
    assertDate(to, len=1)
    ss <- ss[date < to]
  }

  if (nrow(ss) == 0) 
    warning("No data left after subsetting. Check criteria and downloaded data.", call.=FALSE)
  ss
}

#' @title Map \code{Geschlecht} to biological sex
#' 
#' @param geschlecht [\code{character(n)}] \cr
#'        character vector with values in \sQuote{M}, \sQuote{W}, and \sQuote{unbekannt}.
#'
#' @return A factor vector with \code{n} elements and levels \sQuote{male} or \sQuote{female}. 
#'   \sQuote{unbekannt} or other values are mapped to \code{NA}.
rkiGeschlechtToSex <- function(geschlecht) {
  res <- ifelse(geschlecht == "M", "male", ifelse(geschlecht == "W", "female", NA))
  factor(res, levels = c("male", "female"))
}
