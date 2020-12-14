#' @title  visualizeIcu Visualisierung der ICU Daten 
#'
#' @description Quelle: ICU Daten bundesweit
#'
#' @seealso \code{\link{icudata}}
#'
#'
#' @param data icu data, e.g., \code{\link{icudata}}
#' @param region Region: Gemeindeschluessel, \code{int 05374} fuer OBK oder
#' |code{05315} fuer Koeln oder \code{05911} fuer Bochum.
#'
#' @importFrom padr pad
#' @importFrom stats xtabs
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom graphics plot
#'
#' @examples
#' require("stats")
#' icu <- babsim.hospital::icudata
#' icuCov <- as.data.frame(xtabs( faelle_covid_aktuell ~  daten_stand, icu))
#' icuCov$daten_stand <- as.Date(icuCov$daten_stand)
#' icuCovBeatm <- as.data.frame(xtabs(faelle_covid_aktuell_beatmet ~ daten_stand, icu))
#' icuCovBeatm$daten_stand <- as.Date(icuCovBeatm$daten_stand)
#' dataICUBeds <- data.frame(bed=(icuCov$Freq - icuCovBeatm$Freq),
#'                                intensiveBedVentilation=icuCovBeatm$Freq,
#'                                Day =  as.Date(icuCovBeatm$daten_stand))
#'
#' require("padr")
#' require("stats")
#' icu <- babsim.hospital::icudata
#' icu <- pad(icu,interval="day")
#' icuCov <- as.data.frame(xtabs( faelle_covid_aktuell ~ daten_stand, icu))
#' icuCovBeatm <- as.data.frame(xtabs( faelle_covid_aktuell_beatmet   ~ daten_stand, icu))
#' icuCovBett <- as.data.frame(xtabs( betten_belegt   ~ daten_stand, icu))
#' plot(icuCov$daten_stand, icuCov$Freq, type = "p", xlab = "Tag", ylab="COVID ",
#'      main= "COVID-Faelle in Behandlung im KHaus")
#' plot(icuCovBeatm$daten_stand, icuCovBeatm$Freq, type = "p", xlab = "Tag",
#'      ylab="Patienten", main="Beatmete COVID-19-Pat. nur invasive Beatmung und ECMO")
#' plot(icuCovBett$daten_stand, icuCovBett$Freq, type = "l", xlab = "Tag", ylab="ICU Betten belegt")
#'
#' # Nur Daten des OBK:
#' icu <- babsim.hospital::icudata
#' icu <- icu[icu$gemeindeschluessel==05374, ]
#'
#' @format data.frame of 9 variables
#' \describe{
#'   \item{bundesland}{ int  1 1 1 1 1 1 1 1 1 1 ...}
#'   \item{gemeindeschluessel}{ int  1001 1002 1003 1004 1051 1053 1054 1055 1056 1057 ...}
#'   \item{anzahl_meldebereiche}{ int  2 3 2 1 1 2 1 3 2 1 ...}
#'   \item{faelle_covid_aktuell}{ int  0 3 5 1 3 1 0 0 5 1 ...}
#'   \item{faelle_covid_aktuell_beatmet}{ int  0 2 5 1 1 1 0 0 4 0 ...}
#'   \item{anzahl_standorte}{ int  2 3 2 1 1 2 1 3 2 1 ...}
#'   \item{betten_frei}{ int  44 113 115 19 54 7 7 18 10 7 ...}
#'   \item{betten_belegt}{ int  38 110 108 19 26 17 3 34 27 5 ...}
#'   \item{daten_stand}{Date, format: "2020-05-01" "2020-05-01" "2020-05-01" "2020-05-01" ... "2020-08-21"}
#' }
#'
#' @return NULL
#'
#' @export

visualizeIcu <- function(data = babsim.hospital::icudata,
                         region = 05315) {
  ## Landkreis
  if (region > 0 & region > 100) {
    data <- data[data$gemeindeschluessel == region,]
  }
  ## Bundesland
  if (region > 0 & region < 100) {
    data <- data[data$bundesland == region,]
  }
  data <- pad(data, interval = "day")
  dataCov <-
    as.data.frame(xtabs(faelle_covid_aktuell ~ daten_stand, data))
  dataCov$daten_stand <- as.Date(dataCov$daten_stand)
  dataCovBeatm <-
    as.data.frame(xtabs(faelle_covid_aktuell_beatmet   ~ daten_stand, data))
  dataCovBeatm$daten_stand <- as.Date(dataCovBeatm$daten_stand)
  dataCovBett <-
    as.data.frame(xtabs(betten_belegt   ~ daten_stand, data))
  dataCovBett$daten_stand <- as.Date(dataCovBett$daten_stand)
  opar <- par(no.readonly=TRUE)
  on.exit(par(opar))
  par(mfrow = c(1, 2))
  plot(
    dataCov$daten_stand,
    dataCov$Freq,
    type = "b",
    xlab = "Tag",
    ylab = "COVID-Faelle in Behandlung im KHaus",
    main = paste0("COVID-Intensiv. Region: ", region)
  )
  dataCov$weekly <-
    slide_dbl(dataCov$Freq, ~ mean(.x), .before = (7 - 1))
  lines(dataCov$daten_stand, dataCov$weekly, col = "red")
  plot(
    dataCovBeatm$daten_stand,
    dataCovBeatm$Freq,
    type = "b",
    xlab = "Tag",
    ylab = "Beatmete COVID-19-Pat. nur invasive Beatmung und ECMO",
    main = paste0("Beatmete COVID-Intensiv. Region: ", region)
  )
  dataCovBeatm$weekly <-
    slide_dbl(dataCovBeatm$Freq, ~ mean(.x), .before = (7 - 1))
  lines(dataCov$daten_stand, dataCovBeatm$weekly, col = "red")
  par(mfrow = c(1, 1))
}

#' @title  ggVisualizeIcu Visualisierung der ICU Daten 
#'
#' @description Quelle: ICU Daten bundesweit
#'
#' @seealso \code{\link{icudata}}
#' 
#' @param data icu data, e.g., \code{\link{icudata}}
#' @param region Region: Gemeindeschluessel, \code{int 05374} fuer OBK oder
#' |code{05315} fuer Koeln oder \code{05911} fuer Bochum.
#'
#' @importFrom padr pad
#' @importFrom stats xtabs
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 expand_limits
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 theme_dark
#' @importFrom ggplot2 annotate
#'
#' @examples
#' require("stats")
#' icu <- babsim.hospital::icudata
#' icuCov <- as.data.frame(xtabs( faelle_covid_aktuell ~  daten_stand, icu))
#' icuCov$daten_stand <- as.Date(icuCov$daten_stand)
#' icuCovBeatm <- as.data.frame(xtabs(faelle_covid_aktuell_beatmet ~ daten_stand, icu))
#' icuCovBeatm$daten_stand <- as.Date(icuCovBeatm$daten_stand)
#' dataICUBeds <- data.frame(bed=(icuCov$Freq - icuCovBeatm$Freq),
#'                                intensiveBedVentilation=icuCovBeatm$Freq,
#'                                Day =  as.Date(icuCovBeatm$daten_stand))
#'
#' require("padr")
#' require("stats")
#' require("slider")
#' 
#' icu <- babsim.hospital::icudata
#' icu <- pad(icu,interval="day")
#' icuCov <- as.data.frame(xtabs( faelle_covid_aktuell ~ daten_stand, icu))
#' icuCovBeatm <- as.data.frame(xtabs( faelle_covid_aktuell_beatmet   ~ daten_stand, icu))
#' icuCovBett <- as.data.frame(xtabs( betten_belegt   ~ daten_stand, icu))
#' plot(icuCov$daten_stand, icuCov$Freq, type = "p", xlab = "Tag", ylab="COVID ",
#'      main= "COVID-Faelle in Behandlung im KHaus")
#' plot(icuCovBeatm$daten_stand, icuCovBeatm$Freq, type = "p", xlab = "Tag",
#'      ylab="Patienten", main="Beatmete COVID-19-Pat. nur invasive Beatmung und ECMO")
#' plot(icuCovBett$daten_stand, icuCovBett$Freq, type = "l", xlab = "Tag", ylab="ICU Betten belegt")
#'
#' # Nur Daten des OBK:
#' icu <- babsim.hospital::icudata
#' icu <- icu[icu$gemeindeschluessel==05374, ]
#'
#' @format data.frame of 9 variables
#' \describe{
#'   \item{bundesland}{ int  1 1 1 1 1 1 1 1 1 1 ...}
#'   \item{gemeindeschluessel}{ int  1001 1002 1003 1004 1051 1053 1054 1055 1056 1057 ...}
#'   \item{anzahl_meldebereiche}{ int  2 3 2 1 1 2 1 3 2 1 ...}
#'   \item{faelle_covid_aktuell}{ int  0 3 5 1 3 1 0 0 5 1 ...}
#'   \item{faelle_covid_aktuell_beatmet}{ int  0 2 5 1 1 1 0 0 4 0 ...}
#'   \item{anzahl_standorte}{ int  2 3 2 1 1 2 1 3 2 1 ...}
#'   \item{betten_frei}{ int  44 113 115 19 54 7 7 18 10 7 ...}
#'   \item{betten_belegt}{ int  38 110 108 19 26 17 3 34 27 5 ...}
#'   \item{daten_stand}{Date, format: "2020-05-01" "2020-05-01" "2020-05-01" "2020-05-01" ... "2020-08-21"}
#' }
#'
#' @return NULL
#'
#' @export

ggVisualizeIcu <- function(data = babsim.hospital::icudata,
                         region = 05315) {
  
  Freq <- daten_stand <- weekly <- NULL
 ## Landkreis
  if (region > 0 & region > 100) {
    data <- data[data$gemeindeschluessel == region,]
  }
  ## Bundesland
  if (region > 0 & region < 100) {
    data <- data[data$bundesland == region,]
  }
  data <- pad(data, interval = "day")
  ## faelle_covid_aktuell include faelle_covid_aktuell_beatmet, so 
  ## we substract faelle_covid_aktuell_beatmet
  data$faelle_covid_aktuell <- data$faelle_covid_aktuell - data$faelle_covid_aktuell_beatmet
  dataCov <-
    as.data.frame(xtabs(faelle_covid_aktuell ~ daten_stand, data))
  dataCov$daten_stand <- as.Date(dataCov$daten_stand)
  dataCovBeatm <-
    as.data.frame(xtabs(faelle_covid_aktuell_beatmet   ~ daten_stand, data))
  dataCovBeatm$daten_stand <- as.Date(dataCovBeatm$daten_stand)
  dataCovBett <-
    as.data.frame(xtabs(betten_belegt   ~ daten_stand, data))
  dataCovBett$daten_stand <- as.Date(dataCovBett$daten_stand)
  meanDateX <- mean.Date(as.Date(dataCov$daten_stand))
  maxCaseNumberpVent = max(dataCovBeatm$Freq)
  maxCaseNumberpIcu = max(dataCov$Freq)
  if(maxCaseNumberpIcu < maxCaseNumberpVent)
  {
    maxCaseNumber <- maxCaseNumberpVent
  } else {
    maxCaseNumber <- maxCaseNumberpIcu
  }
  
  translator <- golem::get_golem_options('translator')
  if(!is.null(translator)){
      xlabT <- translator$t("Datum")
      labICU <- translator$t("label2")
      labVent <- translator$t("label3")
      plotLegendTitle <- translator$t("Legende")
      legDaily <- translator$t("legDaily")
      legWeek <- translator$t("legWeek")
  }else{
      xlabT <- "Datum"
      labICU <- "Nicht beatmet"
      labVent <- "Invasiv beatmet"
      plotLegendTitle <- "Legende"
      legDaily <- "Tagesmeldung"
      legWeek <- "7-Tage Durchschnitt"
  }
  
  dataCov$weekly <-
    slide_dbl(dataCov$Freq, ~ mean(.x), .before = (7 - 1))
   pIcu <- ggplot() +
     geom_point(aes(x = daten_stand, y = Freq, col = legDaily), data = dataCov, size = 0.8) +#, color = "black") +
     geom_line(aes(x= daten_stand, y = weekly, col = legWeek), data = dataCov) + # color ="blue") +
     scale_colour_manual(values=c("blue", "black")) +
     ggplot2::labs(color = plotLegendTitle, title = "DIVI-Intensivregister") + 
     xlab(xlabT) +
     ylab(labICU) +
     expand_limits(y = c(0, maxCaseNumber)) +
     theme(plot.title = element_text(hjust = 0.5))# +

   dataCovBeatm$weekly <-
       slide_dbl(dataCovBeatm$Freq, ~ mean(.x), .before = (7 - 1))
   pVent <- ggplot() +
     geom_point(aes(x = daten_stand, y = Freq, col = legDaily), data = dataCovBeatm, size = 0.8) + #, color ="black") +
     geom_line(aes(x= daten_stand, y = weekly, col = legWeek), data = dataCovBeatm) + # ,color= "green") +
     scale_colour_manual(values=c("green", "black")) +
     xlab(xlabT) +
     ylab(labVent) +
     expand_limits(y = c(0, maxCaseNumber))+
     theme(plot.title = element_text(hjust = 0.5)) +
   ggplot2::labs(color = plotLegendTitle)
     
   pl <- list(pIcu, pVent) 
}



#' @title  visualizeRki Visualisierung der RKI Daten
#'
#' @description Quelle: RKI Daten bundesweit
#'
#' @seealso \code{\link{rkidata}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom graphics plot
#'
#' @param data rki data, e.g., \code{\link{rkidata}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{"2020-05-01"}
#'
#'
#' @export

visualizeRki <- function(data = babsim.hospital::rkidata,
                         region = 5374,
                         StartDate = "2020-05-01") {
  ## Landkreis
  if (region > 0 & region > 100) {
    data <- data[data$IdLandkreis == region,]
  }
  ## Bundesland
  if (region > 0 & region < 100) {
    data <- data[data$IdBundesland == region,]
  }
  data <- data[which(data$Refdatum >= as.Date(StartDate)), ]
  data$Refdatum <- as.Date(data$Refdatum)
  dataAgg <- as.data.frame(xtabs(AnzahlFall   ~ Refdatum, data))
  dataAgg$Refdatum <- as.Date(dataAgg$Refdatum)
  plot(
    dataAgg$Refdatum,
    dataAgg$Freq,
    type = "b",
    ylab = "Faelle",
    xlab = "Refdatum",
    main = paste0("Infizierte. Region: ", region)
  )
  dataAgg$weekly <-
    slide_dbl(dataAgg$Freq, ~ mean(.x), .before = (7 - 1))
  lines(dataAgg$Refdatum, dataAgg$weekly, col = "red")
}


#' @title  ggVisualizeRki ggPlot Visualisierung der RKI Daten
#'
#' @description Quelle: RKI Daten bundesweit
#'
#' @seealso \code{\link{rkidata}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom ggplot2 scale_fill_brewer
#'
#' @param data rki data, e.g., \code{\link{rkidata}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{"2020-05-01"}
#'
#'
#' @export

ggVisualizeRki <- function(data = babsim.hospital::rkidata,
                           region = 5374,
                           StartDate = "2020-05-01") {
  Freq <- Refdatum <- weekly <- NULL
  ## Landkreis
  if (region > 0 & region > 100) {
    data <- data[data$IdLandkreis == region,]
  }
  ## Bundesland
  if (region > 0 & region < 100) {
    data <- data[data$IdBundesland == region,]
  }
  data <- data[which(data$Refdatum >= as.Date(StartDate)), ]
  data$Refdatum <- as.Date(data$Refdatum)
  dataAgg <- as.data.frame(xtabs(AnzahlFall   ~ Refdatum, data))
  dataAgg$Refdatum <- as.Date(dataAgg$Refdatum)
  dataAgg$weekly <-
    slide_dbl(dataAgg$Freq, ~ mean(.x), .before = (7 - 1))
  maxCaseNumberdataAgg = max(dataAgg$Freq)
  
  translator <- golem::get_golem_options('translator')
  if(!is.null(translator)){
      xlabT <- translator$t("Datum")
      ylabT <- translator$t("infPerDay")
      plotLegendTitle <- translator$t("Legende")
      legDaily <- translator$t("legDaily")
      legWeek <- translator$t("legWeek")
  }else{
      xlabT <- "Datum"
      ylabT <- "Infektionen"
      plotLegendTitle <- "Legende"
      legDaily <- "Tagesmeldung"
      legWeek <- "7-Tage Durchschnitt"
  }
  
  rki <- ggplot() +
    geom_point(aes(x = Refdatum, y = Freq, col = legDaily ), data = dataAgg, size = 0.5) +
    geom_line(aes(x= Refdatum, y = weekly, col = legWeek), data = dataAgg) +
    scale_colour_manual(values=c("red", "black")) +
    ggplot2::labs(color = plotLegendTitle, title = "Robert-Koch Institut (RKI)") + 
    xlab(xlabT) +
    ylab(ylabT) +
    expand_limits(y = c(0, maxCaseNumberdataAgg))+
    theme(plot.title = element_text(hjust = 0.5))
}




#' @title  visualizeRkiEvents Visualisation of the pre-processed RKI Data
#'
#' @description RKI data as result from \code{getRkiData(babsim.hospital::rkidata)}
#'
#' @seealso \code{\link{getRkiData}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom graphics plot
#'
#' @param data rki data as preprocessed by \code{\link{getRkiData}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{"2020-05-01"}
#'
#' @examples
#'
#' p <- visualizeRkiEvents(getRkiData(babsim.hospital::rkidata[1:1000,]))
#'
#'
#' @export

visualizeRkiEvents <-
  function(data = getRkiData(babsim.hospital::rkidata),
           region = 0,
           StartDate = "2020-05-01") {
    ## Landkreis
    if (region > 0 & region > 100) {
      data <- data[data$IdLandkreis == region,]
    }
    ## Bundesland
    if (region > 0 & region < 100) {
      data <- data[data$IdBundesland == region,]
    }
    data <- data[which(data$Day >= as.Date(StartDate)), ]
    data$w <- rep(1, dim(data)[1])
    dataAgg <- as.data.frame(xtabs(w   ~ Day, data))
    dataAgg$Day <- as.Date(dataAgg$Day)
    plot(
      dataAgg$Day,
      dataAgg$Freq,
      type = "b",
      ylab = "COVID-19 Cases",
      xlab = "Date",
      main = paste0("Infizierte. Region: ", region)
    )
    dataAgg$weekly <-
      slide_dbl(dataAgg$Freq, ~ mean(.x), .before = (7 - 1))
    lines(dataAgg$Day, dataAgg$weekly, col = "red")
  }



#' @title  ggVisualizeRkiEvents Visualisation of the pre-processed RKI Data
#'
#' @description ggplot RKI data as result from \code{getRkiData(babsim.hospital::rkidata)}
#'
#' @seealso \code{\link{getRkiData}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_color_gradient2
#'
#' @param data rki data as preprocessed by \code{\link{getRkiData}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{"2020-05-01"}
#'
#' @examples
#' # use 10000 data points only:
#' data <- getRkiData(babsim.hospital::rkidata[1:10000,])
#' p <- ggVisualizeRkiEvents(data=data, region = 0, StartDate="2020-10-01")
#'
#'
#' @export

ggVisualizeRkiEvents <-
  function(data = getRkiData(babsim.hospital::rkidata),
           region = 5374,
           StartDate = "2020-10-01") {
    Day <- Freq <- Age <- NULL
    region <- as.integer(region)
    ## Landkreis
    if (region > 0 & region > 100) {
      data <- data[data$IdLandkreis == region,]
    }
    ## Bundesland
    if (region > 0 & region < 100) {
      data <- data[data$IdBundesland == region,]
    }
    data <- data[which(data$Day >= as.Date(StartDate)), ]
    data$w <- rep(1, dim(data)[1])
    dataAgg <- as.data.frame(xtabs(w ~ Day, data))
    dataAgg$Day <- as.Date(dataAgg$Day)
    data$Age <- apply(as.matrix(data$Altersgruppe), 1, FUN = getRealAge)
    dataAgg$Age <- as.integer(xtabs(Age ~ Day, data))
    dataAgg$Age <- dataAgg$Age / dataAgg$Freq
    p <- ggplot(data = dataAgg,
                aes(x = Day, y = Freq, color = Age)) + geom_point() + geom_line()
    mid <- mean(dataAgg$Age)
    print(mid)
    p + scale_color_gradient2(
      midpoint = mid,
      low = "blue",
      mid = "gray",
      high = "red",
      space = "Lab"
    )
  }



#' @title  ggVisualizeRkiAge Visualisation of the pre-processed RKI Data with
#' respect to age and gender
#'
#' @description ggplot RKI data as result from \code{getRkiData(babsim.hospital::rkidata)}
#'
#' @seealso \code{\link{rkiToBabsimData}}
#'
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme_bw
#' @importFrom reshape2 melt
#'
#'
#' @param data rki data as preprocessed by \code{\link{rkiToBabsimData}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{"2020-05-01"}
#' @param simplify logical. Simplify presentation, Default: TRUE.
#'
#' @export

ggVisualizeRkiAge <- function(data = babsim.hospital::rkidata,
                              region = 5374,
                              StartDate = "2020-10-01",
                              simplify = TRUE) {
  Day <-
    Freq <-
    Age <- Infected <- InfectedWeekly <- value <- variable <- NULL
  region <- as.integer(region)
  ## Landkreis
  if (region > 0 & region > 100) {
    data <- data[data$IdLandkreis == region,]
  }
  ## Bundesland
  if (region > 0 & region < 100) {
    data <- data[data$IdBundesland == region,]
  }
  data$Refdatum <- as.Date(data$Refdatum)
  data <- rkiToBabsimData(data)
  data <- data[which(as.Date(data$Day) >= as.Date(StartDate)), ]
  
  if (simplify == TRUE) {
    data$AgeYoung <-
      data$A00A04 + data$A05A14 + data$A15A34 + data$A35A59
    #data$MittelA59 <- data$A35A59
    data$AgeOld60 <- data$A60A79 + data$A80
    data$A00A04 <-
      data$A05A14 <-
      data$A15A34 <- data$A35A59 <- data$A60A79 <- data$A80 <- NULL
  }
  
  k <- colnames(data)
  k2 <- k[!k %in% c("Day", "GUnbekannt", "AUnbekannt")]
  getWeekly <- function(x) {
    slide_dbl(x, ~ mean(.x), .before = (7 - 1))
  }
  
  varNames <- c()
  for (i in k2) {
    var1 <- paste0(i, "Weekly")
    var2 <- paste0("data$", i)
    y <- getWeekly(eval(parse (text = var2)))
    data <- data.frame(data,  assign(var1,  y))
    varNames <- c(varNames, var1)
  }
  
  colnames(data) <- c(k , varNames)
  weeklyData <- data[, varNames]
  xy <- cbind(Day = data[, c("Day")], weeklyData)
  xymelt <- reshape2::melt(xy, id.vars = "Day")
  ggplot(xymelt, aes(x = Day, y = value, color = variable)) +
    theme_bw() +
    geom_line()
}



#' @title  ggVisualizeRkiExtended Visualisation of the extended RKI Data
#'
#' @description ggplot RKI data as result from \code{\link{extendRki}}
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
#' @importFrom reshape2 melt
#'
#'
#' @param data rki data as preprocessed by \code{\link{extendRki}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{"2020-05-01"}
#' @param simplify logical. Simplify presentation, Default: TRUE.
#' @param preloadedData optional way to pass the result of preloading. 
#'
#' @export

ggVisualizeRkiExtended <- function(data = extendRki(),
                                   region = 5374,
                                   StartDate = "2020-10-01",
                                   simplify = TRUE,
                                   preloadedData = NULL) {
  Day <- value <- variable <- NULL
  if(is.null(preloadedData)){
      preloadedData <- ggVisualizeRkiExtendedDataCalculation(
          data, region, StartDate, simplify)
  }
  xymelt <- preloadedData
  
  translator <- golem::get_golem_options('translator')
  if(!is.null(translator)){
      plotTitle <- translator$t("titlePlotInfections")
      plotXlab <- translator$t("Datum")
      plotYlab <- translator$t("infPerDay")
      plotLegendTitle <- translator$t("Legende")
      xymelt$variable <- translator$t(as.character(xymelt$variable))
  }else{
      plotTitle <- "Konfigurierte Infektionszahlen 7-Tage Durchschnitt"
      plotXlab <- "Datum"
      plotYlab <- "Infektionen pro Tag"
      plotLegendTitle <- "Legende"
  }
  
  xymelt.extended <- xymelt[xymelt$Day >= Sys.Date(),]
  xymelt <- xymelt[xymelt$Day <= Sys.Date(),]

  ggplot(xymelt, aes(x = Day, y = value, color = variable)) +
    geom_line() + 
    geom_line(data = xymelt.extended ,aes(x = Day, y = value, color = variable), 
              linetype = "dashed") +
      ggplot2::ggtitle(plotTitle) + 
      ggplot2::xlab(plotXlab) + 
      ggplot2::ylab(plotYlab) + 
      ggplot2::labs(color = plotLegendTitle)
}


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
#' @importFrom reshape2 melt
#'
#' @param data rki data as preprocessed by \code{\link{extendRki}}
#' @param region Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.
#' @param StartDate Start (Tag), e.g., \code{"2020-05-01"}
#' @param simplify logical. Simplify presentation, Default: TRUE.
#'
#' @examples
#' data = getRkiData(babsim.hospital::rkidata[1:10000, ])
#' p <- ggVisualizeRkiExtended(data=extendRki(data),
#'                           region = 5374, StartDate="2020-10-01")
#'
#' @export
ggVisualizeRkiExtendedDataCalculation <- function(data = extendRki(),
                                                  region = 5374,
                                                  StartDate = "2020-10-01",
                                                  simplify = TRUE){
    Day <-
        Freq <-
        Age <- Infected <- InfectedWeekly <- value <- variable <- NULL
    data$time <- NULL
    data$Altersgruppe <- apply(as.matrix(data$Age), 1, FUN = getAgeGroup)
    
    data$Age <- NULL
    data$Altersgruppe <- as.factor(data$Altersgruppe)
    data$Geschlecht <- as.factor(data$Geschlecht)
    region <- as.integer(region)
    ## Landkreis
    if (region > 0 & region > 100) {
        data <- data[data$IdLandkreis == region,]
    }
    ## Bundesland
    if (region > 0 & region < 100) {
        data <- data[data$IdBundesland == region,]
    }
    data$IdLandkreis <- NULL
    data$IdBundesland <- NULL
    data <- data[which(as.Date(data$Day) >= as.Date(StartDate)), ]
    data$AnzahlFall <- rep(1, nrow(data))
    rki <- data
    ## Aggregate
    rki$Refdatum <- as.Date(rki$Day)
    rki$Weiblich <-
        as.integer((rki$Geschlecht == "W") * (rki$AnzahlFall))
    rki$Maennlich <-
        as.integer((rki$Geschlecht == "M") * (rki$AnzahlFall))
    rki$GUnbekannt <-
        as.integer((rki$Geschlecht == "unbekannt") * (rki$AnzahlFall))
    # "A00-A04"   "A05-A14"   "A15-A34"   "A35-A59"   "A60-A79"   "A80+" "unbekannt"
    rki$A00A04 <-
        as.integer((rki$Altersgruppe == "A00-A04") * (rki$AnzahlFall))
    rki$A05A14 <-
        as.integer((rki$Altersgruppe == "A05-A14") * (rki$AnzahlFall))
    rki$A15A34 <-
        as.integer((rki$Altersgruppe == "A15-A34") * (rki$AnzahlFall))
    rki$A35A59 <-
        as.integer((rki$Altersgruppe == "A35-A59") * (rki$AnzahlFall))
    rki$A60A79 <-
        as.integer((rki$Altersgruppe == "A60-A79") * (rki$AnzahlFall))
    rki$A80 <-
        as.integer((rki$Altersgruppe == "A80+") * (rki$AnzahlFall))
    rki$AUnbekannt <-
        as.integer((rki$Altersgruppe == "unbekannt") * (rki$AnzahlFall))
    
    ## Order Data by date
    rki <- rki[order(rki$Refdatum), ]
    
    ## Aggregate
    rkiAgg <- as.data.frame(xtabs(AnzahlFall ~ Refdatum, rki))
    rkiAgg$Weiblich <- as.integer(xtabs(Weiblich ~ Refdatum, rki))
    rkiAgg$Maennlich <- as.integer(xtabs(Maennlich ~ Refdatum, rki))
    rkiAgg$GUnbekannt <-
        as.integer(xtabs(GUnbekannt ~ Refdatum, rki))
    rkiAgg$A00A04 <- as.integer(xtabs(A00A04 ~ Refdatum, rki))
    rkiAgg$A05A14 <- as.integer(xtabs(A05A14 ~ Refdatum, rki))
    rkiAgg$A15A34 <- as.integer(xtabs(A15A34 ~ Refdatum, rki))
    rkiAgg$A35A59 <- as.integer(xtabs(A35A59 ~ Refdatum, rki))
    rkiAgg$A60A79 <- as.integer(xtabs(A60A79 ~ Refdatum, rki))
    rkiAgg$A80 <- as.integer(xtabs(A80 ~ Refdatum, rki))
    rkiAgg$AUnbekannt <-
        as.integer(xtabs(AUnbekannt ~ Refdatum, rki))
    
    rkiAgg$Refdatum <- as.Date(rkiAgg$Refdatum)
    
    colnames(rkiAgg)[which(names(rkiAgg) == "Refdatum")] <- "Day"
    colnames(rkiAgg)[which(names(rkiAgg) == "Freq")] <- "Infected"
    
    if (simplify == TRUE) {
        rkiAgg$AgeYoung <-
            rkiAgg$A00A04 + rkiAgg$A05A14 + rkiAgg$A15A34 + rkiAgg$A35A59
        #rkiAgg$MittelA59 <- rkiAgg$A35A59
        rkiAgg$AgeOld60 <- rkiAgg$A60A79 + rkiAgg$A80
        rkiAgg$A00A04 <-
            rkiAgg$A05A14 <-
            rkiAgg$A15A34 <-
            rkiAgg$A35A59 <- rkiAgg$A60A79 <- rkiAgg$A80 <- NULL
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
        y <- getWeekly(eval(parse (text = var2)))
        data <- data.frame(data,  assign(var1,  y))
        varNames <- c(varNames, var1)
    }
    
    colnames(data) <- c(k , varNames)
    weeklyData <- data[, varNames]
    xy <- cbind(Day = data[, c("Day")], weeklyData)
    xymelt <- reshape2::melt(xy, id.vars = "Day")
    return(xymelt)
}



#' @title  visualizeGraph Visualisierung der Wahrscheinlichkeiten und Dauern
#'
#' @description Ergebnisse
#'
#' @importFrom methods as
#' @importFrom methods new
#' @importFrom igraph V
#' @importFrom igraph V<-
#' @importFrom igraph E
#' @importFrom igraph layout.reingold.tilford
#' @importClassesFrom markovchain markovchain
#' @importFrom igraph plot.igraph
#'
#' @param para parameter
#' @param option Option: plot probabilities ("P") or durations ("D")
#'
#' @examples
#'
#'  para = babsimHospitalPara()
#'  visualizeGraph(para = para , option = "P")
#'
#' @export

visualizeGraph <- function(para = babsimHospitalPara(),
                           option = "P") {
  states <-  c(
    "infec",
    "out",
    "hosp",
    "normal",
    "intens",
    "vent",
    "intafter",
    "aftercare",
    "death",
    "healthy"
  )
  
  para <- checkSimPara(para = para)
  P <- getMatrixP(para = para)
  D <- getMatrixD(para = para)
  M <- new(
    "markovchain",
    transitionMatrix = P,
    states = states,
    name = "bubsim"
  )
  # Do not show edges from S_i to S_i:
  for (i in 1:dim(P)[1]) {
    M@transitionMatrix[i, i] = 0
  }
  g <- as(M, "igraph")
  for (i in states) {
    igraph::V(g)[i]$color <- "white"
  }
  igraph::V(g)[8]$color <- "lightgreen"
  igraph::V(g)[4]$color <- "yellow"
  igraph::V(g)[6]$color <- "yellow"
  igraph::V(g)[5]$color <- "orange"
  igraph::V(g)[9]$color <- "red"
  igraph::V(g)[10]$color <- "lightgreen"
  igraph::V(g)[2]$color <- "coral1"
  igraph::V(g)[3]$color <- "aliceblue"
  igraph::V(g)[7]$color <- "aliceblue"
  
  coords <- igraph::layout.reingold.tilford
  
  if (option == "P") {
    elabs <- round(igraph::E(g)$prob * 100, 1)
  } else{
    elabs <- round(c(
      D[1, 2],
      D[1, 3],
      D[3, 4],
      D[3, 5],
      D[3, 6],
      D[4, 5],
      D[4, 6],
      D[4, 9],
      D[4, 10],
      D[5, 6],
      D[5, 8],
      D[5, 9],
      D[6, 7],
      D[6, 9],
      D[7, 8],
      D[7, 9],
      D[7, 10],
      D[8, 10]
    ),
    1)
  }
  
  translator <- golem::get_golem_options('translator')
  if(!is.null(translator)){
      if (option == "P") {
          TITLE = translator$t("uebmap")
      } else{
          TITLE = translator$t("uebmap2")
      }
  }else{
      if (option == "P") {
          TITLE = "Wahrscheinlichkeiten (Prozent)"
      } else{
          TITLE = "Dauern (Tage)"
      }
  } 
  
  plot.igraph(
    g,
    vertex.color = igraph::V(g)$color,
    vertex.size = 25,
    vertex.label.cex = 1.2,
    edge.arrow.size = 0.5,
    edge.label = elabs,
    edge.label.cex = 1.0,
    edge.curved = 0.28,
    rescale = TRUE,
    layout = coords,
    asp = .9,
    main = TITLE
  )
}



#' @title plotDailyMaxResults
#'
#' @description  Plot output from \code{getDailyMax()}.
#'
#' @param results Results from \code{getDailyMax}.
#' @param labels  Axes labels (vector). Default: \code{c("babsim", "DIVI")}
#' @param title Title. Default:
#' \code{"Betten: Tuerkis = Realdaten, Rot = Simulation"}
#' @param showBeds should normal beds be shown in the plot?
#' @param icuDataRegion regional \code{icudata}
#' 
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 vars
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labeller
#' @importFrom ggplot2 as_labeller
#'
#'
#' @return This function returns a \code{ggplot} object.
#' @examples
#' set.seed(123)
#' # 1. Generate simulation data based on number of infected persons per day:
#' x <- dataCovidBeds20200624
#' StartDate <- x$Day[1]
#' EndDate <- x$Day[length(x$Day)]
#' arrivalTimes <- getArrivalTimes(x$Infected)
#' para = babsimHospitalPara()
#' conf = babsimToolsConf()
#' y <- babsimHospital(arrivalTimes = arrivalTimes,
#'                     conf = conf,
#'                     para = para)
#'
#' # 2. Extract real data:
#' fieldEvents <- getRealBeds(data = babsim.hospital::dataCovidBeds20200624,
#'              resource=c("bed", "intensiveBed", "intensiveBedVentilation"))
#' conf = babsimToolsConf()              
#' # 3. Combine simlated and real data:
#' res <- getDailyMaxResults(envs = y,  
#'                           fieldEvents = fieldEvents,
#'                           conf = conf)
#' # 4. Plot results
#' p <- plotDailyMaxResults(res)
#' # print(p)
#'
#' @export

plotDailyMaxResults <- function(results,
                                labels = c("babsim", "DIVI"),
                                title = "Betten: Tuerkis = Readfssldaten, Rot = Simulation",
                                showBeds = FALSE,
                                icuDataRegion = NULL) {
  ## the following variables are local:
  med <- lower <- upper <- resource <- NULL
  
  translator <- golem::get_golem_options('translator')
  if(!is.null(translator)){
      to_string <-
          as_labeller(
              c(
                  `bed` = translator$t("label1"),
                  `intensiveBed` = translator$t("label2"),
                  `intensiveBedVentilation` = translator$t("label3")
              )
          )
  }else{
      to_string <-
          as_labeller(
              c(
                  `bed` = "Bett",
                  `intensiveBed` = "Nicht beatmet",
                  `intensiveBedVentilation` = "Beatmet"
              )
          )
  }
  if(!showBeds){
      results <- results[results$resource!="bed",]
  }
  
  if(!is.null(translator)){
      plotTitle <- translator$t("titlePlotBeds")
      plotXlab <- translator$t("Datum")
      plotYlab <- translator$t("bedsUsed")
      plotLegendTitle <- translator$t("Legende")
      results$source <- translator$t(as.character(results$source))
  }else{
      plotTitle <- "Bettenauslastung: Gemeldet DIVI und Simulation"
      plotXlab <- "Datum"
      plotYlab <- "Belegte Intensiv Betten"
      plotLegendTitle <- "Legende"
  }
  
  icuDataRegion$bedsTotal <- icuDataRegion$betten_frei + icuDataRegion$betten_belegt
  
  p <- ggplot(results, aes(x = date, y = med, color = source)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4) +
    # facet_wrap(facets = vars(resource)) +
    facet_grid(facets = vars(resource),
               labeller = labeller(resource = to_string), scales="free") +
      ggplot2::xlab(plotXlab) + 
      ggplot2::ylab(plotYlab) + 
      ggplot2::labs(color = plotLegendTitle, title = plotTitle) + 
      ggplot2::ggtitle(plotTitle)
  return(p)
}

#' @title plotPostprocessedEnvs
#'
#' @description  Plot output from \code{\link{postprocessEnvs}}.
#'
#' @param results Results from \code{\link{postprocessEnvs}}.
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 vars
#' @importFrom ggplot2 labs
#'
#'
#' @return This function returns a \code{ggplot} object.
#' @examples
#' set.seed(123)
#' # 1. Generate simulation data based on number of infected persons per day:
#' x <- dataCovidBeds20200624
#' StartDate <- x$Day[1]
#' EndDate <- x$Day[length(x$Day)]
#' arrivalTimes <- getArrivalTimes(x$Infected)
#' para = babsimHospitalPara()
#' conf = babsimToolsConf()
#' y <- babsimHospital(arrivalTimes = arrivalTimes,
#'                     conf = conf,
#'                     para = para)
#'
#' # 2. Postprocess simmer environment:
#' res <- postprocessEnvs(envs = y, StartDate = "2020-03-03")
#' # 4. Plot results
#' p <- plotPostprocessedEnvs(res)
#' # print(p)
#'
#' @export

plotPostprocessedEnvs <- function(results) {
  ## the following variables are local:
  med <- lower <- upper <- resource <- NULL
  p <- ggplot(results, aes(x = date, y = med, color = source)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4) +
    facet_wrap(facets = vars(resource)) +
    labs(x = "Date", y = "Usage")
  return(p)
}
