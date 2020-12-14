

#' @title getRealAge
#' 
#' @description Calculate real valued age based on RKI age classes
#' 
#' @param x age vector
#' 
#' @return age as a real value
#' @export

getRealAge <- function(x){
  if (x == "A00-A04") {
    rA = 2
  }  else if (x == "A05-A14") {
    rA = 10
  }  else if (x == "A15-A34") {
    rA = 25
  }  else if (x == "A35-A59") {
    rA = 47
  } else if (x == "A60-A79") {
    rA = 70
  } else if (x == "A80+") {
    rA = 90
  }  else {
    rA = 47
  }
  return(rA)
}



#' @title getAgeGroup
#' 
#' @description Calculate age based on RKI age classes
#' 
#' @param x age vector
#' 
#' @return age class
#' @export

getAgeGroup <- function(x){
  if (x <= 4) {
    rA = "A00-A04"
  }  else if (x <= 14) {
    rA = "A05-A14"
  }  else if (x <= 34) {
    rA = "A15-A34"
  }  else if (x <= 59) {
    rA = "A35-A59"
  } else if (x <= 79) {
    rA = "A60-A79"
  } else if (x >= 80) {
    rA = "A80+"
  }  else {
    rA = "AUnbekannt"
  }
  return(rA)
}





#' @title getRkiData 
#' 
#' @description Transforms the freshly downloaded rki data into the babsim data frame.
#'  Also imputes mising dates as zero frequency in the data.
#' 
#' @param rki data.frame of downloaded rki data before preprocessing
#' 
#' @return a data.frame of aggregated data
#'  \describe{
#'		\item{\code{Day}}{Date, format: "2020-01-01" "2020-01-02" "2020-01-03" "2020-01-04" ...}
#'		\item{\code{...}}{ ...}
#'		}
#' 
#' @importFrom data.table data.table
#' @importFrom data.table fsetdiff
#' 
#' @examples 
#' 
#' data <- getRkiData(rkidata[1:100,])
#' 
#' @export

getRkiData <- function(rki){
  rki$Refdatum <- as.Date(rki$Refdatum)
  rki$Weiblich <- as.integer(rki$Geschlecht == "W")
  rki$Maennlich <- as.integer(rki$Geschlecht == "M")
  rki$Unbekannt <- as.integer(rki$Geschlecht == "unbekannt")
  
  ### 
  ### Correct the negative cases:
  ### 
  ## 
  ## Create a data table of only the negative cases
  if(sum(rki$AnzahlFall<0)){
      negativeOnes <- rki[rki$AnzahlFall<0,]
      nTimes <- as.integer(-negativeOnes$AnzahlFall)
      negativeOnes <- negativeOnes[rep (seq_len (nrow (negativeOnes)), nTimes),] 
      negativeOnes$AnzahlFall <- 1
      negativeOnes <- data.table(data.frame(Altersgruppe = negativeOnes$Altersgruppe,
                                            Geschlecht = negativeOnes$Geschlecht,
                                            Day = negativeOnes$Refdatum,
                                            IdBundesland = as.integer(negativeOnes$IdBundesland),
                                            IdLandkreis = as.integer(negativeOnes$IdLandkreis)))
      ## 
      ## Create a data table of only the positive cases
      positiveOnes <- rki[rki$AnzahlFall>0,]
      nTimes <- as.integer(positiveOnes$AnzahlFall)
      positiveOnes <- positiveOnes[rep (seq_len (nrow (positiveOnes)), nTimes),] 
      positiveOnes$AnzahlFall <- 1
      positiveOnes <- data.table(data.frame(Altersgruppe = positiveOnes$Altersgruppe,
                                            Geschlecht = positiveOnes$Geschlecht,
                                            Day = positiveOnes$Refdatum,
                                            IdBundesland = as.integer(positiveOnes$IdBundesland),
                                            IdLandkreis = as.integer(positiveOnes$IdLandkreis)))
      
      ## 
      ## fsetdiff removes those rows of positiveOnes where there is a match in negativeOnes
      ## It is similar to anti_join but removes one to one, this keeping duplicates which is important here
      rki <- data.frame(fsetdiff(data.table::data.table(positiveOnes), data.table::data.table(negativeOnes), all = TRUE))
  }else{
      ## This will remove the entries with cases == 0, those dates are later filled again
      rki <- rki[rki$AnzahlFall>0,]
      nTimes <- as.integer(rki$AnzahlFall)
      rki <- rki[rep (seq_len (nrow (rki)), nTimes),] 
      
      ## remove unnecessary columns
      rki <- data.frame(Altersgruppe = rki$Altersgruppe,
                                            Geschlecht = rki$Geschlecht,
                                            Day = rki$Refdatum,
                                            IdBundesland = as.integer(rki$IdBundesland),
                                            IdLandkreis = as.integer(rki$IdLandkreis))
  }
  
  ## v10.2.24: Nov, 15th 2020: Perform data correction after the one case per row only correction:
  ## 2. Date Correction - Days without any cases have to be filled
  ## data_range is the expected number of days:
  date_range <- seq(min(rki$Day), max(rki$Day), by = 1) 
  miss <- length(date_range[!date_range %in% rki$Day])
  if (miss >0){
    zeroDates <- data.frame("Altersgruppe" = rep("-1", miss),
                            "Geschlecht" = rep("-1", miss),
                            "Day" = date_range[!date_range %in% rki$Day],
                            IdBundesland = rep( -1, miss),
                            IdLandkreis = rep( -1, miss)
                            )
  }
  
  if (miss >0){
    rki <- rbind(rki, zeroDates)
  }
  ## Order Data by date
  # rki <- rki[order(rki$Refdatum),]
  rki <- rki[order(rki$Day),]
  
  rki$time <- as.numeric(rki$Day) - as.numeric(min(rki$Day))
  ## 3. Remove zero entries after time was set:
  rki <- rki[!(rki$IdBundesland == -1),]
  
  #arrivals$AgeRisk <- apply(as.matrix( data$Altersgruppe),1,FUN=getRealAge)
  #rki$IdBundesland <- as.integer(rki$IdBundesland)
  #rki$IdLandkreis <- as.integer(rki$IdLandkreis)
  rki$Age <- apply(as.matrix( rki$Altersgruppe),1,FUN=getRealAge)
  return(rki)
}




#' @title getRkiRisk 
#' 
#' @description Calculate risk for RKI data
#' @param rki data.frame of downloaded rki data 
#' @param para parameter
#' 
#' @return a data.frame 
#' 
#' @examples 
#' 
#' rki <- getRkiData(rkidata[1:10,])
#' para <- babsimHospitalPara()
#' # get risk for the first 10 entries:
#' rkiWithRisk <- getRkiRisk(rki= rki, para)
#' 
#' @export

getRkiRisk <- function(rki, para){
    ## getRealValuedAge:
    a <- para$RiskFactorA
    b <- para$RiskFactorB
    expFun <- function(x) a*exp(b*x)
    rki$Risk <- apply(as.matrix( rki$Age),1,FUN=expFun)
    rki$Risk <- rki$Risk * (1 + (para$RiskMale - 1) * as.integer(rki$Geschlecht == "M") )
  return(rki)
}




#' @title rkiToBabsimData 
#' 
#' @description Transforms the freshly downloaded rki data into the babsim data frame.
#'  Also imputes mising dates as zero frequency in the data.
#' 
#' @param rki data.frame of downloaded rki data before preprocessing
#' 
#' @return a data.frame of aggregated data
#'  \describe{
#'		\item{\code{Day}}{Date, format: "2020-01-01" "2020-01-02" "2020-01-03" "2020-01-04" ...}
#'		\item{\code{Infected}}{Infiziert: num  1 0 0 0 0 0 0 0 0 0 ...}
#'		\item{\code{Weiblich}}{Geschlecht weiblich: int  0 0 0 0 0 0 0 0 0 0 ...}
#'		\item{\code{Maennlich}}{Geschlecht maennlich: int  1 0 0 0 0 0 0 0 0 0 ...}
#'		\item{\code{GUnbekannt}}{Geschlecht unbekannt: int  0 0 0 0 0 0 0 0 0 0 ...}
#'		}
#' 
#' @examples 
#'  
#' data <- rkiToBabsimData(rkidata[1:100, ])
#' plot(data$Day, data$Infected, type = "o")
#' #
#' max(data$Day) - min(data$Day)
#' 
#' 
#' @export

rkiToBabsimData <- function(rki=babsim.hospital::rkidata){
  rki <- data.table::data.table(rki)
  rki$Day <- as.Date(rki$Refdatum)

  ## Normalize levels
  rki$Geschlecht <- factor(rki$Geschlecht, 
                           levels=c("M", "W", "unbekannt"),
                           labels=c("Maennlich", "Weiblich", "GUnbekannt"))
  rki$Altersgruppe <- factor(rki$Altersgruppe,
                             levels=c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+", "unbekannt"),
                             labels=c("A00A04", "A05A14", "A15A34", "A35A59", "A60A79", "A80", "AUnbekannt")) 

  res <- data.table(Day=seq(min(rki$Day), max(rki$Day), by=1))

  sex <- data.table::dcast(rki, Day ~ Geschlecht,
                           value.var="AnzahlFall",
                           fun.aggregate=sum)
  res <- merge(res, sex, by="Day", all.x=TRUE)

  age <- data.table::dcast(rki, Day ~ Altersgruppe, 
                           value.var="AnzahlFall",
                           fun.aggregate=sum)
  res <- merge(res, age, by="Day", all.x=TRUE)

  total <- data.table::dcast(rki, Day ~ .,
                             value.var="AnzahlFall",
                             fun.aggregate=sum)
  colnames(total) <- c("Day", "Infected")
  res <- merge(res, total, by="Day", all.x=TRUE)

  res <- data.table::setnafill(res, fill=0)
  as.data.frame(res)
}


#' @title rkiToBabsimArrivals 
#' 
#' @description Transforms the freshly downloaded rki data into the babsim fitting format of 
#' arrival times. Also imputes mising dates as zero frequency in the data.
#' 
#' @param rki data.frame of downloaded rki data before preprocessing
#' 
#' @return a data.frame of arrival times suited for babsimHospital
#' 
#' @examples 
#' 
#' arrivals <- rkiToBabsimArrivals(rkidata[1:100, ])
#' min(as.Date(rkidata$Refdatum)) + max(arrivals)
#' 
#' @export

rkiToBabsimArrivals <- function(rki){
  rkiAgg <- rkiToBabsimData(rki)
  return(data.frame(time = getArrivalTimes(rkiAgg$Infected)))
}




#' @title  extendRki Erweiterung der RKI Daten 
#' 
#' @description Combine existing data with synthetic data  
#' 
#' @seealso \code{\link{getRkiData}} 
#' 
#' @importFrom stats xtabs
#' @importFrom slider slide_dbl
#' @importFrom graphics lines
#' @importFrom stats rbinom
#' @importFrom utils tail
#' 
#' @param data rki data, e.g., \code{getRkiData(babsim.hospital::rkidata)}
#' @param EndDate Ende (Tag), e.g., \code{"2020-05-04"}
#' @param R0 Basisreproduktionszahl. Constant, if a scalar value is given.
#' If a vector of two values are given, they will be interpreted as 
#' a start and an end value, respectively. \code{c(1,2)} defines an increasing
#' \code{R0} value from \code{1} to \code{2}. Default: \code{1}, i.e., constant \code{1}.
#' Note: This is NOT exactly the same R0 value presented by the Robert-Koch Institute,
#' please refer to \url{https://en.wikipedia.org/wiki/Basic_reproduction_number}
#' for our implementation.
#' @param tau Ansteckungszeitraum in Tagen
#' 
#' 
#' @examples 
#' # take 10,000 data points only:
#' data <- getRkiData(babsim.hospital::rkidata[1:10000,])
#' n <-  as.integer( max(data$Day)-min(data$Day) )
#' StartDay <- min(data$Day) + round(n*0.995)  
#' data <- data[which(data$Day >=  StartDay), ]
#' EndDate <- max(data$Day) + 2
#' dataExt <- extendRki(data = data, 
#'                      EndDate = EndDate,
#'                      R0 = c(0.1, 0.2))
#' 
#' @export
extendRki <- function(data = getRkiData(babsim.hospital::rkidata),
                      EndDate = max(data$Day) + 14,
                      R0 = c(1.0, 1.0),
                      tau = 5) {
  data$dummy <- rep(1,dim(data)[1])
  dataAgg <- as.data.frame(xtabs( dummy ~  Day, data)) 
  dataAgg$Day <- as.Date(dataAgg$Day)
  #dataAgg$Refdatum <- NULL
  data$dummy <- NULL
  #data$Day <- as.Date(data$Refdatum)
  #data$Refdatum <- NULL
  StartDate <- max(data$Day)+1
  EndDate <- as.Date(EndDate)
  # synthetische Daten
  # Faelle <- getInfectedPerDay(lambda= lambda,
  #                            StartDate = StartDate,
  #                            EndDate = EndDate)
  ## Exponentielles Wachstum
  ## https://en.wikipedia.org/wiki/Basic_reproduction_number
  # tau <- 5
  # R0 <- 1.1
  # n0 <- dataAgg[ dataAgg$Day == max(dataAgg$Day),]$Freq
  ## 2020-11-01: new starting value for the number of cases:
  n0 <- median( tail(dataAgg, 7)$Freq)
  x <- 1:(EndDate-StartDate+1)
  # R0 nicht als Skalar, sondern vektoriell => Dynamik
  if(length(R0) > 1 ){
    R0 <- seq(from = R0[1],
              to = R0[2],
              length.out = length(x))
  }
  K <- log(R0)/tau
  Faelle <- round(n0 * exp(K*x))
  DaySeq <- seq(StartDate, EndDate, by = 1) 
  time <- rep(0, Faelle[1])
  Day <- rep(DaySeq[1], Faelle[1])
  k <- length(Faelle) -1
  for(i in 1:k) {
    time <- c(time, rep(i, Faelle[i+1]))
    Day <- c(Day, rep(DaySeq[i+1], Faelle[i+1]))
  }
  time <- time + max(data$time) +1
  n <- sum(Faelle)
 
  # Geschlecht gleichverteilt auswürfeln.
  Geschlecht <- sample(c("W", "M"), n, replace=TRUE, prob=c(0.5, 0.5))

  # Wir brauchen für die "neuen" Fälle ein Bundesland und Landkreis.  Dafür
  # ziehen wir zufällig Orte aus den bestehenden Daten. Das ist nicht
  # optimal...
  location <- sample(1:nrow(data), size=n, replace=TRUE)

  # Alter entsprechend der Verteilung in der Historie auswürfeln.
  # Wir gewichten "junge" Fälle, d.h. die nahe Vergangenheit, höher um 
  # die sich wandelnde Altersverteilung zu berücksichtigen.
  #
  # Verbesserung wäre ggf. noch pro Bundesland zu ziehen um die (leicht) 
  # unterschiedliche Altersverteilung je Bundesland zu berücksichtigen.
  ageProb <- exp(as.numeric(data$Day - max(data$Day))/14)
  Age <- sample(data$Age, n, replace=TRUE, prob=ageProb)

  extData <- data.frame(Altersgruppe = rep("A-", n),
                        Geschlecht = Geschlecht,
                        Day = Day,
                        IdBundesland = data$IdBundesland[location],
                        IdLandkreis = data$IdLandkreis[location],
                        time = time,
                        Age = Age
                        )
  rbind(data, extData)
}




#' @title  getRegionRki Auswahl der RKI Daten fuer eine Region
#' 
#' @description Auswahl anhand der Bundeslaender, Landkreis IDs 
#' 
#' @param data Daten, z.B. rkidata
#' @param region Id der Region, \code{0} Deutschland
#' 
#' @examples 
#' data <- getRegionRki(data = babsim.hospital::rkidata[1:1000,], 
#'                             region = 0)
#' 
#' @export

getRegionRki <- function(data = babsim.hospital::rkidata,
                      region) {
  region <- as.integer(region)
      if (region > 0 & region < 100) {
      return (data[data$IdBundesland == region,])
    } else if (region >= 100) {
      return(data[data$IdLandkreis == region,])
    } else {
      return(data)
    }
}


#' @title  switchRkiRefdatumToMeldedatum Change RKI data File
#' 
#' @description Use Meldedatum instead of Refdatum in 
#' \code{\link{rkidata}}. It is recommended to run
#' install and restart the package afterwards.
#' 
#' @param rkidata RKI Data
#' @param overwrite logical Overwrite existing file. Default \code{TRUE}.
#' 
#' @export

switchRkiRefdatumToMeldedatum <- function(rkidata = rkidata,
                                          overwrite = TRUE){
  names(rkidata) <- c("FID", "IdBundesland",   "Bundesland",     "Landkreis",
                           "Altersgruppe",   "Geschlecht",     "AnzahlFall",     
                           "AnzahlTodesfall", "Refdatum",     "IdLandkreis",  
                           "Datenstand",     "NeuerFall",      "NeuerTodesfall",
                           "Meldedatum",       "NeuGenesen",     "AnzahlGenesen",
                           "IstErkrankungsbeginn", "Altersgruppe2")
  usethis::use_data(name = rkidata, overwrite = overwrite)
}

#' @title  switchRkiMeldedatumToRefdatum Change RKI data File
#' 
#' @description Use Refdatum instead of Meldedatum in
#' \code{\link{rkidata}}. This is the original configuration provides by RKI.
#' It is recommended to run
#' install and restart the package afterwards.
#' 
#' @param data RKI Data
#' @param overwrite logical Overwrite existing file. Default \code{TRUE}.
#' 
#' @export

switchRkiMeldedatumToRefdatum <- function(data = rkidata,
                                          overwrite = TRUE){
  rkidata <- rkidata
  names(rkidata) <- c("FID", "IdBundesland",   "Bundesland",     "Landkreis",
                         "Altersgruppe",   "Geschlecht",     "AnzahlFall",     
                         "AnzahlTodesfall", "Meldedatum",     "IdLandkreis",  
                         "Datenstand",     "NeuerFall",      "NeuerTodesfall",
                         "Refdatum",       "NeuGenesen",     "AnzahlGenesen",
                         "IstErkrankungsbeginn", "Altersgruppe2")
  usethis::use_data(name = rkidata, overwrite = overwrite)
  return(rkidata)
}


#' @title  updateRkidataFile Update RKI data File
#' 
#' @description Update rkidata. Download RKI data from the RKI Server.
#' 
#' @details  Because the downloaded RKI data include data from January 2020
#' (first COVID-19 wave in Germany) until
#' today (or the day before today), the original data are stored as \code{rkidataFull}.
#' \code{babsim.hospital} simulations require data starting from September 2020 (second 
#' COVID-19 wave in Germany). Therefore, a limited data set starting from Sep 1st
#' is stored as \code{\link{rkidata}} in the \code{babsim.hospital} package.
#' Furthermore, because no nowcasting is implemented in the current version of the 
#' \code{babsim.hospital} simulator, the \code{Meldedatum} information will be used 
#' instead of the the \code{Refdatum} information: the corresponding columns in 
#' \code{rkidata} are renamed, because all functions in \code{babsim.hospital}
#' use the \code{Refdatum} information.
#' 
#' Creates two \code{*.rda} files in the folder \code{data}: 
#' 1. \code{babsim.hospital::rkidata} and
#' 2. \code{babsim.hospital::rkidataFull}.
#' Both data frames contain the following 18 variables
#'  \describe{
#'		\item{\code{FID}}{int  40613780 40613781 40613782 40613783 40613784 40613785 40613786 40613787 40613788 40613789 ...}
#'		\item{\code{IdBundesland }}{ 1 1 1 1 1 1 1 1 1 1 ...}
#'		\item{\code{Bundesland}}{chr  "Schleswig-Holstein" "Schleswig-Holstein" "Schleswig-Holstein" "Schleswig-Holstein" ...}
#'		\item{\code{Landkreis}}{chr  "SK Flensburg" "SK Flensburg" "SK Flensburg" "SK Flensburg" ...}
#'		\item{\code{Altersgruppe}}{chr  "A00-A04" "A00-A04" "A00-A04" "A05-A14" ...}
#'		\item{\code{Geschlecht}}{chr  "M" "W" "W" "M" ...}
#'		\item{\code{AnzahlFall}}{int  1 1 1 1 1 1 1 1 1 1 ...}
#'		\item{\code{AnzahlTodesfall}}{ ...}
#'		\item{\code{Meldedatum}}{chr  "2020/09/30 00:00:00" "2020/08/24 00:00:00" "2020/09/26 00:00:00" "2020/09/25 00:00:00" ...}
#'		\item{\code{IdLandkreis}}{int  1001 1001 1001 1001 1001 1001 1001 1001 1001 1001 ...}
#'		\item{\code{Datenstand}}{"04.10.2020, 00:00 Uhr" "04.10.2020, 00:00 Uhr" "04.10.2020, 00:00 Uhr" "04.10.2020, 00:00 Uhr" ...}
#'		\item{\code{NeuerFall}}{int  0 0 0 0 0 0 0 0 0 0 ...}
#'		\item{\code{NeuerTodesfall}}{int  -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 ...}
#'		\item{\code{Refdatum}}{chr  "2020/09/30 00:00:00" "2020/08/24 00:00:00" "2020/09/26 00:00:00" "2020/09/21 00:00:00" ...}
#'		\item{\code{NeuGenesen}}{int  -9 0 -9 -9 -9 -9 0 -9 -9 0 ...}
#'		\item{\code{AnzahlGenesen}}{int  0 1 0 0 0 0 1 0 0 1 ...}
#'		\item{\code{IstErkrankungsbeginn}}{int  0 0 0 1 1 0 0 1 0 1 ...}
#'		\item{\code{Altergruppe2}}{chr  "Nicht übermittelt" "Nicht übermittelt" "Nicht übermittelt" "Nicht übermittelt" ...}
#'		}
#' 
#' @param fileName RKI Filename, e.g., 
#' \code{"https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"}
#' @param overwrite logical Overwrite existing file. Default \code{TRUE}.
#' 
#' @importFrom utils read.csv
#' 
#' @return True if new data was downloaded, otherwise false
#' 
#' @export
updateRkidataFile <- function(fileName,
                              overwrite = TRUE){
  # Testing:
  # fileName = "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
  # overwrite = TRUE
  if (!exists("fileName")) { 
    fileName <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
  }
  rkiOld <- rkidataFull
  rkidataFull <- read.csv(url(fileName),
                      header=TRUE,
                      encoding="UTF-8",
                      stringsAsFactors = FALSE)
  if(all.equal(rkiOld,rkidataFull)){
      ## Download succeeded but there is no new data
      return(FALSE)
  }
  
  usethis::use_data(name = rkidataFull, overwrite = overwrite)
  
  rkidata <- rkidataFull[which(rkidataFull$Meldedatum >= as.Date("2020-09-01")), ]
  
  names(rkidata) <- c("FID", "IdBundesland",   "Bundesland",     "Landkreis",
                      "Altersgruppe",   "Geschlecht",     "AnzahlFall",     
                      "AnzahlTodesfall", "Refdatum",     "IdLandkreis",  
                      "Datenstand",     "NeuerFall",      "NeuerTodesfall",
                      "Meldedatum",       "NeuGenesen",     "AnzahlGenesen",
                      "IstErkrankungsbeginn", "Altersgruppe2")
  usethis::use_data(name = rkidata, overwrite = overwrite)
  return(TRUE)
}
