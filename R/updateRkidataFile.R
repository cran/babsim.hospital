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
#'     \item{\code{FID}}{int  40613780 40613781 40613782 40613783 40613784 40613785 40613786 40613787 40613788 40613789 ...}
#'     \item{\code{IdBundesland }}{ 1 1 1 1 1 1 1 1 1 1 ...}
#'     \item{\code{Bundesland}}{chr  'Schleswig-Holstein' 'Schleswig-Holstein' 'Schleswig-Holstein' 'Schleswig-Holstein' ...}
#'     \item{\code{Landkreis}}{chr  'SK Flensburg' 'SK Flensburg' 'SK Flensburg' 'SK Flensburg' ...}
#'     \item{\code{Altersgruppe}}{chr  'A00-A04' 'A00-A04' 'A00-A04' 'A05-A14' ...}
#'     \item{\code{Geschlecht}}{chr  'M' 'W' 'W' 'M' ...}
#'     \item{\code{AnzahlFall}}{int  1 1 1 1 1 1 1 1 1 1 ...}
#'     \item{\code{AnzahlTodesfall}}{ ...}
#'     \item{\code{Meldedatum}}{chr  '2020/09/30 00:00:00' '2020/08/24 00:00:00' '2020/09/26 00:00:00' '2020/09/25 00:00:00' ...}
#'     \item{\code{IdLandkreis}}{int  1001 1001 1001 1001 1001 1001 1001 1001 1001 1001 ...}
#'     \item{\code{Datenstand}}{'04.10.2020, 00:00 Uhr' '04.10.2020, 00:00 Uhr' '04.10.2020, 00:00 Uhr' '04.10.2020, 00:00 Uhr' ...}
#'     \item{\code{NeuerFall}}{int  0 0 0 0 0 0 0 0 0 0 ...}
#'     \item{\code{NeuerTodesfall}}{int  -9 -9 -9 -9 -9 -9 -9 -9 -9 -9 ...}
#'     \item{\code{Refdatum}}{chr  '2020/09/30 00:00:00' '2020/08/24 00:00:00' '2020/09/26 00:00:00' '2020/09/21 00:00:00' ...}
#'     \item{\code{NeuGenesen}}{int  -9 0 -9 -9 -9 -9 0 -9 -9 0 ...}
#'     \item{\code{AnzahlGenesen}}{int  0 1 0 0 0 0 1 0 0 1 ...}
#'     \item{\code{IstErkrankungsbeginn}}{int  0 0 0 1 1 0 0 1 0 1 ...}
#'     \item{\code{Altergruppe2}}{chr  'Nicht übermittelt' 'Nicht übermittelt' 'Nicht übermittelt' 'Nicht übermittelt' ...}
#'     }
#'
#' @param fileName RKI Filename, e.g.,
#' \code{'https://www.arcgis.com/sharing/rest/content/}
#' \code{items/f10774f1c63e40168479a1feb6c7ca74/data'}
#' @param overwrite logical Overwrite existing file. Default \code{TRUE}.
#'
#' @importFrom utils read.csv
#'
#' @return True if new data was downloaded, otherwise false
#'
#' @export
updateRkidataFile <- function(fileName = NULL,
                              overwrite = TRUE){
    # Testing:
    # fileName = "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
    # overwrite = TRUE
    if(is.null(fileName)){
      fileName <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
    }
    load("babsim.hospital/data/rkidataFull.rda")
    rkiOld <- rkidataFull
    rkidataFull <- read.csv(url(fileName),
                            header=TRUE,
                            encoding="UTF-8",
                            stringsAsFactors = F)
    if(isTRUE(all.equal(rkiOld,rkidataFull)[1])){
        ## Download succeeded but there is no new data
        return(FALSE)
    }
    
    save(rkidataFull, file = "babsim.hospital/data/rkidataFull.rda", compress="xz")
    
    rkidata <- rkidataFull[which(rkidataFull$Meldedatum >= as.Date("2020-09-01")), ]
    
    names(rkidata) <- c("FID", "IdBundesland",   "Bundesland",     "Landkreis",
                        "Altersgruppe",   "Geschlecht",     "AnzahlFall",     
                        "AnzahlTodesfall", "Refdatum",     "IdLandkreis",  
                        "Datenstand",     "NeuerFall",      "NeuerTodesfall",
                        "Meldedatum",       "NeuGenesen",     "AnzahlGenesen",
                        "IstErkrankungsbeginn", "Altersgruppe2")
    save(rkidata, file = "babsim.hospital/data/rkidata.rda", compress="xz")
    return(TRUE)
}
