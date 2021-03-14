
###################################################################################
#' @title rkidata: RKI COVID-19 Daten (complete, not included in CRAN version)
#' 
#' @description RKI Daten komplett
#' 
#' @details Heruntergeladen  mittels 
#' \code{wget https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data}
#' Eingelesen mittels \code{rkidata <- read.csv("data", header=TRUE,encoding="UTF-8")}
#' Eingepflegt mittel \code{usethis::use_data(rkidata)}
#' 
#' 
#' Weiterverabeitung z.B. mit:
#' \code{rkidataobk <- rkidata[rkidata$Landkreis == "LK Oberbergischer Kreis", ]}
#' \code{rkidataobk$Meldedatum <- as.Date(rkidataobk$Meldedatum) }
#' \code{sum(rkidataobk$AnzahlFall)}
#' \code{sum(rkidataobk$AnzahlTodesfall)}
#' \code{rkidataobkAgg <- as.data.frame(xtabs( AnzahlFall   ~ Meldedatum, rkidataobk))}
#' \code{plot(rkidataobkAgg$Meldedatum, rkidataobkAgg$Freq)}
#'
#' Ein data.frame  mit 18 Variablen
#'
#' @format a data.frame of of  18 variables
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
###################################################################################
"rkidata"
