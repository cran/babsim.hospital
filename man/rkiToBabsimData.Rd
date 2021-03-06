% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rkiToBabsimData.R
\name{rkiToBabsimData}
\alias{rkiToBabsimData}
\title{rkiToBabsimData}
\usage{
rkiToBabsimData(rki = babsim.hospital::rkidata)
}
\arguments{
\item{rki}{data.frame of downloaded rki data before preprocessing}
}
\value{
a data.frame of aggregated data
 \describe{
    \item{\code{Day}}{Date, format: '2020-01-01' '2020-01-02' '2020-01-03' '2020-01-04' ...}
    \item{\code{Infected}}{Infiziert: num  1 0 0 0 0 0 0 0 0 0 ...}
    \item{\code{Weiblich}}{Geschlecht weiblich: int  0 0 0 0 0 0 0 0 0 0 ...}
    \item{\code{Maennlich}}{Geschlecht maennlich: int  1 0 0 0 0 0 0 0 0 0 ...}
    \item{\code{GUnbekannt}}{Geschlecht unbekannt: int  0 0 0 0 0 0 0 0 0 0 ...}
    }
}
\description{
Transforms the freshly downloaded rki data into the babsim data frame.
 Also imputes mising dates as zero frequency in the data.
}
\examples{

data <- rkiToBabsimData(rkidata[1:100, ])
plot(data$Day, data$Infected, type = "o")
#
max(data$Day) - min(data$Day)
}
