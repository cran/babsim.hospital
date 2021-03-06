% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rkiToBabsimArrivals.R
\name{rkiToBabsimArrivals}
\alias{rkiToBabsimArrivals}
\title{rkiToBabsimArrivals}
\usage{
rkiToBabsimArrivals(rki)
}
\arguments{
\item{rki}{data.frame of downloaded rki data before preprocessing}
}
\value{
a data.frame of arrival times suited for babsimHospital
}
\description{
Transforms the freshly downloaded rki data into the babsim fitting format of
arrival times. Also imputes mising dates as zero frequency in the data.
}
\examples{

arrivals <- rkiToBabsimArrivals(rkidata[1:100, ])
min(as.Date(rkidata$Refdatum)) + max(arrivals)
}
