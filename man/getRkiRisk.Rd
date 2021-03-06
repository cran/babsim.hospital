% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRkiRisk.R
\name{getRkiRisk}
\alias{getRkiRisk}
\title{getRkiRisk}
\usage{
getRkiRisk(rki, para)
}
\arguments{
\item{rki}{data.frame of downloaded rki data}

\item{para}{parameter}
}
\value{
a data.frame
}
\description{
Calculate risk for RKI data
}
\examples{

rki <- getRkiData(rkidata[1:10, ])
para <- babsimHospitalPara()
# get risk for the first 10 entries:
rkiWithRisk <- getRkiRisk(rki = rki, para)
}
