% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getStartParameter.R
\name{getStartParameter}
\alias{getStartParameter}
\title{getStartParameter}
\usage{
getStartParameter(para = babsimHospitalPara(), region = -1)
}
\arguments{
\item{para}{parameter vector, e.g., generated via
\code{\link{babsimHospitalPara}}. Default: \code{\link{babsimHospitalPara}}}

\item{region}{(int) use region specific start parameter, e.g., \code{5374} for OBK.
If \code{region} is negative (default), a generic start parameter is chosen.
The selection is based on the obkpara, koelnpara, and nrwpara parameter values,
which are the best known values found so far.}
}
\value{
This function returns a (1,n) dim matrix
}
\description{
Returns parameter for babsim runs
}
\examples{
para <- getStartParameter(region = 5374)
}
