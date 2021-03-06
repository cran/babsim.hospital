% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggVisualizeRkiEvents.R
\name{ggVisualizeRkiEvents}
\alias{ggVisualizeRkiEvents}
\title{ggVisualizeRkiEvents Visualisation of the pre-processed RKI Data}
\usage{
ggVisualizeRkiEvents(
  data = getRkiData(babsim.hospital::rkidata),
  region = 5374,
  StartDate = "2020-10-01"
)
}
\arguments{
\item{data}{rki data as preprocessed by \code{\link{getRkiData}}}

\item{region}{Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.}

\item{StartDate}{Start (Tag), e.g., \code{'2020-05-01'}}
}
\description{
ggplot RKI data as result from \code{getRkiData(babsim.hospital::rkidata)}
}
\examples{
# use 10000 data points only:
data <- getRkiData(babsim.hospital::rkidata[1:10000, ])
p <- ggVisualizeRkiEvents(data = data, region = 0, StartDate = "2020-10-01")
}
\seealso{
\code{\link{getRkiData}}
}
