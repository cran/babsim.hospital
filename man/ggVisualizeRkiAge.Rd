% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggVisualizeRkiAge.R
\name{ggVisualizeRkiAge}
\alias{ggVisualizeRkiAge}
\title{ggVisualizeRkiAge Visualisation of the pre-processed RKI Data with
respect to age and gender}
\usage{
ggVisualizeRkiAge(
  data = babsim.hospital::rkidata,
  region = 5374,
  StartDate = "2020-10-01",
  simplify = TRUE
)
}
\arguments{
\item{data}{rki data as preprocessed by \code{\link{rkiToBabsimData}}}

\item{region}{Landkreis Id, e.g., \code{5374} oder Bundesland ID, e.g., \code{5}.}

\item{StartDate}{Start (Tag), e.g., \code{'2020-05-01'}}

\item{simplify}{logical. Simplify presentation, Default: TRUE.}
}
\description{
ggplot RKI data as result from \code{getRkiData(babsim.hospital::rkidata)}
}
\seealso{
\code{\link{rkiToBabsimData}}
}
