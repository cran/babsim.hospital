% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDecision.R
\name{getDecision}
\alias{getDecision}
\title{getDecision}
\usage{
getDecision(p)
}
\arguments{
\item{p}{vector of probabilities}
}
\value{
int decision in the range from 0 to n
}
\description{
For given n probabilities 0 <= pi <= 1 with sum(pi)=1,
return 0,1,2,3,..,n with probability p0, p1, p2, ..., pn.
}
\examples{

p <- c(0.6, 0.3, 0.1)
getDecision(p)
}
