% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/softmax.R
\name{softmax}
\alias{softmax}
\title{softmax}
\usage{
softmax(par)
}
\arguments{
\item{par}{vector}
}
\value{
num vector with components >= 0 and sum = 1
}
\description{
softmax function
}
\examples{

p <- c(0.6, 0.3, 0.1)
softmax(p)
}
