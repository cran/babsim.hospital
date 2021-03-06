% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/messagef.R
\name{messagef}
\alias{messagef}
\title{Output a formatted message}
\usage{
messagef(fmt, ...)
}
\arguments{
\item{fmt}{format string (see \code{\link{sprintf}} for details)}

\item{...}{values passed into \code{fmt}.
Only logical, integer, real and character vectors are supported, but some coercion will be done.}
}
\value{
Nothing, called for the side effect of outputting a message to the console.
}
\description{
Output a formatted message
}
