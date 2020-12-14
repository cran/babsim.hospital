###################################################################################
#' @title nrwpara data
#' 
#' @description  
#' Data: Koeln data generated with SPOT. 
#' 
#' @details 
#' Result from the \code{\link{runoptDirect}} run.
#' Use \code{yx <- nrwpara[nrwpara$y == min(nrwpara$y), ]
#' x <- yx[1,2:34]
#' } 
#' to extract the best parameter set x.
#' 
#' @format 'data.frame':	obs. of  34 variables:
#' \describe{
#'   \item{y}{num  311 158 180 232 297 ...}
#'   \item{x.1}{num  10.55 17.91 3.19 9.5 17.71 ...}
#'   \item{...}{ ...}
#'   \item{x.33}{num  1.072 1.015 1.044 1.057 0.556 ...}
#' }
#'
###################################################################################
"nrwpara"
