###################################################################################
#' @title ukpara02 data
#' 
#' @description  
#' Data: ukpara para generated with SPOT. Probabilities for ICU ventilation reduced 
#' and durations for ICU stays increased.
#' 
#' @details 
#' Result from the \code{runoptUK} run.
#' Use \code{yx <- ukpara[ukpara$y == min(ukpara$y), ]
#' x <- yx[1,2:dim(ukpara)[2]]
#' } 
#' to extract the best parameter set x.
#' 
#' @format 'data.frame':	 obs. of  29 variables:
#' \describe{
#'   \item{y}{num  311 158 180 232 297 ...}
#'   \item{x.1}{num  10.55 17.91 3.19 9.5 17.71 ...}
#'   \item{...}{ ...}
#'   \item{x.29}{num  1.072 1.015 1.044 1.057 0.556 ...}
#' }
#'
###################################################################################
"ukpara02"