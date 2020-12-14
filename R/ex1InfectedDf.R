
###################################################################################
#' @title ex1InfectedDf  
#' 
#' @description 
#' Data used in example 1 and for testing
#' A synthetic data set of COVID-19 cases with 99 obs. of  8 variables:
#'
#' @format A data frame with 99 obs. of  8 variables:
#' \describe{
#'   \item{index}{int  1 2 3 4 5 6 7 8 9 10 ...}
#'   \item{Day}{Date, format: "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" ...}
#'   \item{Infected}{num  6 0 1 3 3 1 5 1 6 104 ...}
#'   \item{Sick}{num  6 6 7 10 13 14 19 20 26 130 ...}
#'   \item{InfectedCum}{num  6 6 7 10 13 14 19 20 26 130 ...}
#'   \item{normalStation}{num  1 1 1 1 2 2 3 3 4 18 ...}
#'   \item{intensive}{num  0 0 0 0 0 0 0 0 0 1 ...}
#'   \item{ventilation}{num  0 0 0 0 0 0 0 0 0 2 ...}
#' }
#' 
#' @examples
#' x <- ex1InfectedDf
#' # first look
#' str(x)
#' 
#' # plot  
#' x$InfCum <- cumsum(x$Infected)
#' plot(x$Day, x$InfCum, type="l", log="y", ylim=c(1,500))
#' lines(x$Day, x$Infected + 1e-6)
#'  
#'    
###################################################################################
"ex1InfectedDf"
