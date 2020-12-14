#' 
#' @title getBounds
#' 
#' @description Returns parameter bounds for babsim runs 
#' (settings version > v10.4.8)
#' 
#' @return This function returns a list of two vectors
#' @examples
#' bounds <- getBounds()
#' lower <- bounds$lower
#' upper <- bounds$upper
#' 
#'  
#' @export
getBounds <- function(){
  n <- length(getStartParameter())
  a <- rep(0, n)
  a[1] = 6 
  a[2] = 7 
  a[3] = 3 
  a[4] = 3
  a[5] = 3 
  a[6] = 5 
  a[7] = 3 
  a[8] = 3 
  a[9] = 25 
  a[10] = 17 
  a[11] = 2 
  a[12] = 1 
  a[13] = 0.25 
  a[14] = 0.05 
  a[15] = 0.07# 0.01 #0.005999 
  a[16] = 0.005 # 0.017999 
  a[17] = 0.07# 0.025299 
  a[18]=  0.0001 # 0.050649 
  a[19] = 0.08 # 0.069499 
  a[20] = 0.25 # 0.124999 
  a[21] = 0.08# 0.124999 
  a[22] = 0.5 # 0.209999 
  a[23] = 1e-6   
  a[24] = 2 #14
  a[25] = 1e-6
  a[26] = 1e-6
  a[27] = 1
  a[28] = 2
  a[29] = 0.5
  
  b <- rep(0,n)
  b[1] = 14 
  b[2] = 13
  b[3] = 7 
  b[4] = 9
  b[5] = 7
  b[6] = 9 
  b[7] = 5
  b[8] = 7 
  b[9] = 35
  b[10] = 25  
  b[11] = 5
  b[12] = 7
  b[13] = 2
  b[14] = 0.15
  b[15] = 0.11# 0.08         #0.018001
  b[16] = 0.02 #0.08         #0.054001
  b[17] = 0.13# 0.075901
  b[18] =  0.002# 0.151951
  b[19] = 0.12# 0.208501
  b[20] = 0.35# 0.375001
  b[21] = 0.12 #0.375001
  b[22] = 0.9# 0.630001
  b[23] = 0.01
  b[24] = 4 # 28  
  b[25] = 1.1  
  b[26] = 0.0625 
  b[27] = 2  
  b[28] = 5
  b[29] = 0.75
  
  ifelse( sum(a>=b) == 0, 
          return( list(lower = a, upper = b )),
          stop("getBounds: inconsistent bounds"))
}




#' @title mapXToPara
#' 
#' @description  \code{mapXToPara} accepts
#' a n-dim vector. Its values will be mapped onto a \code{\link{babsimHospitalPara}}
#' list.
#' 
#' @details This function will replaced hte function \code{simulateHospital} in versions >=  1.2.8.
#' 
#' @param x (num) n-dim vector. Values will be mapped onto \code{babsimHospitalPara}.
#' Names of these parameters can be obtained via \code{\link{getParameterName}}.
#' 
#' @return This function returns an env data frame (tibble [560 × 15] (S3: grouped_df/tbl_df/tbl/data.frame))
#' with the following entries:
#' \describe{
#'		\item{\code{resource} (chr)}{name of the seized resource: "bed" "bed" "bed" "bed" ...}
#'		\item{\code{time} (num)}{time step:  3 10 12 13 14 15 15 15 15 16 ...}
#'		\item{\code{server} (int)}{server: 1 2 3 2 3 4 3 4 5 6 ...}
#'		\item{\code{limit} (num)}{limit: Inf Inf Inf Inf Inf ...}
#'		\item{\code{replication} (int)}{replication:  1 1 1 1 1 1 1 1 1 1 ...}
#'		\item{\code{upper} (int)}{upper: 1 2 3 2 3 5 5 5 5 7 ...}
#'		\item{\code{lower} (int)}{lower: 1 2 3 2 3 3 3 3 3 5 ...}
#'		\item{\code{med} (num)}{med: 1 2 3 2 3 4 4 4 4 6 ...}
#'		\item{\code{date} (POSIXct)}{time, format: \code{yyyy-mm-dd hh:mm.ss}}
#'		\item{\code{rwdate} (POSIXct)}{format: "2020-03-01" "2020-03-08" "2020-03-15" "2020-03-15" ...}
#'		\item{\code{source} (chr)}{name of the simulation that was used: "babsim" "babsim" "babsim" "babsim" ...}
#'		}
#'  
#' @examples 
#' x <- rep(0.2,29)
#' para <- mapXToPara(x)
#' conf <-  babsimToolsConf()
#' data <- getObkData()
#' res <- modelResultHospital(para=para, conf=conf, data = data)
#' getError(res = res, conf = conf)
#' p <- plotDailyMaxResults(res)
#' 
#' @export

mapXToPara <- function(x){
  para <- babsimHospitalPara()
  para$AmntDaysInfectedToHospital = x[1]
  para$AmntDaysNormalToHealthy = x[2]
  para$AmntDaysNormalToIntensive = x[3]
  para$AmntDaysNormalToVentilation = x[4]
  para$AmntDaysNormalToDeath = x[5]
  para$AmntDaysIntensiveToAftercare = x[6]
  para$AmntDaysIntensiveToVentilation = x[7]
  para$AmntDaysIntensiveToDeath = x[8]
  para$AmntDaysVentilationToIntensiveAfter = x[9]
  para$AmntDaysVentilationToDeath = x[10]
  para$AmntDaysIntensiveAfterToAftercare = x[11]
  para$AmntDaysIntensiveAfterToDeath = x[12]
  para$GammaShapeParameter = x[13]
  para$FactorPatientsInfectedToHospital = x[14]
  para$FactorPatientsHospitalToIntensive = x[15]
  para$FactorPatientsHospitalToVentilation = x[16]
  para$FactorPatientsNormalToIntensive = x[17]
  para$FactorPatientsNormalToVentilation = x[18]
  para$FactorPatientsNormalToDeath = x[19]
  para$FactorPatientsIntensiveToVentilation = x[20]
  para$FactorPatientsIntensiveToDeath = x[21]
  para$FactorPatientsVentilationToIntensiveAfter = x[22]
  para$FactorPatientsIntensiveAfterToDeath = x[23]
  para$AmntDaysAftercareToHealthy = x[24]
  para$RiskFactorA = x[25]
  para$RiskFactorB = x[26]
  para$RiskMale = x[27]
  para$AmntDaysIntensiveAfterToHealthy = x[28]
  para$FactorPatientsIntensiveAfterToHealthy  = x[29]
  return(para)
}

#' @title mapPToPara
#' 
#' @description  \code{mapPToPara} accepts
#' a nxn matrix. Its values will be mapped onto 
#' the probability entries of a \code{\link{babsimHospitalPara}}
#' list.
#' 
#' @param P (num) nxn-dim matrix. Values will be mapped onto the probabilities in
#' \code{babsimHospitalPara}.
#' Names of these parameters can be obtained via \code{\link{getParameterName}}.
#' @param para Parameter list, e.g., generated via \code{\link{babsimHospitalPara}}
#' 
#' @return This function returns a parameter list.
#'  
#' @examples 
#' para <- babsimHospitalPara()
#' P <- getMatrixP() 
#' para <-  mapPToPara(P = P,
#'                     para = para)
#'  
#' @export

mapPToPara <- function(P = getMatrixP(),
                       para){
  para$FactorPatientsInfectedToHospital = P[1,3] #x[14]
  para$FactorPatientsHospitalToIntensive = P[3,5] # x[15]
  para$FactorPatientsHospitalToVentilation = P[3,6] # x[16]
  para$FactorPatientsNormalToIntensive = P[4,5] # x[17]
  para$FactorPatientsNormalToVentilation = P[4,6] # x[18]
  para$FactorPatientsNormalToDeath = P[4,9] # x[19]
  para$FactorPatientsIntensiveToVentilation = P[5,6] # x[20]
  para$FactorPatientsIntensiveToDeath = P[5,9] # x[21]
  para$FactorPatientsVentilationToIntensiveAfter = P[6,7] # x[22]
  para$FactorPatientsIntensiveAfterToDeath = P[7,9] # x[23]
  para$FactorPatientsIntensiveAfterToHealthy = P[7,10] # x[29]
  return(para)
}


#' @title checkSimPara
#' 
#' @description check (and correct) parameter list
#' 
#' @param para list: optimization parameters, e.g., generated via \code{\link{babsimHospitalPara}}
#' 
#' @return corrected parameter list
#' 
#' @examples 
#' 
#' x0 <- babsimHospitalPara()
#' x <- checkSimPara(x0)
#' 
#' @export
checkSimPara <- function(para=babsimHospitalPara()){
  minDays = 1e-6
  maxDays = 365
  minVal = 1e-6
  maxVal = 1e6
  minProb = 0
  maxProb = 1
  #
  # x1:
  para$AmntDaysInfectedToHospital <- ensureRangeOpen(para$AmntDaysInfectedToHospital, minDays, maxDays) 
  # x2:
  para$AmntDaysNormalToHealthy <- ensureRangeOpen(para$AmntDaysNormalToHealthy, minDays, maxDays)
  # x3:
  para$AmntDaysNormalToIntensive <- ensureRangeOpen(para$AmntDaysNormalToIntensive, minDays, maxDays)
  # x4:
  para$AmntDaysNormalToVentilation <- ensureRangeOpen(para$AmntDaysNormalToVentilation, minDays, maxDays)
  # x5:
  para$AmntDaysNormalToDeath <- ensureRangeOpen(para$AmntDaysNormalToDeath, minDays, maxDays)
  # x6:
  para$AmntDaysIntensiveToAftercare <- ensureRangeOpen(para$AmntDaysIntensiveToAftercare, minDays, maxDays)
  # x7:
  para$AmntDaysIntensiveToVentilation <- ensureRangeOpen(para$AmntDaysIntensiveToVentilation, minDays, maxDays)
  # x8: 
  para$AmntDaysIntensiveToDeath <- ensureRangeOpen(para$AmntDaysIntensiveToDeath, minDays, maxDays)
  # x9:
  para$AmntDaysVentilationToIntensiveAfter <- ensureRangeOpen(para$AmntDaysVentilationToIntensiveAfter, minDays, maxDays)
  # x10:
  para$AmntDaysVentilationToDeath <- ensureRangeOpen(para$AmntDaysVentilationToDeath, minDays, maxDays)
  # x11:
  para$AmntDaysIntensiveAfterToAftercare <- ensureRangeOpen(para$AmntDaysIntensiveAfterToAftercare, minDays, maxDays)
  # x12:
  para$AmntDaysIntensiveAfterToDeath <- ensureRangeOpen(para$AmntDaysIntensiveAfterToDeath, minDays, maxDays)
  # x13: 
  para$GammaShapeParameter <- ensureRangeOpen(para$GammaShapeParameter, minVal, maxVal)
  # x14:
  para$FactorPatientsInfectedToHospital <- ensureRangeOpen(para$FactorPatientsInfectedToHospital, minProb, maxProb)
  # x15: 
  para$FactorPatientsHospitalToIntensive <- ensureRangeOpen(para$FactorPatientsHospitalToIntensive, minProb, maxProb)
  # x16:
  para$FactorPatientsHospitalToVentilation <- ensureRangeOpen(para$FactorPatientsHospitalToVentilation, minProb, maxProb)
  # x17:
  para$FactorPatientsNormalToIntensive <- ensureRangeOpen(para$FactorPatientsNormalToIntensive, minProb, maxProb)
  # x18:
  para$FactorPatientsNormalToVentilation <- ensureRangeOpen(para$FactorPatientsNormalToVentilation, minProb, maxProb)
  # x19:
  para$FactorPatientsNormalToDeath <- ensureRangeOpen(para$FactorPatientsNormalToDeath, minProb, maxProb)
  # x20:
  para$FactorPatientsIntensiveToVentilation <- ensureRangeOpen(para$FactorPatientsIntensiveToVentilation, minProb, maxProb)
  # x21:
  para$FactorPatientsIntensiveToDeath <- ensureRangeOpen(para$FactorPatientsIntensiveToDeath, minProb, maxProb)
  # x22:
  para$FactorPatientsVentilationToIntensiveAfter <- ensureRangeOpen(para$FactorPatientsVentilationToIntensiveAfter, minProb, maxProb)
  # x23:
  para$FactorPatientsIntensiveAfterToDeath <- ensureRangeOpen(para$FactorPatientsIntensiveAfterToDeath, minProb, maxProb)
  # x24:
  para$AmntDaysAftercareToHealthy <- ensureRangeOpen(para$AmntDaysAftercareToHealthy, minDays, maxDays)
  # x25:
  para$RiskFactorA = ensureRangeOpen(para$RiskFactorA, minVal, maxVal)
  # x26:
  para$RiskFactorB = ensureRangeOpen(para$RiskFactorB, minVal, maxVal)
  # Risk based on gender
  # x27:
  para$RiskMale <- ensureRangeOpen(para$RiskMale, minVal, maxVal)
  # x28:
  para$AmntDaysIntensiveAfterToHealthy <- ensureRangeOpen(para$AmntDaysIntensiveAfterToHealthy, minDays, maxDays)
  # x29:
  para$FactorPatientsIntensiveAfterToHealthy <- ensureRangeOpen(para$FactorPatientsIntensiveAfterToHealthy, minProb, maxProb)
  ## Repair probabilities:
  P <- getMatrixP(para = para)
  m <- rep(1, nrow(P))
  Q <- P %*% diag(m)
  S <- diag(1/rowSums(Q))
  R <- S %*% Q
  para <- mapPToPara(P=R,
                     para=para)
  return(para)
}


#' @title ensureRangeOpen
#' @description Ensure that value belongs to the open interval ]a,b[ 
#' @param x value
#' @param a lower limit
#' @param b upper limit
#' @return corrected value
#' @examples 
#' # return 1:
#' ensureRangeOpen(x=10, a=0, b=1)
#' # return 0:
#' ensureRangeOpen(x=0, a=0, b=1)
#' # return 0.5:
#' ensureRangeOpen(x=0.5, a=0, b=1)
#' 
#' @export
#'  
ensureRangeOpen <- function(x, a, b) {
  if (x < a) {
    return(a)
  }
  else{
    if (x > b) {
      return(b)
    }
    else{
      return(x)
    }
  }
}


#' @title getParameterName
#' 
#' @description Returns the name (chr) of the babsim \code{x} parameter vector. 
#' 
#' @param n int: position

#' @return This function returns a  character value, which represents
#' the name of the n-th \code{x} variable.

#' @examples
#' getParameterName(16)
#'  
#' @export


getParameterName <- function(n){
  param = list("AmntDaysInfectedToHospital",
               "AmntDaysNormalToHealthy",
               "AmntDaysNormalToIntensive",
               "AmntDaysNormalToVentilation",
               "AmntDaysNormalToDeath",
               "AmntDaysIntensiveToAftercare",
               "AmntDaysIntensiveToVentilation",
               "AmntDaysIntensiveToDeath",
               "AmntDaysVentilationToIntensiveAfter",
               "AmntDaysVentilationToDeath",
               "AmntDaysIntensiveAfterToAftercare",
               "AmntDaysIntensiveAfterToDeath",
               "GammaShapeParameter",
               "FactorPatientsInfectedToHospital",
               "FactorPatientsHospitalToIntensive",
               "FactorPatientsHospitalToVentilation",
               "FactorPatientsNormalToIntensive",
               "FactorPatientsNormalToVentilation",
               "FactorPatientsNormalToDeath",
               "FactorPatientsIntensiveToVentilation",
               "FactorPatientsIntensiveToDeath",
               "FactorPatientsVentilationToIntensiveAfter",
               "FactorPatientsIntensiveAfterToDeath",
               "AmntDaysAftercareToHealthy",
               "RiskFactorA",
               "RiskFactorB",
               "RiskMale",
               "AmntDaysIntensiveAfterToHealthy",
               "FactorPatientsIntensiveAfterToHealthy"
               )
  return(param[[n]])
}


#' @title getParameterNameList
#' 
#' @description Returns the names (chr) of the babsim \code{x} parameter vector. 
#' 
#' @param x vector of int: positions

#' @return This function returns a vector. Its elements represent
#' the names of the n-th \code{x} variables.

#' @examples
#' getParameterNameList( c(16,18) )
#'  
#' @export


getParameterNameList <- function(x){
  y <- matrix(x, 1,)
  y <- apply(X = y, FUN = getParameterName, 2)
  names(y) <- paste0("x",x)
  return(y)
}


#' @title getParameterDataFrame
#' 
#' @description Get parameterss (probabilities and durations) of the
#' babsim.hospital simulator 
#' 
#' @param paraList list of parameter values. 
#' Each list element has the form \code{obk=obkparam}.
#' An example list looks as follows: \code{list(obk=obkpara, 
#'                                             koeln=koelnpara, 
#'                                             nrw=nrwpara)}
#'                                             
#' @return data.frame with parameters in each column.
#' 
#' @examples 
#' 
#' df <- getParameterDataFrame(paraList = list(obk=obkpara, 
#'                                             koeln=koelnpara, 
#'                                             nrw=nrwpara))
#' @export 
getParameterDataFrame <- function(paraList = list(obk=babsim.hospital::obkpara, 
                                                  koeln=babsim.hospital::koelnpara, 
                                                  nrw=babsim.hospital::nrwpara)){
  x <- data.frame(getStartParameter())
  n <- length(x)
  p <- as.character(getParameterNameList(1:n))
  colnames(x) <- p
  x <- t(x)
  for (para in paraList){
    yColumn <- match('y', colnames(para))
    bestRow <- which.min(para[, yColumn]) 
    q <- unlist(para[bestRow, -yColumn], 
                use.names = FALSE)
    x <- cbind(x, q)
  }
  bounds <- getBounds()
  x <- cbind(x, bounds[[1]], bounds[[2]])
  
  colnames(x) <- c("default", names(paraList), "min", "max")
  return(x)
}
 
#' @title getStartParameter
#' 
#' @description Returns parameter for babsim runs 
#' 
#' @param para parameter vector, e.g., generated via 
#' \code{\link{babsimHospitalPara}}. Default: \code{\link{babsimHospitalPara}}
#' @param region (int) use region specific start parameter, e.g., \code{5374} for OBK.
#' If \code{region} is negative (default), a generic start parameter is chosen.
#' The selection is based on the obkpara, koelnpara, and nrwpara parameter values,
#' which are the best known values found so far.
#' 
#' @return This function returns a (1,n) dim matrix
#' @examples
#' para <- getStartParameter(region = 5374)
#'  
#' @export
getStartParameter <- function(para = babsimHospitalPara(),
                              region = -1){
   if (region == 5374){
     n <- length(babsim.hospital::obkpara)
     obkpara <- unlist(babsim.hospital::obkpara)
    return(matrix(obkpara[2:n], 1, n-1))
  } else if (region == 5315){
    n <- length(babsim.hospital::koelnpara)
    koelnpara <- unlist(babsim.hospital::koelnpara)
    return(matrix(koelnpara[2:n], 1, n-1))
  } else if (region == 5){
    n <- length(babsim.hospital::nrwpara)
    nrwpara <- unlist(babsim.hospital::nrwpara)
    return(matrix(nrwpara[2:n], 1, n-1))
  } else {
  x0 <- c(
    para$AmntDaysInfectedToHospital,
    para$AmntDaysNormalToHealthy,
    para$AmntDaysNormalToIntensive,
    para$AmntDaysNormalToVentilation,
    para$AmntDaysNormalToDeath,
    para$AmntDaysIntensiveToAftercare,
    para$AmntDaysIntensiveToVentilation,
    para$AmntDaysIntensiveToDeath,
    para$AmntDaysVentilationToIntensiveAfter,
    para$AmntDaysVentilationToDeath,
    para$AmntDaysIntensiveAfterToAftercare,
    para$AmntDaysIntensiveAfterToDeath,
    para$GammaShapeParameter,
    para$FactorPatientsInfectedToHospital,
    para$FactorPatientsHospitalToIntensive,
    para$FactorPatientsHospitalToVentilation,
    para$FactorPatientsNormalToIntensive,
    para$FactorPatientsNormalToVentilation,
    para$FactorPatientsNormalToDeath,
    para$FactorPatientsIntensiveToVentilation,
    para$FactorPatientsIntensiveToDeath,
    para$FactorPatientsVentilationToIntensiveAfter,
    para$FactorPatientsIntensiveAfterToDeath,
    para$AmntDaysAftercareToHealthy,
    para$RiskFactorA,
    para$RiskFactorB,
    para$RiskMale,
    para$AmntDaysIntensiveAfterToHealthy,
    para$FactorPatientsIntensiveAfterToHealthy
  )
  return(matrix(x0, 1, length(x0)))
  }
}


#' @title Return the best parameter set found
#'
#' @description Extract the best result from a data frame of optimization runs and return it as a valid Para list.
#'
#' @param para A data frame with columns 'y' and 'x.1' to 'x.27'.
#'
#' @return This function returns an env data frame (tibble [560 × 15] (S3: grouped_df/tbl_df/tbl/data.frame))
#' with the following entries:
#' \describe{
#'		\item{\code{resource} (chr)}{name of the seized resource: "bed" "bed" "bed" "bed" ...}
#'		\item{\code{time} (num)}{time step:  3 10 12 13 14 15 15 15 15 16 ...}
#'		\item{\code{server} (int)}{server: 1 2 3 2 3 4 3 4 5 6 ...}
#'		\item{\code{limit} (num)}{limit: Inf Inf Inf Inf Inf ...}
#'		\item{\code{replication} (int)}{replication:  1 1 1 1 1 1 1 1 1 1 ...}
#'		\item{\code{upper} (int)}{upper: 1 2 3 2 3 5 5 5 5 7 ...}
#'		\item{\code{lower} (int)}{lower: 1 2 3 2 3 3 3 3 3 5 ...}
#'		\item{\code{med} (num)}{med: 1 2 3 2 3 4 4 4 4 6 ...}
#'		\item{\code{date} (POSIXct)}{time, format: \code{yyyy-mm-dd hh:mm.ss}}
#'		\item{\code{rwdate} (POSIXct)}{format: "2020-03-01" "2020-03-08" "2020-03-15" "2020-03-15" ...}
#'		\item{\code{source} (chr)}{name of the simulation that was used: "babsim" "babsim" "babsim" "babsim" ...}
#'		}
#' 
#' @seealso \code{\link{mapXToPara}}
#' 
#' @examples
#'   getBestParameter(obkpara)
#'   
#' @export
getBestParameter <- function(para) {
  yColumn <- match('y', colnames(para))
  bestRow <- which.min(para[, yColumn]) # y column
  mapXToPara(unlist(para[bestRow, -yColumn], use.names=FALSE))
}

#' @title Smooth a parameter set using another parameter set
#' 
#' @description Calculate the average of two parameter sets to smooth out any local anomalies.
#' Mostly useful to smooth out a local (say OBK) parameter set using a global one (say NRW).
#'
#' Technically this function calculates 
#'   \code{(1-weight) * para + weight * other}
#' ensuring that the names etc. of \code{para} are preserved.
#'
#' @param para Parameter set to smooth
#' @param other Other parameters to average in
#' @param weight Weight of other parameters
#'
#' @return Weighted parameter set
#' 
#' @export
smoothParameter <- function(para, other, weight=0.2) {
  newParameters <- as.list(unlist(para) * (1 - weight) + unlist(other) * weight)
  names(newParameters) <- names(para)
  newParameters
}
