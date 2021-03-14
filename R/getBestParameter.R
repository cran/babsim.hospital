#' @title Return the best parameter set found
#'
#' @description Extract the best result from a data frame of optimization runs and return it as a valid Para list.
#'
#' @param para A data frame with columns 'y' and 'x.1' to 'x.27'.
#'
#' @return This function returns an env data frame (tibble [560 × 15] (S3: grouped_df/tbl_df/tbl/data.frame))
#' with the following entries:
#' \describe{
#'     \item{\code{resource} (chr)}{name of the seized resource: 'bed' 'bed' 'bed' 'bed' ...}
#'     \item{\code{time} (num)}{time step:  3 10 12 13 14 15 15 15 15 16 ...}
#'     \item{\code{server} (int)}{server: 1 2 3 2 3 4 3 4 5 6 ...}
#'     \item{\code{limit} (num)}{limit: Inf Inf Inf Inf Inf ...}
#'     \item{\code{replication} (int)}{replication:  1 1 1 1 1 1 1 1 1 1 ...}
#'     \item{\code{upper} (int)}{upper: 1 2 3 2 3 5 5 5 5 7 ...}
#'     \item{\code{lower} (int)}{lower: 1 2 3 2 3 3 3 3 3 5 ...}
#'     \item{\code{med} (num)}{med: 1 2 3 2 3 4 4 4 4 6 ...}
#'     \item{\code{date} (POSIXct)}{time, format: \code{yyyy-mm-dd hh:mm.ss}}
#'     \item{\code{rwdate} (POSIXct)}{format: '2020-03-01' '2020-03-08' '2020-03-15' '2020-03-15' ...}
#'     \item{\code{source} (chr)}{name of the simulation that was used: 'babsim' 'babsim' 'babsim' 'babsim' ...}
#'     }
#'
#' @seealso \code{\link{mapXToPara}}
#'
#' @examples
#' getBestParameter(getParaSet(5374))
#' @export
getBestParameter <- function(para) {
  yColumn <- match("y", colnames(para))
  bestRow <- which.min(para[, yColumn]) # y column
  mapXToPara(unlist(para[bestRow, -yColumn], use.names = FALSE))
}
