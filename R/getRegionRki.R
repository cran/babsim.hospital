#' @title  getRegionRki Auswahl der RKI Daten fuer eine Region
#'
#' @description Auswahl anhand der Bundeslaender, Landkreis IDs
#'
#' @param data Daten, z.B. rkidata
#' @param region Id der Region, \code{0} Deutschland
#'
#' @examples
#' data <- getRegionRki(
#'   data = babsim.hospital::rkidata[1:1000, ],
#'   region = 0
#' )
#' @export

getRegionRki <- function(data = babsim.hospital::rkidata, region) {
  region <- as.integer(region)
  if (region > 0 & region < 100) {
    return(data[data$IdBundesland == region, ])
  } else if (region >= 100) {
    return(data[data$IdLandkreis == region, ])
  } else {
    return(data)
  }
}
