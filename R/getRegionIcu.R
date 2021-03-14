#' @title  getRegionIcu Auswahl der ICU Daten fuer eine Region
#'
#' @description Auswahl anhand der Bundeslaender, Landkreis IDs
#'
#' @param data Daten, z.B. icudata
#' @param region Id der Region, \code{0} Deutschland
#'
#' @examples
#' data <- getRegionIcu(babsim.hospital::icudata, 5315)
#' @export

getRegionIcu <- function(data = babsim.hospital::icudata, region = 5315) {
  region <- as.integer(region)
  if (region > 0 & region < 100) {
    return(data[data$bundesland == region, ])
  } else if (region >= 100) {
    return(data[data$gemeindeschluessel == region, ])
  } else {
    return(data)
  }
}
