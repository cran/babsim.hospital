#' @title updateParaSet
#'
#' @description Delete old parameters from paras and replace them with new ones.
#'
#' @param paramDF the data frame with the new parameters that should replace the old ones
#' @param region integer, the region id
#' @param path path to the old para file.
#'
#' @return parameter set
#'
#' @examples
#' getParaSet(5315)
#' @export
updateParaSet <- function(paramDF, region, path = NULL) {
  ## Add region variable to freshly optimized parameter set
  newParams <- paramDF
  newParams$region <- region

  ## Load old paras and remove alls rows with the given region
  if (is.null(path)) {
    allParas <- babsim.hospital::paras[babsim.hospital::paras$region != region, ]
  } else {
    loadRData <- function(fileName) {
      # loads an RData file, and returns it
      load(fileName)
      get(ls()[ls() != "fileName"])
    }
    allParas <- loadRData(path)
    allParas <- allParas[allParas$region != region, ]
  }

  ## Add new parameters to the data frame
  allParas <- rbind(allParas, newParams)

  ## Update package data
  paras <- allParas
  usethis::use_data(name = paras, overwrite = TRUE)
}
