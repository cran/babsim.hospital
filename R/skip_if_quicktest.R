#' Skip test if in quicktest mode
#'
#' Skip test if option \sQuote{babsim.hospital.quicktest} is \code{TRUE} or if
#' the environment variable \sQuote{BABSIM_HOSPTIAL_QUICKTEST} is set.
#'
#' @importFrom testthat skip
skip_if_quicktest <- function() {
  o <- options()
  e <- Sys.getenv()
  shouldSkip <- FALSE

  if ("babsim.hospital.test.level" %in% names(o)) {
    message("Using deprecated 'babsim.hospital.test.level'. Please use the 'babsim.hospital.longtest' flag instead.")
    if (o[["babsim.hospital.test.level"]] == 0)
      shouldSkip <- TRUE
  } 
  if ("babsim.hospital.quicktest" %in% names(o)) {
    if (isTRUE(o[["babsim.hospital.quicktest"]])) 
      shouldSkip <- TRUE
  }
  if ("BABSIM_HOSPITAL_QUICKTEST" %in% names(e)) {
    if (toupper(e[["BABSIM_HOSPITAL_QUICKTEST"]]) != "NO") {
      shouldSkip <- TRUE
    }
  }
       
  if (shouldSkip) {
    skip("Skipping test because quicktest option is set.")
  }
}
