#' @title  switchRkiRefdatumToMeldedatum Change RKI data File
#'
#' @description Use Meldedatum instead of Refdatum in
#' \code{\link{rkidata}}. It is recommended to run
#' install and restart the package afterwards.
#'
#' @param rkidata RKI Data
#' @param overwrite logical Overwrite existing file. Default \code{TRUE}.
#'
#' @export

switchRkiRefdatumToMeldedatum <- function(rkidata = rkidata, overwrite = TRUE) {
  names(rkidata) <- c(
    "FID", "IdBundesland", "Bundesland", "Landkreis", "Altersgruppe",
    "Geschlecht", "AnzahlFall", "AnzahlTodesfall", "Refdatum", "IdLandkreis",
    "Datenstand", "NeuerFall", "NeuerTodesfall", "Meldedatum", "NeuGenesen",
    "AnzahlGenesen", "IstErkrankungsbeginn", "Altersgruppe2"
  )
  usethis::use_data(name = rkidata, overwrite = overwrite)
}
