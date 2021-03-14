#' @title rkiToBabsimData
#'
#' @description Transforms the freshly downloaded rki data into the babsim data frame.
#'  Also imputes mising dates as zero frequency in the data.
#'
#' @param rki data.frame of downloaded rki data before preprocessing
#'
#' @return a data.frame of aggregated data
#'  \describe{
#'     \item{\code{Day}}{Date, format: '2020-01-01' '2020-01-02' '2020-01-03' '2020-01-04' ...}
#'     \item{\code{Infected}}{Infiziert: num  1 0 0 0 0 0 0 0 0 0 ...}
#'     \item{\code{Weiblich}}{Geschlecht weiblich: int  0 0 0 0 0 0 0 0 0 0 ...}
#'     \item{\code{Maennlich}}{Geschlecht maennlich: int  1 0 0 0 0 0 0 0 0 0 ...}
#'     \item{\code{GUnbekannt}}{Geschlecht unbekannt: int  0 0 0 0 0 0 0 0 0 0 ...}
#'     }
#'
#' @importFrom data.table data.table
#' @examples
#'
#' data <- rkiToBabsimData(rkidata[1:100, ])
#' plot(data$Day, data$Infected, type = "o")
#' #
#' max(data$Day) - min(data$Day)
#' @export
rkiToBabsimData <- function(rki = babsim.hospital::rkidata) {
  rki <- data.table::data.table(rki)
  rki$Day <- as.Date(rki$Refdatum)

  ## Normalize levels
  rki$Geschlecht <- factor(rki$Geschlecht, levels = c("M", "W", "unbekannt"), labels = c(
    "Maennlich",
    "Weiblich", "GUnbekannt"
  ))
  rki$Altersgruppe <- factor(rki$Altersgruppe, levels = c(
    "A00-A04", "A05-A14",
    "A15-A34", "A35-A59", "A60-A79", "A80+", "unbekannt"
  ), labels = c(
    "A00A04",
    "A05A14", "A15A34", "A35A59", "A60A79", "A80", "AUnbekannt"
  ))

  res <- data.table(Day = seq(min(rki$Day), max(rki$Day), by = 1))

  sex <- data.table::dcast(rki, Day ~ Geschlecht, value.var = "AnzahlFall", fun.aggregate = sum)
  res <- merge(res, sex, by = "Day", all.x = TRUE)

  age <- data.table::dcast(rki, Day ~ Altersgruppe, value.var = "AnzahlFall", fun.aggregate = sum)
  res <- merge(res, age, by = "Day", all.x = TRUE)

  total <- data.table::dcast(rki, Day ~ ., value.var = "AnzahlFall", fun.aggregate = sum)
  colnames(total) <- c("Day", "Infected")
  res <- merge(res, total, by = "Day", all.x = TRUE)

  res <- data.table::setnafill(res, fill = 0)
  as.data.frame(res)
}
