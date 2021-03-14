#' Retrieve links to all DIVI day reports.
#'
#' @return List of download URLs to the daily reports.
#'
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#'
#' @export
getDiviLinks <- function() {
  html_data <- xml2::read_html("https://www.divi.de/divi-intensivregister-tagesreport-archiv-csv?layout=table")
  links <- html_data %>% html_nodes("a")
  urls <- links %>% html_attr("href")

  prefix <- "https://www.divi.de"

  sampleLink <- "/divi-intensivregister-tagesreport-archiv-csv/"
  sampleSite <- "/divi-intensivregister-tagesreport-archiv-csv?layout=table&start="

  sites <- c()
  report <- c()

  # Scrap the first page
  for (link in urls) {
    if (grepl(sampleLink, link, fixed = TRUE)) {
      report <- append(report, link)
    } else if (grepl(sampleSite, link, fixed = TRUE)) {
      sites <- append(sites, link)
    }
  }

  # Search for the highest tableindex
  listLength <- c()
  for (i in sites) {
    startNumber <- as.integer(substr(i, nchar(sampleSite) + 1, nchar(i)))
    listLength <- append(listLength, startNumber)
  }
  highestNumber <- max(listLength)

  # create all indices
  size <- highestNumber / 20
  tableIndex <- paste0(sampleSite, (0:size) * 20)
  sites <- unique(tableIndex)

  # Fetch HTML for every other site
  for (site in sites) {
    html_data <- xml2::read_html(paste0(prefix, site))
    urls <- html_attr(html_nodes(html_data, "a"), "href")
    report <- c(report, urls[grepl(sampleLink, urls, fixed = TRUE)])
  }

  paste0(prefix, unique(report))
}
