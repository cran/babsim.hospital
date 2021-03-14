#' @title  visualizeGraph Visualisierung der Wahrscheinlichkeiten und Dauern
#'
#' @description Ergebnisse
#'
#' @importFrom methods as
#' @importFrom methods new
#' @importFrom igraph V
#' @importFrom igraph V<-
#' @importFrom igraph E
#' @importFrom igraph layout.reingold.tilford
#' @importClassesFrom markovchain markovchain
#' @importFrom igraph plot.igraph
#'
#' @param para parameter
#' @param option Option: plot probabilities ('P') or durations ('D')
#'
#' @examples
#'
#' para <- babsimHospitalPara()
#' visualizeGraph(para = para, option = "P")
#' @export

visualizeGraph <- function(para = babsimHospitalPara(), option = "P") {
  states <- c(
    "infec", "out", "hosp", "normal", "intens", "vent", "intafter", "aftercare",
    "death", "healthy"
  )

  para <- checkSimPara(para = para)
  P <- getMatrixP(para = para)
  D <- getMatrixD(para = para)
  M <- new("markovchain", transitionMatrix = P, states = states, name = "bubsim")
  # Do not show edges from S_i to S_i:
  for (i in 1:dim(P)[1]) {
    M@transitionMatrix[i, i] <- 0
  }
  g <- as(M, "igraph")
  for (i in states) {
    igraph::V(g)[i]$color <- "white"
  }
  igraph::V(g)[8]$color <- "lightgreen"
  igraph::V(g)[4]$color <- "yellow"
  igraph::V(g)[6]$color <- "yellow"
  igraph::V(g)[5]$color <- "orange"
  igraph::V(g)[9]$color <- "red"
  igraph::V(g)[10]$color <- "lightgreen"
  igraph::V(g)[2]$color <- "coral1"
  igraph::V(g)[3]$color <- "aliceblue"
  igraph::V(g)[7]$color <- "aliceblue"

  coords <- igraph::layout.reingold.tilford

  if (option == "P") {
    elabs <- round(igraph::E(g)$prob * 100, 1)
  } else {
    elabs <- round(c(D[1, 2], D[1, 3], D[3, 4], D[3, 5], D[3, 6], D[4, 5], D[
      4,
      6
    ], D[4, 9], D[4, 10], D[5, 6], D[5, 8], D[5, 9], D[6, 7], D[6, 9], D[
      7,
      8
    ], D[7, 9], D[7, 10], D[8, 10]), 1)
  }

  translator <- golem::get_golem_options("translator")
  if (!is.null(translator)) {
    if (option == "P") {
      TITLE <- translator$t("uebmap")
    } else {
      TITLE <- translator$t("uebmap2")
    }
  } else {
    if (option == "P") {
      TITLE <- "Wahrscheinlichkeiten (Prozent)"
    } else {
      TITLE <- "Dauern (Tage)"
    }
  }

  plot.igraph(g,
    vertex.color = igraph::V(g)$color, vertex.size = 25, vertex.label.cex = 1.2,
    edge.arrow.size = 0.5, edge.label = elabs, edge.label.cex = 1, edge.curved = 0.28,
    rescale = TRUE, layout = coords, asp = 0.9, main = TITLE
  )
}
