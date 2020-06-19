# Load up all required packages ================================================
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(visNetwork)
  library(readr)
  library(igraph)
  library(shinyjs)
  library(rmarkdown)
  library(scales)
  library(tools)
})

# Source files with additional functionalities =================================
source("import_modal.R")
# source("export_modal.R")

# Define the js method that resets the page ====================================
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
`%{}%` <- function(lhs, rhs) if (length(lhs)) lhs else rhs

# Define the normalized harmonic centrality function ===========================
sne_harmonic_centrality <- function(g, normalized = TRUE) {
  # Test input is igraph
  stopifnot(igraph::is_igraph(g))
  # Reciprocal distances
  dist <- 1 / igraph::distances(g, mode = "all")
  # Remove diagonal (no self distance)
  diag(dist) <- 0
  # Normalize test
  if (!normalized) {
    return(rowSums(dist))
  }
  # Normalize
  out <- rowSums(dist) / (igraph::vcount(g) - 1)
  # Return output
  out
}

# Define the inverse constraint function =======================================
.rconstraint <- function(g, nodes = igraph::V(g)) {
  res <- 1.125 - igraph::constraint(g, nodes = nodes)
  res[is.na(res) | res < 0] <- 0
  res
}

sne_rconstraint <- function(g, scope = c("extended", "ego"), ...) {
  stopifnot(igraph::is_igraph(g))

  scope <- match.arg(scope, c("extended", "ego"))
  if (scope == "extended") {
    return(.rconstraint(g))
  }

  had_names <- igraph::is_named(g)
  if (!had_names) {
    igraph::vertex_attr(g, "name") <- sprintf("n%d", seq_len(igraph::vcount(g)))
  }

  ego_nets <- igraph::make_ego_graph(g) # get all ego nets as list
  names(ego_nets) <- igraph::vertex_attr(g, "name") # put egos' names on each element
  # do what's essentially purrr::iwalk()
  out <- mapply(.rconstraint, ego_nets, names(ego_nets), USE.NAMES = FALSE)

  if (had_names) out else unname(out)
}

# Define a function for degree centrality with undirected data =================
sne_undirected_degree <- function(g, weighted = FALSE, loops = TRUE) {
  stopifnot(igraph::is_igraph(g))
  if (igraph::is_directed(g)) {
    stop("graph is directed")
  }
  if (weighted) {
    out <- igraph::get.adjacency(g,
      type = "upper",
      names = TRUE,
      sparse = FALSE,
      attr = "weight"
    )
    return(rowSums(out) + colSums(`diag<-`(out, 0)))
  }
  out <- igraph::get.adjacency(
    g,
    type = "upper",
    names = TRUE,
    sparse = FALSE
  )

  if (!loops) {
    return(rowSums(`diag<-`(out, 0)) + colSums(`diag<-`(out, 0)))
  }

  rowSums(out) + colSums(`diag<-`(out, 0))
}
