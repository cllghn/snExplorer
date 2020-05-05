# Load up all required packages ================================================
suppressPackageStartupMessages({
    library(V8)
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

# Define the normalized harmonic centrality funtion ============================
sne_harmonic_centrality <- function(g, normalized = TRUE) {
    # Test input is igraph
    stopifnot(igraph::is_igraph(g))  
    # Reciprocal distances
    dist       <- 1/igraph::distances(g, mode = "all")
    # Remove diagonal (no self distance)
    diag(dist) <- 0
    # Normalize test
    if (normalized == FALSE) {
        return(rowSums(dist))
    }
    # Normalize
    out        <- rowSums(dist)/(igraph::vcount(g) - 1)
    # Return output
    out 
}

# Define a function for degree centrality with undirected data =================
sne_undirected_degree <- function(g, weighted = FALSE, loops = TRUE) {
    stopifnot(igraph::is_igraph(g)) 
    if (igraph::is_directed(g)) {
        stop("graph is directed")
    }
    if (weighted == TRUE) {
        out <- igraph::get.adjacency(g,
                                     type   = "upper",
                                     names  = TRUE,
                                     sparse = FALSE,
                                     attr   = "weight")
        return(rowSums(out) + colSums(`diag<-`(out, 0)))
    }
    out <- igraph::get.adjacency(g,
                                 type   = "upper",
                                 names  = TRUE,
                                 sparse = FALSE)
    
    if (loops == FALSE) {
        return(rowSums(`diag<-`(out, 0)) + colSums(`diag<-`(out, 0))) 
    }
    
    rowSums(out) + colSums(`diag<-`(out, 0))
}

# TODO: Add metadata using the library(metathis) functions.