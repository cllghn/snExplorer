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
})

# Source files with additional functionalities =================================
source("import_modal.R")
# source("export_modal.R")

# Define the js method that resets the page ====================================
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
`%{}%` <- function(lhs, rhs) if (length(lhs)) lhs else rhs

# TODO: Add metadata using the library(metathis) functions.