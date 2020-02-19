# Load up all required packages ================================================
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
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

# TODO: Add metadata using the library(metathis) functions.