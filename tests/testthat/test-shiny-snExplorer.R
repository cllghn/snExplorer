library(shinytest)
library(testthat)
context("app-file")
# This file is for testing the applications in the inst/ directory.

test_that("snExplorer works", {
    skip_on_cran()
    skip_on_ci() # something's off on system dependencies
    
    app_dir <- system.file(package = "snExplorer", "shiny-snExplorer")
    expect_pass(testApp(app_dir, compareImages = FALSE))
    
})
