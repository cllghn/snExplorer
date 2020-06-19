`{snExplorer}`
================

  - [Installation](#installation)
  - [Lauching App](#lauching-app)

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/cjcallag/snExplorer/workflows/R-CMD-check/badge.svg)](https://github.com/cjcallag/snExplorer/actions?workflow=R-CMD-check)
[![Travis-CI Build
Status](https://travis-ci.org/cjcallag/snExplorer.svg?branch=master)](https://travis-ci.org/cjcallag/snExplorer)
[![Depends](https://img.shields.io/badge/Depends-GNU_R%3E=3.5-blue.svg)](https://www.r-project.org/)
[![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/cjcallag/snExplorer.svg)](https://github.com/cjcallag/snExplorer)
[![HitCount](http://hits.dwyl.io/cjcallag/snExplorer.svg)](http://hits.dwyl.io/cjcallag/snExplorer)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://www.mit.edu/~amini/LICENSE.md)
<!-- badges: end -->

**{snExplorer}** is a parsimonious Shiny application for interactive
visualization and analysis of networks.

## Installation

**{snExplorer}** is an R package and can be installed using
**{devtools}** :

``` r
if (!requireNamespace("remotes")) install.packages("remotes")

remotes::install_github("cjcallag/snExplorer")
```

## Lauching App

``` r
snExplorer::launch_shiny_app()
```

<img src="man/figures/landing.png" width="100%" />
