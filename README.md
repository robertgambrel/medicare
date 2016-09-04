
medicare: tools for simplifying Medicare data analysis
======================================================

<!-- README.md is generated from README.Rmd. Please edit that file -->
The medicare package is a collection of functions and methods I've used to manipulate Medicare data and get it ready for analysis. This includes things like efficiently subsetting messy Cost Report data to pull desired variables, renaming variables in data that doesn't come with headers, and finding more useful names for Provider of Service files from the early 2000's that name variables sequentially from "PROV0001".

Installation and Documentation
------------------------------

`medicare` is still under active development and thus not ready for CRAN. You can install the latest development version of the package by using

install.packages("broom")

You can also install the development version of the broom package using [devtools](https://github.com/hadley/devtools):

    library(devtools)
    install_github("robertgambrel/medicare")

For examples on how to use some of the functionality, check out the Vignettes, which show examples similar to what I've done in my work.

    browseVignettes(package="medicare")
