
medicare: tools for simplifying Medicare data analysis
======================================================

<!-- README.md is generated from README.Rmd. Please edit that file -->
The medicare package is a collection of functions and methods I've used to manipulate Medicare data and get it ready for analysis. This includes things like efficiently subsetting messy Cost Report data to pull desired variables, renaming variables in data that doesn't come with headers, and finding more useful names for Provider of Service files from the early 2000's that name variables sequentially from "PROV0001".

Publicly available Medicare data often requires extensive preparation and cleaning before any analysis can take place. Files are often raw dumps of database tables, which the researcher is expected to subset and merge to make a workable dataset. This package contains methods to extract data from such datasets (e.g. [Cost Reports](https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Cost-Reports/Cost-Reports-by-Fiscal-Year.html)), provide useful names for variables (Cost Reports and [Provider of Services File](https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Provider-of-Services/)), and even parse data dictionary / layout files to extract variable names for older datasets, where names in the raw data are essentially `Var1, Var2, Var3...` (Provider of Services File).

Installation and Documentation
------------------------------

`medicare` is under active development and available on CRAN. You can install the latest release version of the package by using

    install.packages("medicare")

You can install the development version of the `medicare` package using [devtools](https://github.com/hadley/devtools):

    library(devtools)
    devtools::install_github("robertgambrel/medicare")

Please let me know about any problems by [opening an issue](http://github.com/robertgambrel/medicare/issues).

For detailed examples on how to use some of the functionality, check out the Vignettes, which show examples similar to what I've done in my own work.

    browseVignettes(package="medicare")
