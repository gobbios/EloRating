---
output:
  html_document: default
  pdf_document: default
---
# EloRating2

currently on CRAN:

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/EloRating)](https://cran.r-project.org/package=EloRating)


This is my first attempt to put the `EloRating` package on GitHub.

Until the transfer of all functions to GiHub is complete, the package is named `EloRating2` and should be considered incomplete.

To install the current development version from GitHub, use the following command:

`devtools::install_github("gobbios/EloRating2")`

Since version 0.45-0, the package contains C++ code, which means that you need to have some extra tools installed if you want to install from GitHub:

+ **Windows**: Rtools from  https://cran.r-project.org/bin/windows/Rtools/

+ **Mac**: Xcode from the App Store


If you want to have a pdf of the tutorial, please use:

`devtools::install_github("gobbios/EloRating2", build_vignettes=TRUE)`

