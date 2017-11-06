mansched
========

R package with tools for manpower cost computation in Taganito Mine

Installation
------------

### R

This package requires an [R](https://www.r-project.org/) version of at least 3.4.0 although it is recommended to install the latest version.

### java

Installation of mansched requires java ([jre](https://java.com/inc/BrowserRedirect1.jsp?locale=en)). A 64-bit installation of R requires 64-bit installation of java while a 32-bit installation of R requires 32-bit installation of java.

Install the appropriate version of java.

### Rtools and XCode

If you are using a windows machine, installation of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) is required.

If you are using OS X, installation of [Xcode](https://developer.apple.com/xcode/) is required.

### devtools

mansched is only available via github. To facilitate the installation of mansched, install [devtools](https://github.com/hadley/devtools).

To install devtools, in R console, run:

``` r
install.packages("devtools")
```

### mansched install

Once devtools is installed, run the following in R console:

``` r
devtools::install_github('basilrabi/mansched')
```

To do
-----

-   Correct calculation of bonus

To do (Enhancements)
--------------------

-   Speed up code
    -   Migrate some code to other language
    -   See similarities in code chunks using`tempData <- getCM(x)` in `getCost()`
-   Add visualization tools for excess pool and requirement (shiny app)
