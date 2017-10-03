mansched
========

R package with tools for manhours budgeting in Taganito Mine

Installation
------------

### R

This packages requires an [R](https://www.r-project.org/) version of at least 3.4.0 although it is recommended to install the latest version.

### java

Installation of mansched requires java ([jre](https://java.com/inc/BrowserRedirect1.jsp?locale=en)) to be installed first. 64-bit installation of R requires 64-bit installation of java while 32-bit installation of R requires 32-bit installation of java.

Install the appropriate version of java.

### devtools

mansched is only available via github. To facilitate the installation of mansched, install [devtools](https://github.com/hadley/devtools) first.

In R console, run:

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
-   Distribute other costs to non "OT" hours

To do (Enhancements)
--------------------

-   Speed up code
    -   Look into multithreading or migrate some code to other language
    -   See similarities in code chunks using`tempData <- getCM(x)` in `getCost()`
-   Change `something's wrong` error message
-   Remove `suppressMessages()`
