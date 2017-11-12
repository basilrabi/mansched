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

### Compilers

#### Windows

Installation of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) is required.

#### macOS

Installation of [Xcode](https://developer.apple.com/xcode/) is required. Additionally, you might need to [reconfigure](https://stackoverflow.com/questions/34971966/how-does-one-configure-rjava-on-osx-to-select-the-right-jvm-jinit-failing) the java installation.

In the terminal, run the following commands:

``` bash
sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib
sudo R CMD javareconf
```

Then in R console, run:

``` r
install.packages("rJava",type='source')
```

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
    -   Migrate some code to C++ (WIP)
    -   See similarities in code chunks using`tempData <- getCM(x)` in `getCost()`
-   Add visualization tools for excess pool and requirement (shiny app)
