mansched
========

R package with tools for manpower cost computation in Taganito Mine

Installation
------------

### R

This package requires an [R](https://www.r-project.org/) version of at least
3.5.0 although it is recommended to install the latest version.

### java

Installation of mansched requires java
([jre](https://java.com/inc/BrowserRedirect1.jsp?locale=en)).
A 64-bit installation of R requires 64-bit installation of java while a 32-bit
installation of R requires 32-bit installation of java.

Install the appropriate version of java for your R installation.

#### java in macOS

In macOS, you might need to
[reconfigure](https://stackoverflow.com/questions/34971966/how-does-one-configure-rjava-on-osx-to-select-the-right-jvm-jinit-failing)
the java installation.

To reconfigure the java installation in macOS, run the following commands in terminal:

``` bash
sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib
sudo R CMD javareconf
```

Then in R console, run:

``` r
install.packages("rJava", type = "source")
```

### devtools

mansched is only available via github.
To facilitate the installation of mansched, install [devtools](https://github.com/r-lib/devtools).

To install devtools, in R console, run:

``` r
install.packages("devtools")
```

### mansched install

Once devtools is installed, run the following in R console:

``` r
devtools::install_github("basilrabi/mansched")
```
