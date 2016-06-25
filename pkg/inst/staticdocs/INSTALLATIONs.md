## 1. R requirement

R (http://www.r-project.org) is a language and environment for statistical computing and graphics. We assume R version (>= 3.0.2) has been installed in your local machine. The current version can be installed following quick instructions below for different platforms (Windows, Mac, and Linux).

* Quick link for `Windows`: [Download R for Windows](http://cran.r-project.org/bin/windows/base).
* Quick link for `Mac`: [Download R for Mac OS X 10.6 (Snow Leopard or higher)](http://cran.r-project.org/bin/macosx).

* Below are shell command lines for R installation in Terminal (for `Linux`):

Assume you have a `ROOT (sudo)` privilege:
    
    sudo su
    wget http://www.stats.bris.ac.uk/R/src/base/R-3/R-3.2.1.tar.gz
    tar xvfz R-3.2.1.tar.gz
    cd R-3.2.1
    ./configure
    make
    make check
    make install
    R # start R

Assume you do not have a ROOT privilege and want R installation under your home directory (below `/home/hfang` should be replaced with yours):

    wget http://www.stats.bris.ac.uk/R/src/base/R-3/R-3.2.1.tar.gz
    tar xvfz R-3.2.1.tar.gz
    cd R-3.2.1
    ./configure --prefix=/home/hfang/R-3.2.1
    make
    make check
    make install
    /home/hfang/R-3.2.1/bin/R # start R

## 2. Install the package from Bioconductor and R-Forge

Notes: below are R command lines (NOT shell command lines).

To install [stable release version](http://bioconductor.org/packages/release/bioc/html/supraHex.html), run:

    source("http://bioconductor.org/biocLite.R")
    biocLite(c("supraHex","devtools"))

To install [latest development version](https://github.com/hfang-bristol/supraHex) (`highly recommended` for benefits of latest improvements), run:
    
    library(devtools)
    for(pkg in c("supraHex")){
        if(pkg %in% rownames(installed.packages())) remove.packages(pkg)
        install_github(repo=paste("hfang-bristol",pkg,sep="/"))
    }
