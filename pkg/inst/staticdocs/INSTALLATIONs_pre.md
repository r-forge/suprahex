## 1. R requirement

R (http://www.r-project.org) is a language and environment for statistical computing and graphics. We assume R (version 3.1.0) has been installed in your local machine. The current version 3.1.0 can be installed following quick instructions below for different platforms (Windows, Mac, and Linux).

* Quick link for `Windows`: [Download R for Windows](http://www.stats.bris.ac.uk/R/bin/windows/base/R-3.1.0-win.exe).
* Quick link for `Mac`: [Download R for Mac OS X 10.6 (Snow Leopard or higher)](http://cran.r-project.org/bin/macosx/R-3.1.0-snowleopard.pkg).

* Below are shell command lines for R installation in Terminal (for `Linux`):

Assume you have a `ROOT (sudo)` privilege:
    
    sudo su
    wget http://www.stats.bris.ac.uk/R/src/base/R-3/R-3.1.0.tar.gz
    tar xvfz R-3.1.0.tar.gz
    cd R-3.1.0
    ./configure
    make
    make check
    make install
    R # start R

Assume you do not have a ROOT privilege and want R installation under your home directory (below `/home/hfang` should be replaced with yours):

    wget http://www.stats.bris.ac.uk/R/src/base/R-3/R-3.1.0.tar.gz
    tar xvfz R-3.1.0.tar.gz
    cd R-3.1.0
    ./configure --prefix=/home/hfang/R-3.1.0
    make
    make check
    make install
    /home/hfang/R-3.1.0/bin/R # start R

## 2. Install the package from Bioconductor and R-Forge

Notes: below are R command lines (NOT shell command lines).

To install [stable release version](http://bioconductor.org/packages/release/bioc/html/supraHex.html), run:

    source("http://bioconductor.org/biocLite.R")
    biocLite("supraHex")

To install [latest development version](http://bioconductor.org/packages/devel/bioc/html/supraHex.html) (`highly recommended` for benefits of latest improvements), run:

    if(!require(hexbin)) install.packages("hexbin",repos="http://www.stats.bris.ac.uk/R")
    if(!require(ape)) install.packages("ape",repos="http://www.stats.bris.ac.uk/R")
    install.packages("supraHex",repos="http://R-Forge.R-project.org", type="source")
