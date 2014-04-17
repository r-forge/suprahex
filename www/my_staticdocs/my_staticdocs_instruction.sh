source("http://bioconductor.org/biocLite.R")
biocLite(c("whisker","highlight","markdown","devtools","testthat"))

cd ~/Sites/SVN/R-Forge/suprahex/www/my_staticdocs
R CMD install staticdocs


cd ~/Sites/SUPERFAMILY/supraHex
R
library(staticdocs)
## load all *.r
sourceDir <- function(path) {
    for (nm in list.files(path, pattern="\\.[RrSsQq]$")) {
        source(file.path(path, nm))
    }
}
sourceDir(path="~/Sites/SVN/R-Forge/suprahex/www/my_staticdocs/staticdocs/R")
# grep "index.r" ~/Sites/SVN/R-Forge/suprahex/www/my_staticdocs/staticdocs/R

# "examples": no evaluation for examples
# "replace.examplefiles.forced": whether old example files will be forced to replaced (it does not mean they will not be evaluated: it only controls whether write out)
# "flag_demos": to tell which demo to run

## all in one go (very slow)
build_package(package="~/Sites/SVN/supraHex", base_path="~/Sites/SUPERFAMILY/supraHex")

# no examples and no demo (very fast)
build_package(package="~/Sites/SVN/supraHex", base_path="~/Sites/SUPERFAMILY/supraHex", examples=F, replace.examplefiles.forced=F, flag_demos=c(F,F,F,F))

# full examples will be regenerated
build_package(package="~/Sites/SVN/supraHex", base_path="~/Sites/SUPERFAMILY/supraHex", examples=T, replace.examplefiles.forced=T, flag_demos=c(F,F,F,F))
# only new examples will be generated (keep the old examples)
build_package(package="~/Sites/SVN/supraHex", base_path="~/Sites/SUPERFAMILY/supraHex", examples=T, replace.examplefiles.forced=F, flag_demos=c(F,F,F,F))
# all demos will be regenerated
build_package(package="~/Sites/SVN/supraHex", base_path="~/Sites/SUPERFAMILY/supraHex", examples=F, replace.examplefiles.forced=F, flag_demos=T)
# only 1st demo will be regenerated
build_package(package="~/Sites/SVN/supraHex", base_path="~/Sites/SUPERFAMILY/supraHex", examples=F, replace.examplefiles.forced=F, flag_demos=c(T,F,F,F))