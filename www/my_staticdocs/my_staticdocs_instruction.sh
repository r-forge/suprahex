source("http://bioconductor.org/biocLite.R")
biocLite(c("whisker","highlight","markdown","devtools"))

cd ~/Sites/SVN/R-Forge/suprahex/www/my_staticdocs
R CMD install staticdocs
library(staticdocs)

## load all *.r
sourceDir <- function(path) {
    for (nm in list.files(path, pattern="\\.[RrSsQq]$")) {
        source(file.path(path, nm))
    }
}
sourceDir(path="~/Sites/SVN/R-Forge/suprahex/www/my_staticdocs/staticdocs/R")
# grep "index.r" ~/Sites/SVN/R-Forge/suprahex/www/my_staticdocs/staticdocs/R

## all in one go
build_package(package="~/Sites/SVN/supraHex", base_path="~/Sites/SUPERFAMILY/supraHex")

# Disable 'flag_demos' if no update
build_package(package="~/Sites/SVN/supraHex", base_path="~/Sites/SUPERFAMILY/supraHex", examples=T, flag_demos=F)
build_package(package="~/Sites/SVN/supraHex", base_path="~/Sites/SUPERFAMILY/supraHex", examples=F, flag_demos=F)

