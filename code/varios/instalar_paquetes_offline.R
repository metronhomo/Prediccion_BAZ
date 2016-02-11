library("miniCRAN")

# Specify list of packages to download
pkgs <- c("lubridate","ggplot2","dplyr","tidyr")

# Make list of package URLs
revolution <- c(CRAN="http://cran.revolutionanalytics.com")
pkgList <- pkgDep(pkgs, repos=revolution, type="source" )
pkgList

# Set location to store source files 
local_CRAN <- "/Volumes/JAKE/"

# Make repo for source
makeRepo(pkgs = pkgList,path = local_CRAN,type = "win.binary",repos = "http://cran.us.r-project.org",)

# install...
install.packages(pkgs, 
                 repos = local_CRAN, # do I really need "file:///"?
                 dependencies = TRUE, 
                 contrib.url = local_CRAN,
                 type  = "source" )