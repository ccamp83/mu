#' Load and update all the required libraries of the shapelab package
#' @param update if TRUE, the function also updates the libraries to newest version (FALSE by default)
#' @export
libraries <- function(update=F)
{
  # retrieve the list of installed packages using installed.packages()
  packages <- levels(as.data.frame(installed.packages())$Package)
  
  # list of the required packages
  reqPackages <- c(
    "Hmisc",
    "plyr",
    "ggplot2",
    "lme4",
    "RCurl", # ubuntu 14.04 requires "sudo apt-get install libcurl4-gnutls-dev" to install this package
    "fields",
    "gridExtra",
    "grid",
    "rgl", # ubuntu 14.04 requires "sudo apt-get install libX11-dev freeglut3 freeglut3-dev libxml2-dev" to install this package
    "grDevices",
    "car",
    "phia",
    "gtools",
    "lattice",
    "roxygen2",
    "AICcmodavg",
    "reshape2",
    "fpc",
    "pbkrtest",
    "clusterGeneration", # rcorrmatrix
    "DescTools",
    "parallel",
    "mvtnorm",
    "heplots"
  )
  
  # check which of the required libraries have to be installed
  toBeInstalled <- reqPackages[reqPackages %in% packages==F]
  
  # install missing packages (if existing)
  if(length(toBeInstalled)!=0)
  {
    install.packages(toBeInstalled)
  }
  
  # update packages if requested by user
  if(update)
  {
    update.packages()
  }
  
  # load the required libraries
  for(i in 1:length(reqPackages))
  {
    library(reqPackages[i], character.only=T)
  }
}