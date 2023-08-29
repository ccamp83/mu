#' Load and update all the required libraries of the shapelab package
#' @param load should the packages be loaded? (default to TRUE)
#' @param update if TRUE, the function also updates the libraries to newest version (FALSE by default)
#' @export
libraries <- function(load=T, update=F)
{
  # retrieve the list of installed packages using installed.packages()
  packages <- rownames(installed.packages())
  
  # list of the required packages
  reqPackages <- c(
    "Hmisc",
    "plyr",
    "ggplot2",
    "cowplot",
    "lme4",
    "fields",
    "gridExtra",
    "grid",
    "grDevices",
    "car",
    "phia",
    "gtools",
    "lattice",
    "roxygen2",
    "AICcmodavg",
    "fpc",
    "pbkrtest",
    "clusterGeneration", # rcorrmatrix
    "DescTools",
    "parallel",
    "heplots"
  )
  
  # check which of the required libraries have to be installed
  toBeInstalled <- reqPackages[reqPackages %in% packages==F]
  
  # install missing packages (if existing)
  if(length(toBeInstalled)!=0)
  {
    install.packages(toBeInstalled, repos = "https://cloud.r-project.org/", dependencies = T)
  }
  
  # update packages if requested by user
  if(update)
  {
    update.packages(repos = "https://cloud.r-project.org/")
  }
  
  if(load)
  {
    # load the required libraries
    for(i in 1:length(reqPackages))
    {
      library(reqPackages[i], character.only=T)
    }
  }
}