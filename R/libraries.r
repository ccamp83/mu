#' Install and Load Required Packages for ShapeLab
#' 
#' @description
#' This function manages package dependencies for the ShapeLab package. It checks for required packages,
#' installs missing ones, optionally updates existing packages, and loads them into the R environment.
#' 
#' @param load Logical. If TRUE (default), loads all required packages after installation
#' @param update Logical. If TRUE, updates all installed packages to their newest versions (default: FALSE)
#' 
#' @details
#' The function performs the following steps:
#' 1. Checks currently installed packages
#' 2. Compares against required package list
#' 3. Installs any missing packages from CRAN
#' 4. Optionally updates all packages
#' 5. Optionally loads all required packages
#' 
#' Required packages include:
#' * Data manipulation: plyr
#' * Visualization: ggplot2, cowplot, grid, grDevices, lattice
#' * Statistical analysis: lme4, car, AICcmodavg, fpc
#' * Mixed models: pbkrtest
#' * Utilities: Hmisc, fields, gridExtra, phia, gtools
#' * Development: roxygen2
#' * Matrix operations: clusterGeneration
#' * Statistical tools: DescTools
#' * Parallel processing: parallel
#' * Visualization helpers: heplots
#' 
#' @return No return value, called for side effects
#' 
#' @examples
#' # Load packages only
#' libraries(load = TRUE, update = FALSE)
#' 
#' # Update and load packages
#' libraries(load = TRUE, update = TRUE)
#' 
#' # Install/update packages without loading
#' libraries(load = FALSE, update = TRUE)
#' 
#' @note
#' Packages are installed from the CRAN mirror "https://cloud.r-project.org/"
#' All dependencies are automatically installed with each package
#' 
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
