#' Load and Install Required R Packages
#' 
#' @description
#' This function checks if requested packages are installed, installs missing ones,
#' and loads all requested packages into the R environment. It simplifies package
#' management by handling both installation and loading in a single function call.
#' 
#' @param wants A character vector containing the names of R packages to load
#' 
#' @return Invisibly returns the results of loading each package via lapply
#' 
#' @details
#' The function performs three main steps:
#' 1. Checks which requested packages are already installed
#' 2. Installs any missing packages from CRAN
#' 3. Loads all requested packages using require()
#' 
#' Packages are installed with dependencies from the cloud.r-project.org repository.
#' 
#' @examples
#' # Load a single package
#' mu.library("dplyr")
#' 
#' # Load multiple packages
#' mu.library(c("ggplot2", "tidyr", "readr"))
#' 
#' @export
mu.library <- function(wants)
{
  # what we have
  has <- wants %in% rownames(installed.packages())
  # go get what we want that we don't have 
  if(any(!has)) install.packages(wants[!has], dependencies = T, repos = "https://cloud.r-project.org/")
  # load 'em all
  lapply(wants, require, character.only=T)
}