#' Load libraries and download them if not existing
#' @param wants a vector of strings with the names of the packages to load
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