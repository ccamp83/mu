#' @export
load.libraries <- function(libList) 
{
  # what we have
  has   <- libList %in% rownames(installed.packages())
  # go get what we want that we don't have 
  if(any(!has)) install.packages(libList[!has])
  # load 'em all
  lapply(libList, require, character.only=T)
}