#' Install shapelab package
#' @export
shapelab.install <- function(address = "~/Dropbox/RtG Simulator/analysis r/shapelab/", shapelab.type='source')
{
  install.packages(address, type=shapelab.type, repos=NULL)
}