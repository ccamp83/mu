#' @export
removeNA <- function(x) { 
  # calculate proportion of NA
  numNA <- length(x[is.na(x)]) / length(x)
  
  if(numNA > .05)
    warning(paste0("NAs are ", round(numNA*100, 2), "% of the data.\n"))
  
  x[is.na(x)] <- mean(x, na.rm=T)
  return(x) 
}
