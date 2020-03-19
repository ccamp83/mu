#' @export
string.split <- function(string, sep, position){
  if(!is.character(string)) {string <- as.character(string)}
  x <- strsplit(string, sep)
  if(is.numeric(position))
  {
    output <- unlist(lapply(x, "[[", position))
  } else
  {
    output <- unlist(lapply(x, tail, 1))
  }
  
  return(output)
}
