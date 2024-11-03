#' Split a String and Extract Elements
#' 
#' @description
#' Splits a string based on a separator and returns either a specific position
#' or the last element of the resulting vector.
#' 
#' @param string A character vector (or object that can be coerced to character)
#' @param sep The separator to use for splitting the string
#' @param position Numeric value indicating which element to extract after splitting.
#'   If not provided or non-numeric, returns the last element
#' 
#' @return A character vector containing the extracted elements
#' 
#' @details
#' The function first ensures the input is a character vector, then splits it using
#' the provided separator. If a numeric position is specified, it extracts that element
#' from each split result. If no position is specified or the position is non-numeric,
#' it returns the last element of each split result.
#' 
#' @examples
#' string.split("hello.world", ".", 1)  # Returns "hello"
#' string.split("hello.world", ".", 2)  # Returns "world"
#' string.split("hello.world.test", ".") # Returns "test"
#' 
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
