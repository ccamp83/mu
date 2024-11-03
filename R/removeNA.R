#' Replace NA Values with Mean
#' 
#' @description
#' Replaces NA values in a numeric vector with the mean of non-NA values. 
#' Issues a warning if the proportion of NA values exceeds 5%.
#' 
#' @param x A numeric vector containing NA values
#' 
#' @return A numeric vector with NA values replaced by the mean
#' 
#' @details
#' The function:
#' * Calculates the proportion of NA values in the input vector
#' * Issues a warning if more than 5% of values are NA
#' * Replaces all NA values with the mean of non-NA values
#' 
#' @examples
#' x <- c(1, 2, NA, 4, NA, 6)
#' removeNA(x)  # Returns c(1, 2, 3.25, 4, 3.25, 6)
#' 
#' @export
removeNA <- function(x) { 
  # calculate proportion of NA
  numNA <- length(x[is.na(x)]) / length(x)
  
  if(numNA > .05)
    warning(paste0("NAs are ", round(numNA*100, 2), "% of the data.\n"))
  
  x[is.na(x)] <- mean(x, na.rm=T)
  return(x) 
}
