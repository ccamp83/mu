#' Count Unique Elements
#' 
#' @description
#' Returns the number of unique elements in a vector
#' 
#' @param x A vector of any type
#' @return Integer representing the count of unique elements
#' 
#' @examples
#' lengthunique(c(1,1,2,3,3,4)) # Returns 4
#' lengthunique(c("a","a","b","c")) # Returns 3
#' 
#' @export
lengthunique <- function(x){
    length(unique(x))
}

#' Minimum with NA Handling
#' 
#' @description
#' Wrapper for min() with na.rm=TRUE by default. Useful for ddply and graphs.
#' 
#' @param x Numeric vector
#' @return Minimum value in the vector, ignoring NA values
#' 
#' @examples
#' cc.min(c(1,2,NA,4)) # Returns 1
#' 
#' @export
cc.min <- function(x) { min(x, na.rm=T) }

#' Maximum with NA Handling
#' 
#' @description
#' Wrapper for max() with na.rm=TRUE by default. Useful for ddply and graphs.
#' 
#' @param x Numeric vector
#' @return Maximum value in the vector, ignoring NA values
#' 
#' @examples
#' cc.max(c(1,2,NA,4)) # Returns 4
#' 
#' @export
cc.max <- function(x) { max(x, na.rm=T) }

#' Locate Local Maxima
#' 
#' @description
#' Identifies local maxima in a sequence of y values corresponding to x coordinates.
#' Used in kin.extract function.
#' 
#' @param x Numeric vector of x coordinates
#' @param y Numeric vector of y values
#' @return Logical vector indicating which x values correspond to local maxima
#' 
#' @examples
#' x <- 1:5
#' y <- c(1,2,3,2,1)
#' lmax.locate(x, y) # Returns c(FALSE,FALSE,TRUE,FALSE,FALSE)
#' 
#' @export
lmax.locate <- function(x, y) {
    lmaxmin <- which(diff(sign(diff(y)))==-2)+1
    return(x %in% x[lmaxmin])
}

#' Mean with NA Handling
#' 
#' @description
#' Wrapper for mean() with na.rm=TRUE by default
#' 
#' @param x Numeric vector
#' @return Mean value of the vector, ignoring NA values
#' 
#' @examples
#' cc.mean(c(1,2,NA,4)) # Returns 2.333...
#' 
#' @export
cc.mean <- function(x) {mean(x, na.rm=T)}

#' Sort Data Frame
#' 
#' @description
#' Sorts a data frame by one or more columns specified by name or number
#' 
#' @param x A data frame
#' @param decreasing Logical; should the sort be in decreasing order?
#' @param by Column name(s) or number(s) to sort by
#' @param ... Additional arguments passed to order()
#' @return Sorted data frame
#' 
#' @examples
#' df <- data.frame(x=c(2,1,3), y=c("b","a","c"))
#' sortdataframe(df, by="x") # Sort by x column
#' sortdataframe(df, by=c("x","y")) # Sort by multiple columns
#' 
#' @export
sortdataframe <- function(x, decreasing=FALSE, by=1, ... ){
    f <- function(...) order(...,decreasing=decreasing)
    i <- do.call(f,x[by])
    x[i,,drop=FALSE]
}

#' Standard Error
#' 
#' @description
#' Calculates the standard error of a numeric vector.
#' Adapted from the sciplot package.
#' 
#' @param x Numeric vector
#' @param na.rm Logical; should NA values be removed?
#' @return Standard error value
#' 
#' @examples
#' se(c(1,2,3,4,5)) # Returns standard error
#' se(c(1,2,NA,4,5), na.rm=TRUE) # Handles NA values
#' 
#' @export
se <- function (x, na.rm = TRUE) {
    sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))
}