#' Check and Clean Outliers in a Numeric Vector
#' 
#' @description
#' Identifies and optionally replaces outliers in a numeric vector using the Interquartile Range (IQR) 
#' method. Outliers are defined as values that fall outside Q1 - k*IQR or Q3 + k*IQR, where k is a 
#' user-defined threshold.
#' 
#' @param raw.vector A numeric vector to check for outliers
#' @param sd.thresh Numeric value specifying the threshold multiplier for the IQR (default = 1.5)
#' @param fun.replace Character string specifying the replacement method: either "mean" or "median" 
#'   (default = "mean")
#' @param logical Logical value. If TRUE, returns a factor indicating outlier status (1 = outlier, 
#'   0 = not outlier). If FALSE, returns the cleaned vector (default = FALSE)
#' 
#' @return
#' If logical = FALSE (default), returns a numeric vector with outliers replaced by either the mean 
#' or median of the original vector.
#' If logical = TRUE, returns a factor vector where 1 indicates outliers and 0 indicates non-outliers.
#'
#' @details
#' The function uses the following method to identify outliers:
#' * Calculates Q1 (25th percentile), Q3 (75th percentile), and IQR
#' * Identifies values outside: [Q1 - sd.thresh*IQR, Q3 + sd.thresh*IQR]
#' * Replaces outliers with either mean or median of the original vector
#' 
#' @examples
#' # Clean outliers using mean replacement
#' x <- c(1, 2, 3, 100, 2, 3, 4, -50)
#' outliers.check(x)
#' 
#' # Get logical vector of outlier positions
#' outliers.check(x, logical = TRUE)
#' 
#' # Clean outliers using median with different threshold
#' outliers.check(x, sd.thresh = 2, fun.replace = "median")
#'
#' @export
outliers.check <- function(raw.vector, sd.thresh=1.5, fun.replace = "mean", logical=F)
{
  if(fun.replace=="median")
    cleaned.vector <- ifelse(raw.vector > quantile(raw.vector, na.rm = T)[4] + sd.thresh*IQR(raw.vector, na.rm = T) | raw.vector < quantile(raw.vector, na.rm = T)[2] - sd.thresh*IQR(raw.vector, na.rm = T), median(raw.vector, na.rm = T), raw.vector)
  else if(fun.replace=="mean")
    cleaned.vector <- ifelse(raw.vector > quantile(raw.vector, na.rm = T)[4] + sd.thresh*IQR(raw.vector, na.rm = T) | raw.vector < quantile(raw.vector, na.rm = T)[2] - sd.thresh*IQR(raw.vector, na.rm = T), mean(raw.vector, na.rm = T), raw.vector)
  else
    stop('Replacement function can be "median" or "mean"')
  
  is.outlier <- ifelse(raw.vector > quantile(raw.vector, na.rm = T)[4] + sd.thresh*IQR(raw.vector, na.rm = T) | raw.vector < quantile(raw.vector, na.rm = T)[2] - sd.thresh*IQR(raw.vector, na.rm = T), 1, 0)
  
  if(!logical) {
    output <- cleaned.vector
  } else {
    output <- factor(is.outlier)
  }
  return(output)
}
