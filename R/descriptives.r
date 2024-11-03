#' Calculate Descriptive Statistics
#'
#' @description
#' Calculates comprehensive descriptive statistics for numeric columns in a dataset,
#' including measures of central tendency, dispersion, and distribution shape.
#'
#' @param data A data frame or matrix containing numeric columns to analyze
#'
#' @return A data frame where each row represents a numeric variable from the input data,
#' and columns contain the following statistics:
#'   \itemize{
#'     \item N: Number of non-missing observations
#'     \item Missing: Number of missing values
#'     \item Min: Minimum value
#'     \item Max: Maximum value
#'     \item Mean: Arithmetic mean
#'     \item SD: Standard deviation
#'     \item Median: Median value
#'     \item IQR: Interquartile range (using type 3 calculation)
#'     \item Skewness: Skewness coefficient
#'     \item Kurtosis: Kurtosis coefficient (using method 2)
#'   }
#'
#' @details
#' The function automatically identifies and processes only numeric columns in the input dataset.
#' All statistics are calculated excluding NA values. Results for mean, SD, median, IQR,
#' skewness, and kurtosis are rounded for readability. Skewness and kurtosis calculations
#' use functions from the DescTools package.
#'
#' @examples
#' # Using with iris dataset
#' result <- descriptives(iris)
#'
#' # Using with specific numeric columns
#' data <- data.frame(
#'   age = c(25, 30, 35, NA, 40),
#'   score = c(85, 90, 88, 92, 95)
#' )
#' result <- descriptives(data)
#'
#' @export
descriptives <- function(data)
{
  nums <- sapply(data, is.numeric) # select only the columns that are numeric
  stopifnot(all(sapply(data[,nums], is.numeric)))
  if(length(nums) > 1)
  {
    x <- data[, nums] # subset the data keeping only the numeric columns
  } else
  {
    x <- data
  }

  temp <- rbind(
    apply(!is.na(x), 2, sum),
    nrow(x) - apply(!is.na(x), 2, sum),
    apply(x, 2, min, na.rm=T),
    apply(x, 2, max, na.rm=T),
    round(apply(x, 2, mean, na.rm=T), 2),
    round(apply(x, 2, sd, na.rm=T), 2),
    round(apply(x, 2, median, na.rm=T), 2),
    round(apply(x, 2, IQR, na.rm=T, type=3), 3),
    round(apply(x, 2, DescTools::Skew, na.rm = T), 3),
    round(apply(x, 2, DescTools::Kurt, na.rm = T, method = 2), 3)
  )
  output <- as.data.frame(t(temp))

  colnames(output) <- c("N","Missing","Min","Max","Mean","SD","Median","IQR","Skewness","Kurtosis")

  return(output)
}
