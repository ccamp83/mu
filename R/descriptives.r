#' @export
# Descriptive
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
    round(apply(x, 2, Skew, na.rm = T), 3),
    round(apply(x, 2, Kurt, na.rm = T, method = 2), 3)
  )
  output <- as.data.frame(t(temp))
  
  colnames(output) <- c("N","Missing","Min","Max","Mean","SD","Median","IQR","Skewness","Kurtosis")

  return(output)
}