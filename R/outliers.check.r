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
