#' Generate Normal Probability Plots and Detrended Normal Plots
#'
#' @description
#' Creates normal probability plots (NNPLOT) and detrended normal probability plots
#' for each numeric variable in the input dataset. These plots are useful for assessing
#' normality of variables and identifying potential deviations from normality.
#'
#' @param data A data frame containing numeric variables to be analyzed
#'
#' @details
#' For each numeric variable, the function creates two plots:
#' 1. Normal Probability Plot (NNPLOT): Plots observed values against their expected
#'    normal values, with a reference line showing perfect normality
#' 2. Detrended Normal Plot: Shows deviations from normality by plotting residuals
#'    from the normal probability line against observed values
#'
#' The function automatically:
#' * Identifies and processes only numeric columns
#' * Calculates normal scores and z-scores
#' * Creates side-by-side plots for each variable
#' * Resets the plotting parameters after completion
#'
#' @return
#' No return value. Generates plots directly to the current graphics device.
#'
#' @examples
#' # Create plots for all numeric variables in mtcars
#' npplots(mtcars)
#'
#' # Create plots for a single numeric variable
#' npplots(data.frame(x = rnorm(100)))
#'
#' @export
npplots <- function(data)
{
  nums <- sapply(data, is.numeric) # select only the columns that are numeric
  stopifnot(all(sapply(data[,nums], is.numeric)))
  if(length(nums) > 1)
  {
    data.num <- data[, nums] # subset the data keeping only the numeric columns
  } else
  {
    data.num <- data
  }

  for(col in 1:length(data.num))
  {
    x <- data.num[names(data.num)[col]]

    name <- names(x)
    x <- as.numeric(t(x))
    nscore <- qqnorm(x, plot.it = FALSE)$x
    trn.table <- data.frame(x=sort(x),nscore=sort(nscore),zscore=sort(scale(x)))
    trn.table <- ddply(trn.table, .(x), mutate,
                       enorm = median(nscore),
                       devnorm = zscore - enorm)

    par(mfrow=c(1,2))
    plot(enorm ~ x, data=trn.table,
         xlab="Observed Value", ylab="Expected Normal",
         main=paste("NNPLOT of ", name))
    abline(lm(scale(x) ~ x))

    plot(devnorm ~ x, data=trn.table,
         xlab="Observed Value", ylab="Dev from Normal",
         main=paste("Detrended NNPLOT of ", name))
    abline(a=0, b=0)

    par(mfrow=c(1,1))
  }
}
