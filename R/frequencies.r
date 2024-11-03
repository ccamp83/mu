#' Calculate Frequency Tables with Statistics
#' 
#' @description
#' Generates frequency tables for all columns in a dataset, including valid percentages,
#' cumulative percentages, and missing value statistics.
#' 
#' @param data A data frame or data structure containing the variables to analyze
#' 
#' @details
#' For each column in the input data, the function calculates and displays:
#' * Frequency counts
#' * Percentages of total observations
#' * Valid percentages (excluding missing values)
#' * Cumulative percentages
#' * Summary of valid and missing observations
#' 
#' The output for each variable includes:
#' * A frequency table with the above statistics
#' * Count and percentage of valid observations
#' * Count and percentage of missing observations
#' * Total number of observations
#' 
#' @return
#' Prints formatted frequency tables for each column in the dataset.
#' Each table includes:
#' \item{Var1}{The unique values in the column}
#' \item{Freq}{Frequency counts}
#' \item{Percent}{Percentage of total observations}
#' \item{ValidPercent}{Percentage excluding missing values}
#' \item{CumulativePercent}{Cumulative valid percentages}
#' 
#' @examples
#' data <- data.frame(
#'   gender = c("M", "F", "M", NA, "F"),
#'   age = c(25, 30, NA, 40, 35)
#' )
#' frequencies(data)
#' 
#' @export
frequencies <- function(data)
{
  for(col in 1:length(data))
  {
    x <- data[names(data)[col]]
    # tot observations
    x.tot <- nrow(x) 
    # tot valid observations
    x.val <- sum(!is.na(x)) 
    # frequency table
    xout <- as.data.frame(table(x)) 
    # add other statistics to frequency table
    xout2 <- within(xout, { 
      ValidPercent <- round(prop.table(Freq)*100, 1)
      CumulativePercent <- cumsum(ValidPercent)
      Percent <- round(Freq/x.tot*100, 1)
    }
    )
    
    # prepare the output
    output <- xout2[c(1,2,3,5,4)] # reorder the columns
    # print the output
    cat('\n-----------------------------------------------\n')
    cat('\nName: ', names(x), "\n\n", sep='')
    print(output)
    cat("\nValid Obs:\t\t", x.val, " (",round(x.val/x.tot*100, 1),"%) \n", sep='')
    cat("Missing Obs:\t", x.tot-x.val, " (",round((x.tot-x.val)/x.tot*100, 1),"%) \n", sep='')
    cat("Total Obs:\t\t", x.tot, "\n", sep='')
  }
}