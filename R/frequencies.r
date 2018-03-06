#' @export
# Frequencies
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