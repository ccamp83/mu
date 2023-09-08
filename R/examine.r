#'@export
# EXAMINE
examine <- function(data, x=NULL, all.results=FALSE, ...)
{
  # if data is a vector, convert to data.frame
  if(is.vector(data))
  {
    data <- data.frame(data)
  }
  
  # check which arguments have been declared
  arguments <- as.list(match.call())
  
  if("x" %in% names(arguments))
  {
    data.num = data.frame(eval(arguments$x, data))
    names(data.num) <- toString(arguments$x)
  } else
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
  }
  
  for(col in 1:length(data.num))
  {
    x <- data.num[names(data.num)[col]]
    
    Name <- names(x)
    Ntot <- nrow(x)
    Nval <- sum(!is.na(x))
    Nmiss <- Ntot - Nval
    PCNTval <- Nval/Ntot*100
    PCNTmiss <- Nmiss/Ntot*100
    
    x <- as.numeric(t(x))
    
    temp <- data.frame(
      "Mean" = round(mean(x, na.rm=T), 2),
      "Variance" = round(var(x, na.rm=T), 3),
      "Std.Deviation" = round(sd(x, na.rm=T), 3),
      "Std.Error" = round(sd(x, na.rm=T)/sqrt(Nval), 3),
      "Minimum" = min(x, na.rm=T),
      "Maximum" = max(x, na.rm=T),
      "TrimmedMean" = round(mean(x, trim=0.25, na.rm=T), 2),
      "IQR" = IQR(x, na.rm=T, type=3),
      "Skewness" = round(DescTools::Skew(x, na.rm=T), 3),
      "Kurtosis" = round(DescTools::Kurt(x, na.rm=T, method=2), 3),
      "Median" = round(median(x, na.rm=T), 3)
    )
    
    temp <- within(temp, {
      "95CIup" <- round(Mean+1.96*Std.Error, 2)
      "95CIlow" <- round(Mean-1.96*Std.Error, 2)
      Range <- Maximum-Minimum
    })
    
    SES <- round(sqrt( (6*Nval*(Nval-1)) / ((Nval-2)*(Nval+1)*(Nval+3)) ), 3)
    SEK <- round(2*SES*sqrt( (Nval^2 - 1) / ((Nval-3)*(Nval+5)) ), 3)
    
    if(all.results)
    {
      output <- as.data.frame(t(temp[c(1,13,14,7,11,2,3,5,6,12,8,9,10)]))
      names(output) <- "Statistic"
      output$"Std.Error" <- c(temp[4], rep('', 10), SES, SEK)
      output$'' <- c(rep(NA, 11), ifelse(abs(temp[9]/SES) > 2, ' *', ''), ifelse(abs(temp[10]/SEK) > 2, ' *', ''))
    } else
    {
      output <- as.data.frame(t(temp[c(1,7,11,3,9,10)]))
      names(output) <- "Statistic"
      output$"Std.Error" <- c(temp[4], rep('', 3), SES, SEK)
      output$'' <- c(rep("", 4), ifelse(abs(temp[9]/SES) > 2, ' *', ''), ifelse(abs(temp[10]/SEK) > 2, ' *', ''))
    }
    cat('\n------------------------------------\n')
    cat('\nName: ', Name, "\n", sep='')
    cat("\nValid Obs:\t\t", Nval, " (",round(Nval/Ntot*100, 1),"%) \n", sep='')
    cat("Missing Obs:\t", Nmiss, " (",round(Nmiss/Ntot*100, 1),"%) \n", sep='')
    cat("Total Obs:\t\t", Ntot, "\n\n", sep='')
    print(output, na.print='', quote=F)
  }
}