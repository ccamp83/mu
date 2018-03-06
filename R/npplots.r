#'@export
## NPPLOT and Detrended NPPLOT
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