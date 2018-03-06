#' Check the data file and provide fixes if available
#' @param dataset an object of the type data.frame
#' @param refreshRate the refresh rate used during the motion capture (in hertz)
#' @param time.unit the unit of measurement in which time is expressed in the 'time' column of the dataset given to the function. 1 = seconds, 10 = deciseconds, 100 = centiseconds, 1000 = milliseconds, ...
#' @examples
#' libraries()
#' 
#' ### restoring missing columns
#' 
#' data(rtgData_bad)
#' 
#' head(rtgData_bad)
#' rtgChecked <- data.check(rtgData_bad)
#' test_subject
#' # subjName is given without quotes
#' head(rtgChecked)
#' 
#' detach(rtgData_bad)
#' 
#' ### time.unit
#' 
#' data(rtgData)
#' rtgData <- data.check(rtgData) 
#' # rtgData has time in milliseconds so data.check goes through smoothly
#' head(rtgData)
#' 
#' # instead, should the dataset have time in seconds
#' # the function will return frameT as a vector of NAs
#' data(rtgData) # reload dataset
#' rtgData$time <- rtgData$time / 1000 # change time to seconds
#' rtgData <- data.check(rtgData)
#' rtgData$frameT # always check that frameT looks good
#' 
#' # use time.unit to fix it
#' data(rtgData) # reload dataset
#' rtgData$time <- rtgData$time / 1000 # change time to seconds
#' rtgData <- data.check(rtgData, time.unit = 1)
#' rtgData$frameT
#' 
#' detach(rtgData)
#' @export
data.check <- function(dataset, refreshRate = 85, time.unit = 1000, ...) 
{
  indCols <- c("indexXraw","indexYraw","indexZraw")
  thuCols <- c("thumbXraw","thumbYraw","thumbZraw")
  
  if(!all(c(is.element("trialN", names(dataset)), is.element(indCols, names(dataset)))) ||
     !all(c(is.element("trialN", names(dataset)), is.element(thuCols, names(dataset)))))
  {
    stop("data.check expected a column called 'trialN' and at least one of the following triples: \n
         indexXraw, indexYraw, indexZraw \n         thumbXraw, thumbYraw, thumbZraw \n
         but could not find them in the dataset \n")
  } else
  {
    reqCols <- c("subjName", "frameN", "time", "fingersOccluded", 
                 "framesOccluded", "globalTime","frameT")
    missingCols <- reqCols[reqCols %in% names(dataset) == F]
    if (length(missingCols) > 0) {
      print("The following columns do not exist:")
      print(sprintf("%s", missingCols))
      print("Fixing...")
      if ("subjName" %in% missingCols) {
        print("Please input subject name:")
        name <- readline()
        dataset$subjName <- sprintf("%s", name)
      }
      if ("frameN" %in% missingCols) {
        dataset <- cc.frameN(dataset)
      }
      if ("time" %in% missingCols) {
        cat("'time' column is absent and will be replaced.\n")
        dataset <- cc.time(dataset, refreshRate)
      }
      if ("fingersOccluded" %in% missingCols) {
        dataset <- cc.fingersOccluded(dataset)
      }
      if ("framesOccluded" %in% missingCols) {
        dataset <- cc.framesOccluded(dataset)
      }
      
      if(any(ddply(dataset, .(trialN), summarise,
                   any(fingersOccluded) - any(framesOccluded))[2] < 0)) {
        print("fingersOccluded does not look right: fixing...")
        dataset <- cc.fingersOccluded(dataset)
      }
      
      if(any(ddply(dataset, .(trialN), summarise,
                   any(fingersOccluded) - any(framesOccluded))[2] > 0)) {
        print("framesOccluded does not look right: fixing...")
        dataset <- cc.framesOccluded(dataset)
      }
      
      if ("globalTime" %in% missingCols) {
        isGlobalTimeMissing <- T
        dataset <- cc.globalTime(dataset)
      } else
      {
        isGlobalTimeMissing <- F
      }
      
      if ("frameT" %in% missingCols) {
        if(isGlobalTimeMissing)
        {
          dataset <- ddply(dataset, .(trialN), mutate, 
                           frameT = c(NA, diff(time)))
        } else
        {
          dataset <- ddply(dataset, .(trialN), mutate, 
                           frameT = c(NA, diff(globalTime)))
        }
        theorframeT <- time.unit/refreshRate # in millisecs
        # we establish that frameT can be accepted only if equal to the theoretical frameT or multiple of it (only greater cases)
        # we remove all values that are smaller than 90% of the theoretical frameT
        if(isGlobalTimeMissing)
        {
          dataset$frameT <- with(dataset, ifelse(frameT < theorframeT*.9, theorframeT, frameT))
          warning(paste("globalTime missing: incorrect frameT will be replaced by the theoretical frame time (", theorframeT, ")", sep=''))
        } else
        {
          dataset$frameT <- with(dataset, ifelse(frameT < theorframeT*.9, NA, frameT))
        }
        
        if(sum(is.na(dataset$frameT)) == length(dataset$frameT))
          warning("frameT is a vector of NAs. Check that time and refresh rate have the same unit of measurement")
      }
      
      print("Database fixed successfully.")
    }
    else {
      if(any(ddply(dataset, .(trialN), summarise,
                   any(fingersOccluded) - any(framesOccluded))[2] < 0)) {
        print("checking 'fingersOccluded' column ...")
        dataset <- cc.fingersOccluded(dataset)
      }
      
      if(any(ddply(dataset, .(trialN), summarise,
                   any(fingersOccluded) - any(framesOccluded))[2] > 0)) {
        print("checking 'framesOccluded' column ...")
        dataset <- cc.framesOccluded(dataset)
      }
      
      print("Database looks good.")
    }
    return(dataset)
  }
}
