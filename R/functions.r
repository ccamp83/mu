# Summary:
# 1. lengthunique(x)
# 2. cc.smooth
# 3. cc.derive
# 4. cc.rescale
# 5. cc.trialN
# 6. cc.frameN
# 7. cc.fingersOccluded
# 8. cc.framesOccluded
# 9. cc.time
# 10. cc.min / cc.max
# 11. cc.globalTime
# 12. cc.smooth.repair
# 13. cc.GA.smooth.repair
# 14. lmax.locate
# 15. cc.mean
# 16. sort.data.frame
# 17. se

####################################################################################	1.
#' @export
lengthunique <- function(x){
	length(unique(x))
}

####################################################################################	2. 3. 4.
# functions to smooth, derive and rescale kinematics
# they exploit built-in r functions, to be applied with ddply

#' smooth data points.
#' @param x: predictor
#' @param  y: variable to be smoothed
#' @param  l: lambda - smoothing parameter (default to .2)
#' @export
cc.smooth <- function(x,y,l=.2){
  fit <- sreg(x, y, lambda=l)
  return(predict(fit, x))
}

#' smooth then calculate derivatives of a given variable with respect to a specific predictor. Inputs:
#' @param x: predictor
#' @param y: variable to be smoothed
#' @param l: lambda - smoothing parameter (default to .2)
#' @param d: order of derivative
#' @export
cc.derive <- function(x,y,l=.2,d){predict(sreg(x,y,lambda=l), deriv=d)[match(x,sreg(x,y, lambda=l)$x)]}

#' rescale the values of a vector within a specified range. Inputs:
#' @param x: vector to be rescaled
#' @param a: range minimum
#' @param b: range maximum
#' @export
cc.rescale <- function(x,a,b) { 
  rg <- c(a,b)
  rescale(x, to = rg, from = range(x, na.rm = TRUE)) }

####################################################################################  5. 6. 7.
# functions to fix dataset

# 5. trialN from 1 to N
#' @export
cc.trialN <- function(dataset) 
{ 
  if("trialN"%in%names(dataset))
  {
    if(0 %in% unique(dataset$trialN))
    {
      dataset$trialN <- dataset$trialN+1 
      print("TrialN fixed. Trials sequence now starts from 1.")
    }else
    {
      print("Trials sequence already starts from 1.")
    }
  }else
  {
    print("No 'trialN' column found in dataset.")
  }
  return(dataset)
}

# 6. frameN generator: frames counter
#' @export
cc.frameN <- function(dataset)
{
  dataset <- ddply(dataset, .(trialN), mutate, frameN=seq(1:length(trialN)))
  return(dataset)
}

# 7. fingersOccluded generator: flag the frames where either of the two fingers is invisible
#' @export
cc.fingersOccluded <- function(dataset)
{
  datatemp <- ddply(dataset, .(trialN), mutate, 
                    indexVisibility=c(-999,abs(diff(indexXraw))),
                    thumbVisibility=c(-999,abs(diff(thumbXraw))),
                    indexXraw = ifelse(indexVisibility==0.000000,NA,indexXraw),
                    indexYraw = ifelse(indexVisibility==0.000000,NA,indexYraw),
                    indexZraw = ifelse(indexVisibility==0.000000,NA,indexZraw),
                    thumbXraw = ifelse(thumbVisibility==0.000000,NA,thumbXraw),
                    thumbYraw = ifelse(thumbVisibility==0.000000,NA,thumbYraw),
                    thumbZraw = ifelse(thumbVisibility==0.000000,NA,thumbZraw),
                    fingersOccluded=ifelse(indexVisibility*thumbVisibility==0.000000,1,0) # if the increment is exactly zero, two frames have exactly the same coordinate, which means that the marker was not recorded
  )
  # update fingers raw coords
  dataset$indexXraw <- datatemp$indexXraw
  dataset$indexYraw <- datatemp$indexYraw
  dataset$indexZraw <- datatemp$indexZraw
  dataset$thumbXraw <- datatemp$thumbXraw
  dataset$thumbYraw <- datatemp$thumbYraw
  dataset$thumbZraw <- datatemp$thumbZraw
  dataset$fingersOccluded <- ifelse(is.na(datatemp$fingersOccluded), 1,  datatemp$fingersOccluded) # drop index- and thumbVisibility and include only fingersOccluded
  return(dataset)
}

# 8. framesOccluded generator: incremental counter of occluded frames within each trial
#' @export
cc.framesOccluded <- function(dataset)
{
  dataset <- ddply(dataset, .(trialN), mutate, framesOccluded = fingersOccluded * unlist(lapply(rle(fingersOccluded)$lengths, seq_len)))
  return(dataset)
}

# 9. time generator: multiplies each frame by a constant equal to the (nominal) refresh rate of the screen (Warning: this is just an artificial fix, actual refresh rate is variable - always make sure that actual time steps are in the original output file)
#' @export
cc.time <- function(dataset, refreshRate = 85)
{
  # assign refreshRate to global environment for looping (temporarily)
  assign("refreshRate", refreshRate, envir = .GlobalEnv)
  dataset <- ddply(dataset, .(trialN), mutate, time = frameN * 1000/refreshRate) # in milliseconds
  # remove refreshRate from global environment
  remove(refreshRate, envir = .GlobalEnv)
  return(dataset)
}

####################################################################################  10.

# min and max functions with na.rm = TRUE by default (for usage in ddply and graphs)
#' @export
cc.min <- function(x) { min(x, na.rm=T) }
#' @export
cc.max <- function(x) { max(x, na.rm=T) }

####################################################################################  11.

# recovery global time
#' @export
cc.globalTime <- function(dataset)
{
  millisecPerFrame = median(diff(dataset$time))
  assign("millisecPerFrame", millisecPerFrame, envir = .GlobalEnv)
  
  dataset <- ddply(dataset, .(trialN), mutate, 
                   globalTime = frameN*millisecPerFrame
                   )
  remove(millisecPerFrame, envir = .GlobalEnv)
  
  return(dataset)
}

####################################################################################  12.

#' @export
cc.smooth.repair <- function(x, y.raw, lam = .0005, maxFrames = 18, fingersOccluded, framesOccluded)
{
  y.raw[which(fingersOccluded==1)] <- NA
  
  # assign lam temporary in the global environment (temporarily)
  assign("lam", lam, envir = .GlobalEnv)
  
  if(any(is.na(y.raw)))
  {
    # which rows contain NAs?
    missing.frames <- attr(na.omit(y.raw), "na.action")
    
    fit <- sreg(x[-missing.frames], na.omit(y.raw),lambda=lam)
    
    occluded.frames.check <- data.frame(
      f = (fingersOccluded*x)[fingersOccluded*x != 0]
      ,
      occluded = (fingersOccluded*framesOccluded)[fingersOccluded*framesOccluded != 0]
    )
    occluded.frames.check$group <- with(occluded.frames.check, c(0, cumsum(diff(f) != 1)))
    
    # assign maxFrames temporary in the global environment (temporarily)
    assign("maxFrames", maxFrames, envir = .GlobalEnv)
    
    occluded.frames.check <- ddply(occluded.frames.check, .(group), mutate,
                                   repairable = ifelse(max(occluded) <= maxFrames, 'repair', 'discard'))
    
    frames.to.interpolate <- match(occluded.frames.check$f[occluded.frames.check$repairable=='repair'], x)
    
    y <- predict(fit, x) * ifelse(fingersOccluded == 1, NA, 1)
    y[frames.to.interpolate] <- predict(fit, x[frames.to.interpolate])

    # remove maxFrames from the global environment
    remove(maxFrames, envir = .GlobalEnv)
    # remove lam from the global environment
    remove(lam, envir = .GlobalEnv)
    
    return(y)
    
  }else
  {
    fit <- sreg(x, y.raw,lambda=lam)
    y <- predict(fit, x)
    
    # remove lam from the global environment
    remove(lam, envir = .GlobalEnv)
    
    return(y)
  }
}

####################################################################################  13.
# function to smooth the GA (used in kin.SmoothAndDerive)
#' @export
cc.GA.smooth.repair <- function(x, y, trialN, lam = 1e-04, maxFrames = 17, fingersOccluded, framesOccluded, n.lmax, n.lmax.lim = 60)
{
  if(length(n.lmax) > 1)
    n.lmax <- unique(n.lmax)
  if(length(trialN) > 1)
    trialN <- unique(trialN)
  
  GA <- NULL
  if(n.lmax > n.lmax.lim)
  {
    cat("Trial #", trialN, ": too many local maxima (", n.lmax, "). Smoothing with lambda = ", lam, ".\n", sep='')
    GA <- cc.smooth.repair(x, y, lam, maxFrames, fingersOccluded, framesOccluded)
    return(GA)
  } else if(n.lmax > 5)
  {
    
    lmaxmin <- NULL
    
    while (n.lmax > 5)
    {
      GA <- cc.smooth.repair(x, y, lam, maxFrames, fingersOccluded, framesOccluded)
      lmaxmin <- which(diff(sign(diff(GA)))==-2)+1
      n.lmax <- length(lmaxmin)
      lam <- lam + .005
      if(lam > 10)
        stop(cat("smoothing parameter too big. Trial #", trialN))
    }
    return(GA)
  } else
    return(y)
}

####################################################################################  14.
# function to locate the local maxima (used in kin.extract)
#' @export
lmax.locate <- function(x, y)
{
  lmaxmin <- which(diff(sign(diff(y)))==-2)+1
  return(x %in% x[lmaxmin])
}

####################################################################################  15.
# mean without NAs by default
#' @export
cc.mean <- function(x) {mean(x, na.rm=T)}

####################################################################################  16.
# function to sort a data.frame by column(s) name or number
#' @export
sortdataframe <- function(x, decreasing=FALSE, by=1, ... ){
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}

####################################################################################  17.
# standard error (from package sciplot)
#' @export
se <- function (x, na.rm = TRUE) {sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))}
