trialRepair <- function(n_trial, trajData, kinData, modifications=NULL, frame=T, ...)
{
  test.env <<- new.env()
  
  # check which arguments have been declared
  arguments <- as.list(match.call())
  test.env$modifications <- arguments$modifications
  
  if(!is.null(test.env$modifications))
  {
    if(is.character(test.env$modifications))
      subset_argument <- paste("trialN==repTrial & ", test.env$modifications, sep='')
    else
      subset_argument <- paste("trialN==repTrial & ", eval(test.env$modifications), sep='')
  } else
    subset_argument <- "trialN==repTrial"
  
  repTrial <- n_trial
  
  if(frame)
  {
    before <- view.single.trial.GA(repTrial, trajData, kinData, frame=T) + labs(title=paste("trial ", repTrial, ": before", sep=''))
    kinData[kinData$trialN==repTrial,] <- kin.extract(subset(trajData, eval(parse(text=subset_argument))))
    after <- view.single.trial.GA(repTrial, trajData, kinData, frame=T) + labs(title=paste("trial ", repTrial, ": after", sep=''))
    grid.arrange(before, after, ncol=2)
  } else
  {
    before <- view.single.trial.GA(repTrial, trajData, kinData, frame=F) + labs(title=paste("trial ", repTrial, ": before", sep=''))
    kinData[kinData$trialN==repTrial,] <- kin.extract(subset(trajData, eval(parse(text=subset_argument))))
    after <- view.single.trial.GA(repTrial, trajData, kinData, frame=F) + labs(title=paste("trial ", repTrial, ": after", sep=''))
    grid.arrange(before, after, ncol=2)
  }
  
  rm(test.env, envir=.GlobalEnv)
  
  return(kinData[kinData$trialN==repTrial,])
}

# trialRepair(1, allData, kinData, "frameN>280 & GA < 50")
# stri <- "frameN>280 & GA < 50"
# trialRepair(1, allData, kinData, stri)

#' @export
repair_trials <- function(n_trial, trajData, kinData, ...)
{
  for(temp_trial in 1:length(n_trial))
  {
    repaired <- FALSE
    user_modifications <<- NULL
    
    while(!repaired)
    {    
      current_trial <- n_trial[temp_trial]
      cat("Repairing trial #", current_trial, " ...\n", sep='')
      if(is.null(user_modifications))
        repaired_trial <- trialRepair(current_trial, trajData, kinData)
      cat("Type in modifications and hit Enter to inspect. When the trial is repaired type R and hit Return.")
      user_modifications <<- readline()
      #       return(sprintf("%s",user_modifications))
      if(user_modifications=='R')
      {
        kinData[kinData$trialN==current_trial,] <- repaired_trial
        repaired = TRUE
      } else
      {
        repaired_trial <- trialRepair(current_trial, trajData, kinData, modifications=user_modifications)
      }
    }
    rm(user_modifications, envir=.GlobalEnv)
  }
  return(kinData)
}