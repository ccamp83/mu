#' @title Repair the extraction of kinematics
#' @param n_trial number of the trial to repair
#' @param trajData dataset with trajectory data
#' @param kinData dataset with the extracted kinematics as produced by kin.extract
#' @param modifications in quotes, the modifications to apply in the new extraction of the kinematics (see example)
#' @param MGA.range as in kin.extract
#' @param TGA.type as in kin.extract
#' @param TGA.lim as in kin.extract
#' @param frame logic: should the data be plotted against frames (TRUE) or time (FALSE)
#' @examples
#' 
#' data(rtgData)
#' # always check the data first
#' rtgData <- data.check(rtgData)
#' 
#' # make sure that the trial count starts from 1 and not from zero
#' rtgData <- cc.trialN(rtgData)
#' # smooth and derive the dataset
#' rtgData <- kin.SmoothAndDerive(rtgData)
#' # extract the kinematics
#' kinData <- kin.extract(subset(rtgData, GPZ>100 & GA>15 & frameN > 10), graphs=F, TGA.type="s", TGA.lim = 20)
#' # view all trials at once
#' view.summary.trials.GA(datatraj = rtgData, datakin = kinData, print =F)
#' 
#' # repair trials
#' repaired_trial <- trialRepair(3, rtgData, kinData, "GPZ>100 & GA>15 & frameN > 10", MGA.range = c(70, 90))
#' # this displays the modifications and returns the relative line of kinData BUT does not modifies kinData
#' view.single.trial.GA(3, rtgData, kinData) # if you plot again the trial using kinData, you can see it is still unmodified
#' # in order to apply the modifications to kinData permanently, you need to rewrite that line in it
#' kinData[kinData$trialN==3,] <- repaired_trial
#' view.single.trial.GA(3, rtgData, kinData) # not it shows up correctly
#' 
#' # and so on
#' repaired_trial <- trialRepair(41, rtgData, kinData, "GPZ>100 & GA>15 & frameN > 10", MGA.range = c(10, 50), TGA.lim = 35)
#' kinData[kinData$trialN==41,] <- repaired_trial
#' # it is always preferable to first take a look at the preview without overwriting kinData immediately
#' 
#' # finally, it might be convenient to set a variable with the number of the trial
#' trial_to_repair <- 44
#' repaired_trial <- trialRepair(trial_to_repair, rtgData, kinData, "GPZ>100 & GA>15 & frameN > 10", MGA.range = c(70, 110), TGA.type = "v", TGA.lim = .5)
#' kinData[kinData$trialN==trial_to_repair,] <- repaired_trial
#' 
#' detach(rtgData)
#'
#' @export
trialRepair <- function(n_trial, trajData, kinData, modifications=NULL, MGA.range = NULL, TGA.type = "s", TGA.lim = 20, frame=T, ...)
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
    kinData[kinData$trialN==repTrial,] <- kin.extract(subset(trajData, eval(parse(text=subset_argument))), MGA.range = MGA.range, TGA.type = TGA.type, TGA.lim = TGA.lim)
    after <- view.single.trial.GA(repTrial, trajData, kinData, frame=T) + labs(title=paste("trial ", repTrial, ": after", sep=''))
    grid.arrange(before, after, ncol=2)
  } else
  {
    before <- view.single.trial.GA(repTrial, trajData, kinData, frame=F) + labs(title=paste("trial ", repTrial, ": before", sep=''))
    kinData[kinData$trialN==repTrial,] <- kin.extract(subset(trajData, eval(parse(text=subset_argument))), MGA.range = MGA.range, TGA.type = TGA.type, TGA.lim = TGA.lim)
    after <- view.single.trial.GA(repTrial, trajData, kinData, frame=F) + labs(title=paste("trial ", repTrial, ": after", sep=''))
    grid.arrange(before, after, ncol=2)
  }
  
  rm(test.env, envir=.GlobalEnv)
  
  return(kinData[kinData$trialN==repTrial,])
}