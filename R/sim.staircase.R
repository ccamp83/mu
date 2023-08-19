#' @export
sim.staircase <- function(sc.specs,
                          mu,
                          sd,
                          maxTrials,
                          maxReversals,
                          maxSteps,
                          delta.gradient,
                          seed = NULL
                          )
{

  if(is.null(seed))
  {
    seed <- sample(1:1000, 1)
  }

  set.seed(seed)

  # scaling factor to apply to delta at each reversal
  switch(delta.gradient,

         fixed = {
           deltaScale <- rep(1, maxReversals)
         },

         exp = {
           stepRatio <- (0:(maxReversals-1))/(maxReversals-1)
           deltaScale <- exp(-stepRatio)
         },

         half = {
           stepChunks <- cut(1:maxReversals, breaks = 2, labels = F)-1
           deltaScale <- 1/(2^(stepChunks))
         }
  )

  exit <- F

  # calculate staircases specs
  for(s in 1:length(sc.specs))
  {
    # staircase 1 specs
    sc.specs[[s]] <- mutate(sc.specs[[s]],
                                delta.forw = ifelse(direction < 0, as.numeric(down.param["delta"]), as.numeric(up.param["delta"])),
                                delta.back = ifelse(delta.forw == as.numeric(down.param["delta"]), as.numeric(up.param["delta"]), as.numeric(down.param["delta"])),
                                maxSteps = ifelse(direction < 0, as.numeric(down.param["steps"]), as.numeric(up.param["steps"]))
    )
  }

  # initializing staircases
  for(s in 1:length(sc.specs))
  {
    # staircase 1 specs
    sc.specs[[s]] <- mutate(sc.specs[[s]],
                            stepN = 1,
                            stepRep = 1,
                            reversalN = 0,
                            value = startValue,
                            delta = NA,
                            active = T
    )
  }

  # initializing task
  trialN <- 1

  # --- loop ---

  # staircase data
  scData <- NULL

  while(!exit)
  {
    # check the active staircases
    activeStaircasesID <- NULL # reset counter
    for(s in 1:length(sc.specs))
    {
      if(sc.specs[[s]]$active)
      {
        activeStaircasesID <- c(activeStaircasesID, s)
      }
    }

    # select a staircase at random among the active ones
    currentStaircaseID <- ifelse(length(activeStaircasesID) > 1, sample(activeStaircasesID, 1), activeStaircasesID)
    staircase <- sc.specs[[currentStaircaseID]]

    # calculate stimulus probability given the observer's estimate distribution
    p.stim <- pnorm(staircase$value, mu, sd)

    # produce a response
    resp <- rbinom(1, 1, p.stim)

    # record staircase data
    scData <- rbind(scData,
                    data.frame(trialN,
                               staircaseID = currentStaircaseID,
                               stepN = staircase$stepN,
                               stepRep = staircase$stepRep,
                               reversalN = staircase$reversalN,
                               delta = staircase$delta,
                               value = staircase$value,
                               resp))

    # update reversalN
    isReversal <- (resp & staircase$direction > 0) || (!resp & staircase$direction < 0)

    # if this is a reversal
    # reset stepRep
    # update stim using the delta that goes in opposite direction of the staircase
    if(isReversal)
    {
      staircase$stepRep <- 1
      staircase$delta <- staircase$delta.back*deltaScale[staircase$reversalN+1]
      staircase$value <- staircase$value + staircase$delta
      staircase$reversalN <- staircase$reversalN + 1
    } else
    {
      # if this is not a reversal

      #   if stepsN < maxSteps (defined by the direction of the staircase)
      #     increment stepRep
      #   if stepRep == maxsteps (defined by the direction of the staircase)
      #     reset stepRep
      #     update stim using the delta that goes in the same direction as the staircase
      #   else something's wrong
      if(staircase$stepRep < staircase$maxSteps)
      {
        staircase$stepRep <- staircase$stepRep + 1
      } else if (staircase$stepRep == staircase$maxSteps)
      {
        staircase$stepRep <- 1
        staircase$delta <- staircase$delta.forw*deltaScale[staircase$reversalN+1]
        staircase$value <- staircase$value + staircase$delta
      } else
      {
        stop("Staircase update failed.")
      }
    }

    # update staircase
    staircase$active <- staircase$reversalN < maxReversals && staircase$stepN < maxSteps
    staircase$stepN <- staircase$stepN + 1
    sc.specs[[currentStaircaseID]] <- staircase

    # update trialN
    trialN <- trialN + 1

    # exit
    # check again how many staircases are active
    activeStaircasesID <- NULL # reset counter
    for(s in 1:length(sc.specs))
    {
      if(sc.specs[[s]]$active)
      {
        activeStaircasesID <- c(activeStaircasesID, s)
      }
    }

    exit <- (trialN == maxTrials) || length(activeStaircasesID) == 0
  }

  return(scData)
}
