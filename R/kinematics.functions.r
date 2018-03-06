# Summary:
# 1. kin.SmoothAndDerive
# 2. data.check
# 3. data.review
# 4. kin.extract
# 5. data.norm

############################################################################### 1.
# Smooth and derive trajectories
# kin.SmoothAndDerive <- function(dataset, lambda = .0005, frames.to.interpolate = 17) # TODO: see below -- , index=T, thumb=T)
# {
#   
#   ### debug calls (uncomment and run to debug):
#   # data(debugData)
#   # dataset <- debugData
#   # lambda <- .0005; frames.to.interpolate <- 17
#   
#   ### functions to wrap lines of code with to get process time
#   # prc <- proc.time()
#   # -- test code --
#   # proc.time() - prc
#   
#   # TODO: implement 3 cases (both fingers recorded, index only, thumb only)
#   #   # case 1: both fingers are recorded
#   #   if(index==T & thumb==F) ##NOT WORKING
#   #   {
#   
#   # fingers z coords are reversed in sign
#   dataset$indexZraw <- -1*dataset$indexZraw
#   dataset$thumbZraw <- -1*dataset$thumbZraw
#   
#   # Grip Aperture (raw)
#   # GA is the 3D euclidean distance between the index and the thumb
#   dataset$GAraw <- with(dataset,
#                         sqrt(
#                           (indexXraw - thumbXraw)^2 +
#                             (indexYraw - thumbYraw)^2 +
#                             (indexZraw - thumbZraw)^2
#                         )
#   )
#   
#   # 3D Grip Position (raw)
#   # GP is the midway position between index and thumb
#   dataset$GPXraw <- with(dataset, (indexXraw + thumbXraw)/2)
#   dataset$GPYraw <- with(dataset, (indexYraw + thumbYraw)/2)
#   dataset$GPZraw <- with(dataset, (indexZraw + thumbZraw)/2)
#   
#   # 3D Grip Orientation (raw)
#   # GOF is the angle between the parallel projection of GA and the x axis on the Frontal (coronal) plane
#   dataset$GOFraw <- with(dataset, atan( (indexYraw-thumbYraw) / (indexXraw-thumbXraw) )*180/pi )
#   # GOS is the angle between the parallel projection of GA and the z axis on the Sagittal plane
#   dataset$GOSraw <- with(dataset, atan( (indexYraw-thumbYraw) / (indexZraw-thumbZraw) )*180/pi )
#   # GOT is the angle between the parallel projection of GA and the z axis on the Transverse plane
#   dataset$GOTraw <- with(dataset, atan( (indexXraw-thumbXraw) / (indexZraw-thumbZraw) )*180/pi )
#   
#   # assign lambda to global environment for looping (temporarily)
#   assign("lambda", lambda, envir = .GlobalEnv)
#   # assign frames.to.interpolate to global environment for looping (temporarily)
#   assign("frames.to.interpolate", frames.to.interpolate, envir = .GlobalEnv)
#   
#   # smooth and derive
#   dataset <- ddply(dataset, .(trialN), mutate,
#                    
#            # index
#            indexX=cc.smooth.repair(frameN,indexXraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            indexXvel=cc.derive(frameN,indexX,d=1),
#            indexY=cc.smooth.repair(frameN,indexYraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            indexYvel=cc.derive(frameN,indexY,d=1),
#            indexZ=cc.smooth.repair(frameN,indexZraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            indexZvel=cc.derive(frameN,indexZ,d=1),
#            indexVel=sqrt(indexXvel^2 + indexYvel^2 + indexZvel^2),
#            indexAcc=cc.derive(frameN,indexVel,d=1),
#            indexJerk=cc.derive(frameN,indexAcc,d=2),
#            indexAllKinRaw=sqrt(indexVel^2 + indexAcc^2 + indexJerk^2),
#            
#            # thumb
#            thumbX=cc.smooth.repair(frameN,thumbXraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            thumbXvel=cc.derive(frameN,thumbX,d=1),
#            thumbY=cc.smooth.repair(frameN,thumbYraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            thumbYvel=cc.derive(frameN,thumbY,d=1),
#            thumbZ=cc.smooth.repair(frameN,thumbZraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            thumbZvel=cc.derive(frameN,thumbZ,d=1),
#            thumbVel=sqrt(thumbXvel^2 + thumbYvel^2 + thumbZvel^2),
#            thumbAcc=cc.derive(frameN,thumbVel,d=1),
#            thumbJerk=cc.derive(frameN,thumbAcc,d=2),
#            thumbAllKinRaw=sqrt(thumbVel^2 + thumbAcc^2 + thumbJerk^2),
#            
#            # grip aperture
#            GA=cc.smooth.repair(frameN,GAraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            GAvel=cc.derive(frameN,GA,d=1),
#            GAacc=cc.derive(frameN,GA,d=2),
#            GAjerk=cc.derive(frameN,GAacc,d=1),
#            GAallKinRaw=sqrt(GAvel^2 + GAacc^2 + GAjerk^2),
#            
#            # grip orientation
#            GOF=cc.smooth.repair(frameN,GOFraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            GOS=cc.smooth.repair(frameN,GOSraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            GOT=cc.smooth.repair(frameN,GOTraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            
#            # grip position
#            GPX=cc.smooth.repair(frameN,GPXraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            GPXvel=cc.derive(frameN,GPX,d=1),
#            GPY=cc.smooth.repair(frameN,GPYraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            GPYvel=cc.derive(frameN,GPY,d=1),
#            GPZ=cc.smooth.repair(frameN,GPZraw, lam = lambda, maxFrames= frames.to.interpolate, fingersOccluded=fingersOccluded, framesOccluded=framesOccluded),
#            GPZvel=cc.derive(frameN,GPZ,d=1),
#            GPvel=sqrt(GPXvel^2 + GPYvel^2 + GPZvel^2),
#            GPacc=cc.derive(frameN,GPvel,d=1),
#            GPjerk=cc.derive(frameN,GPacc,d=2),
#            GPallKinRaw=sqrt(GPvel^2 + GPacc^2 + GPjerk^2),
#            
#            # global momentum
#            fingersAllKinRaw=sqrt(indexAllKinRaw^2 + thumbAllKinRaw^2 + GAallKinRaw^2 + GPallKinRaw^2)
#                    
#   )
#   #  }
#   
#   # remove lambda from global environment
#   remove(lambda, envir = .GlobalEnv)
#   # remove frames.to.interpolate from global environment
#   remove(frames.to.interpolate, envir = .GlobalEnv)
#   
#   return(dataset)
#   
# }

############################################################################### 2.

# #' check the output file and provide fixes if available
# data.check <- function (dataset, refreshRate = 85, ...) 
# {
#   dataset <- ddply(dataset, .(trialN), mutate, 
#                    frameT = c(NA, diff(time)))
#   theorframeT <- 1000/refreshRate # in millisecs
#   # we establish that frameT can be accepted only if equal to the theoretical frameT or multiple of it (only greater cases)
#   # we remove all values that are smaller than 90% of the theoretical frameT
#   dataset$frameT <- with(dataset, ifelse(frameT < theorframeT*.9, NA, frameT))
#   
#   reqCols <- c("subjName", "frameN", "time", "fingersOccluded", 
#                "framesOccluded", "globalTime")
#   missingCols <- reqCols[reqCols %in% names(dataset) == F]
#   if (length(missingCols) > 0) {
#     print("The following columns do not exist:")
#     print(sprintf("%s", missingCols))
#     print("Fixing...")
#     if ("subjName" %in% missingCols) {
#       print("Please input subject name:")
#       name <- readline()
#       dataset$subjName <- sprintf("%s", name)
#     }
#     if ("frameN" %in% missingCols) {
#       dataset <- cc.frameN(dataset)
#     }
#     if ("time" %in% missingCols) {
#       dataset <- cc.time(dataset, refreshRate)
#     }
#     if ("fingersOccluded" %in% missingCols) {
#       dataset <- cc.fingersOccluded(dataset)
#     }
#     if ("framesOccluded" %in% missingCols) {
#       dataset <- cc.framesOccluded(dataset)
#     }
#     
#     if(any(ddply(dataset, .(trialN), summarise,
#                  any(fingersOccluded) - any(framesOccluded))[2] < 0)) {
#       print("fingersOccluded does not look right: fixing...")
#       dataset <- cc.fingersOccluded(dataset)
#     }
#     
#     if(any(ddply(dataset, .(trialN), summarise,
#                  any(fingersOccluded) - any(framesOccluded))[2] > 0)) {
#       print("framesOccluded does not look right: fixing...")
#       dataset <- cc.framesOccluded(dataset)
#     }
#     
#     if ("globalTime" %in% missingCols) {
#       dataset <- cc.globalTime(dataset)
#     }
#     print("Database fixed successfully.")
#   }
#   else {
#     if(any(ddply(dataset, .(trialN), summarise,
#                  any(fingersOccluded) - any(framesOccluded))[2] < 0)) {
#       print("fingersOccluded does not look right: fixing...")
#       dataset <- cc.fingersOccluded(dataset)
#     }
#     
#     if(any(ddply(dataset, .(trialN), summarise,
#                  any(fingersOccluded) - any(framesOccluded))[2] > 0)) {
#       print("framesOccluded does not look right: fixing...")
#       dataset <- cc.framesOccluded(dataset)
#     }
#     
#     print("Database looks good.")
#   }
#   return(dataset)
# }

############################################################################### 3.

#' review the output file
#' @export
data.review <- function(dataset, grid.size=20, print=T, res=28, frame=F)
{
  # check the overall visibility of the fingers in each trial
  data.rev <- ddply(dataset[c('trialN','fingersOccluded')], .(trialN), summarise, 
                    percent.occluded=round(mean(fingersOccluded)*100, digits=2),
                    discard=ifelse(percent.occluded >= 80.0, 'DISCARD','OK'),
                    review=ifelse(percent.occluded >= 50.0 & discard=='OK', 'REVIEW','OK')
  )
  
  data.rev1 <- subset(data.rev, discard=='OK')
  data.rev2 <- subset(data.rev, discard=='DISCARD')
  data.rev3 <- subset(data.rev, review=='REVIEW')
  
  overallQuality <- 100 - median(data.rev$percent.occluded)
  
  reviewTrials <- data.rev3$trialN
  data.rev <- data.rev[with(data.rev, order(percent.occluded, decreasing=T)),]
  reviewOrder <- data.rev$trialN
  
  results <- list("reviewOrder"= reviewOrder, "data.rev" = data.rev, "overallQuality" = overallQuality)
  
  if(print)
  {
    print(view.summary.trials(trials=reviewOrder, dataset=dataset, grid.size=grid.size, res=res, frame=frame))
  }
  return(results)
}

############################################################################### 4.
# Extract kinematics
# kin.extract <- function(dataset, target=NULL)
# {
#   # debug lines
#   # dataset <- mse
#   # targetReg <- 150
#   
#   # errors in direction (final x and y position of the grasp) are typically smaller than 
#   # errors in depth (final z position of the grasp), therefore we look for the minimum momentum of grasp kinematics
#   # when the hand is at its closet distance from (0,0) in x,y coords
#   # we exploit the pythagorean theorem to track the distance of the grasp from such origin
#   dataset$graspToObjXYdistance <- with(dataset, sqrt(GPX^2 + GPY^2))
#   dataset <- ddply(dataset, .(trialN), mutate, minHandTargDist=min(graspToObjXYdistance, na.rm=T))
#   
#   # within a region defined by a circle of radius 15cm centered at (0,0) x and y coords.
#   dataset$graspInTargetRegion <- with(dataset, ifelse(graspToObjXYdistance < range(dataset$minHandTargDist)[2]+50.0, 1, 0))
#   
#   # extraction of info about final grasp from the target region
#   data.final <- ddply(subset(dataset, graspInTargetRegion==1), .(trialN), summarise, 
#                       # Final Grip Aperture
#                       FGA = GA[match(cc.min(fingersAllKinRaw), fingersAllKinRaw)],
#                       timeToFGA = time[match(FGA, GA)],
#                       frameToFGA = frameN[match(FGA, GA)],
#                       motionAtFGA = cc.min(fingersAllKinRaw),
#                       
#                       # Final Grip Orientation
#                       FGOf= GOF[match(FGA, GA)],
#                       FGOs= GOS[match(FGA, GA)],
#                       FGOt= GOT[match(FGA, GA)],
#                       
#                       # Final Grip Position
#                       FGPx=GPX[match(FGA, GA)],
#                       FGPy=GPY[match(FGA, GA)],
#                       FGPz=GPZ[match(FGA, GA)],
#                       
#                       # Final Index Position
#                       FIPx=indexX[match(FGA, GA)],
#                       FIPy=indexY[match(FGA, GA)],
#                       FIPz=indexZ[match(FGA, GA)],
#                       
#                       # Final Thumb Position
#                       FTPx=thumbX[match(FGA, GA)],
#                       FTPy=thumbY[match(FGA, GA)],
#                       FTPz=thumbZ[match(FGA, GA)],
#                       
#                       # Final Grasp-from-Object XY Distance 
#                       FgraspToObjXYdistance = graspToObjXYdistance[match(FGA, GA)]
#   )
#   
#   # bring data.final temporary in the global environment (not the best solution, TO REVIEW)
#   assign("data.final", data.final, envir = .GlobalEnv)
#   
#   # drop everything after the FGA
#   dataset <- ddply(dataset, .(trialN), mutate, 
#                    isMovementOver = ifelse(frameN <= data.final$frameToFGA[data.final$trialN==unique(trialN)], 0, 1)
#   )
#   
#   ## TODO: extraction of terminal grasp TGA
#   
#   # extraction of global info from the entire movement
#   data.global <- ddply(subset(dataset, isMovementOver==0), .(trialN), summarise,
#                        
#                        # Maximum Grip Aperture
#                        MGA = cc.max(GA),
#                        timeToMGA = time[match(MGA, GA)],
#                        frameToMGA = frameN[match(MGA, GA)],
#                        
#                        # Grip Orientation at MGA
#                        GOf.MGA = GOF[match(MGA, GA)],
#                        GOs.MGA= GOS[match(MGA, GA)],
#                        GOt.MGA= GOT[match(MGA, GA)],
#                        
#                        # Grip Position at MGA
#                        GPx.MGA=GPX[match(MGA, GA)],
#                        GPy.MGA=GPY[match(MGA, GA)],
#                        GPz.MGA=GPZ[match(MGA, GA)],
#                        
#                        # Index Position at MGA
#                        IPx.MGA=indexX[match(MGA, GA)],
#                        IPy.MGA=indexY[match(MGA, GA)],
#                        IPz.MGA=indexZ[match(MGA, GA)],
#                        
#                        # Thumb Position at MGA
#                        TPx.MGA=thumbX[match(MGA, GA)],
#                        TPy.MGA=thumbY[match(MGA, GA)],
#                        TPz.MGA=thumbZ[match(MGA, GA)],
#                        
#                        # Maximum Grip Velocity
#                        MGV = cc.max(GPvel),
#                        timeToMGV = time[match(MGV, GPvel)], # Acceleration Phase
#                        frameToMGV = frameN[match(MGV, GPvel)],
#                        
#                        # Maximum Grip Acceleration
#                        MGACC = cc.max(GPacc),
#                        timeToMGACC = time[match(MGACC, GPacc)], # Acceleration Phase
#                        frameToMGACC = frameN[match(MGACC, GPacc)],
#                        
#                        # Maximum Grip Aperture Velocity
#                        MGAvel = cc.max(GAvel),
#                        timeToMGAvel = time[match(MGAvel, GAvel)], # Acceleration Phase
#                        frameToMGAvel = frameN[match(MGAvel, GAvel)],
#                        
#                        # Maximum Grip Aperture Acceleration
#                        MGAacc = cc.max(GAacc),
#                        timeToMGAacc = time[match(MGAacc, GAacc)], # Acceleration Phase
#                        frameToMGAacc = frameN[match(MGAacc, GAacc)],
#                        
#                        # Movement Time
#                        movTime = cc.max(time)
#   )
#   
#   # merge datasets
#   data.extract <- merge(data.final, data.global, "trialN")
# 
#   # remove data.final from the global environment
#   remove(data.final, envir = .GlobalEnv)
#   
#   # Deceleration Phase
#   data.extract$decelPhase <- with(data.extract, timeToFGA - timeToMGV)
#   data.extract$accelDecelRatio <- with(data.extract, timeToMGV/decelPhase)
#   
#   return(round(data.extract, digits=3))
# }

### debug lines
# test1 <- allData
# prc <- proc.time()
# (test1 <- kin.extract(test1))
# proc.time()-prc

###############################################################################
## yet to implement / incomplete
data.norm <- function(dataset)
{
  
  dataset <- ddply(dataset, .(trialN), summarise,
                   
                   frameN=rescale(frameN, newrange=c(0,1)),
                   GA=rescale(GA, newrange=c(0,1)),
                   GAXtheta=rescale(GAXtheta, newrange=c(0,1)),
                   GAYtheta=rescale(GAYtheta, newrange=c(0,1)),
                   GAZtheta=rescale(GAZtheta, newrange=c(0,1)),
                   fingersAllKin=rescale(fingersAllKin, newrange=c(0,1)),
                   time=rescale(time, newrange=c(0,1)),
                   HPX=rescale(HPX, newrange=c(0,1)),
                   HPY=rescale(HPY, newrange=c(0,1)),
                   HPZ=rescale(HPZ, newrange=c(0,1)),
                   indexX=rescale(indexX, newrange=c(0,1)),
                   indexY=rescale(indexY, newrange=c(0,1)),
                   indexZ=rescale(indexZ, newrange=c(0,1)),
                   thumbX=rescale(thumbX, newrange=c(0,1)),
                   thumbY=rescale(thumbY, newrange=c(0,1)),
                   thumbZ=rescale(thumbZ, newrange=c(0,1)),
                   indexAllKinRaw=rescale(indexAllKinRaw, newrange=c(0,1)),
                   thumbAllKinRaw=rescale(thumbAllKinRaw, newrange=c(0,1))
                   
                   )
  return(dataset)
}
