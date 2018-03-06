#' @export
tga.extract <- function(dataset, TGAprop.lim = .2)
{
  assign("dataset", dataset, envir = .GlobalEnv)
  assign("TGAprop.lim", TGAprop.lim, envir = .GlobalEnv)
  
  dataset <- ddply(dataset, .(trialN), mutate, 
                   isMovementOver = ifelse(frameN <= dataset$frameToFGA[dataset$trialN==unique(trialN)], 0, 1)
  )
  
  # drop everything before the MGA
  dataset <- ddply(dataset, .(trialN), mutate, 
                   afterMGA = ifelse(frameN <= dataset$frameToMGA[dataset$trialN==unique(trialN)], 0, 1)
  )
  
  dataset <- ddply(subset(dataset, afterMGA == 1 & isMovementOver==0), .(trialN), mutate,
                   TGAvel.lim = min(GPvel, na.rm = T) + TGAprop.lim * (max(GPvel, na.rm = T) - min(GPvel, na.rm = T))
  )
  dataset$TGAcheck <- with(dataset, GPvel <= TGAvel.lim)
  dataset <- ddply(dataset, .(trialN), mutate,
                   TGAspot = (unlist(lapply(rle(TGAcheck)$lengths, seq_len))*TGAcheck)==1,
                   TGAspot2 = cumsum(TGAspot[!is.na(TGAspot)])*TGAspot == 1
  )
  
  data.tga <- ddply(subset(dataset, afterMGA == 1 & isMovementOver==0), .(trialN), summarise,
                    
                    # Terminal Grip Aperture
                    frameToTGA = frameN[unique(which(TGAspot2==T))], 
                    TGA = GA[match(frameToTGA, frameN)],
                    timeToTGA = time[match(frameToTGA, frameN)],
                    
                    # Grip Orientation at TGA
                    GOf.TGA = GOF[match(frameToTGA, frameN)],
                    GOs.TGA= GOS[match(frameToTGA, frameN)],
                    GOt.TGA= GOT[match(frameToTGA, frameN)],
                    
                    # Grip Position at TGA
                    GPx.TGA=GPX[match(frameToTGA, frameN)],
                    GPy.TGA=GPY[match(frameToTGA, frameN)],
                    GPz.TGA=GPZ[match(frameToTGA, frameN)],
                    
                    # Index Position at TGA
                    IPx.TGA=indexX[match(frameToTGA, frameN)],
                    IPy.TGA=indexY[match(frameToTGA, frameN)],
                    IPz.TGA=indexZ[match(frameToTGA, frameN)],
                    
                    # Thumb Position at TGA
                    TPx.TGA=thumbX[match(frameToTGA, frameN)],
                    TPy.TGA=thumbY[match(frameToTGA, frameN)],
                    TPz.TGA=thumbZ[match(frameToTGA, frameN)]
  )
  
  remove(dataset, envir = .GlobalEnv)
  remove(TGAprop.lim, envir = .GlobalEnv)
  
  return(round(data.tga, digits=3))
}