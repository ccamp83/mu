#' @export
kin.handDistance <- function(datatraj, datakin, chop=TRUE)
{

  dataset <- merge(datatraj, datakin[c("trialN",
                                       "FIPx","FIPy","FIPz",
                                       "FTPx","FTPy","FTPz",
                                       "FGPx","FGPy","FGPz",
                                       "frameToFGA")], by=c("trialN"))
  
  max_ind_distance <- with(dataset, cc.max(sqrt(cc.max(indexX)^2 + cc.max(indexY)^2 + cc.max(indexZ)^2)))
  max_thu_distance <- with(dataset, cc.max(sqrt(cc.max(thumbX)^2 + cc.max(thumbY)^2 + cc.max(thumbZ)^2)))
  max_grip_distance <- with(dataset, cc.max(sqrt(cc.max(GPX)^2 + cc.max(GPY)^2 + cc.max(GPZ)^2)))
  max_distance <<- ceiling(max(c(max_ind_distance), max(max_thu_distance), max(max_grip_distance)))
  
  dataset <- ddply(dataset, .(subjName), mutate,
                   thuDist = sqrt((thumbX-FTPx)^2 + (thumbY-FTPy)^2 + (thumbZ - FTPz)^2),
                   thuDistB = cut(thuDist, breaks = seq(max_distance,0,length.out = 100),dig.lab=1,labels = F),
                   indDist = sqrt((indexX-FIPx)^2 + (indexY-FIPy)^2 + (indexZ - FIPz)^2),
                   indDistB = cut(indDist, breaks = seq(max_distance,0,length.out = 100),dig.lab=1,labels = F),
                   gripDist = sqrt((GPX-FGPx)^2 + (GPY-FGPy)^2 + (GPZ - FGPz)^2),
                   gripDistB = cut(gripDist, breaks = seq(max_distance,0,length.out = 100),dig.lab=1,labels = F)
  )
  
  dataset$thuDistB <- 101-dataset$thuDistB
  dataset$indDistB <- 101-dataset$indDistB
  dataset$gripDistB <- 101-dataset$gripDistB
  
  output <- dataset[!names(dataset) %in% c("FIPx","FIPy","FIPz",
                                           "FTPx","FTPy","FTPz",
                                           "FGPx","FGPy","FGPz")]
  if(chop)
  {
    output <- subset(output, frameN <= frameToFGA)
  }
  
  remove(max_distance, envir=.GlobalEnv)
  
  return(output)
}