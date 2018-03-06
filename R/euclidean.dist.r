#' Euclidean distance
#' @export
euclidean.dist <- function(datatraj, datakin=NULL, bins=100)
{
  if(!is.null(datakin))
  {
    dataset <- merge(datatraj, datakin, by="trialN")
  } else
  {
    dataset <- datatraj
  }
  dataset <- ddply(dataset, .(trialN), mutate,
                       GP.euD = sqrt((GPX-FGPx)^2 + (GPY-FGPy)^2 + (GPZ-FGPz)^2),
                       ind.euD = sqrt((indexX-FIPx)^2 + (indexY-FIPy)^2 + (indexZ-FIPz)^2),
                       thu.euD = sqrt((thumbX-FTPx)^2 + (thumbY-FTPy)^2 + (thumbZ-FTPz)^2)
  )
  assign("bins", bins, envir=.GlobalEnv)
  dataset <- ddply(dataset, .(trialN), mutate, 
                   bGP.euD = bins - cut(GP.euD, breaks=bins, labels=F),
                   bind.euD = bins - cut(ind.euD, breaks=bins, labels=F),
                   bthu.euD = bins - cut(thu.euD, breaks=bins, labels=F)
  )
  remove(bins, envir=.GlobalEnv)
  return(dataset)
}