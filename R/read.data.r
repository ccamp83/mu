#' @export
read.data <- function(directory, lookfor=".txt", .parallel = F)
{
  setwd(directory)
  trialfiles <- mixedsort(list.files()[grep(lookfor, list.files())])
  if(!.parallel)
    temp <- lapply(trialfiles, read.table, header=T)
  else
    temp <- parLapply(cl, trialfiles, read.table, header=T)
  allData <- do.call("rbind", temp)
  rm(temp)
  rm(trialfiles)
  return(allData)
}