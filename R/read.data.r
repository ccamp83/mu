#' Read Multiple Data Files from a Directory
#'
#' @description
#' Reads multiple data files from a specified directory and combines them into a single data frame.
#' The function supports parallel processing for improved performance with large datasets.
#'
#' @param directory Character string specifying the path to the directory containing the data files
#' @param lookfor Character string specifying the file extension or pattern to match (default = ".txt")
#' @param .parallel Logical value indicating whether to use parallel processing (default = FALSE)
#'
#' @return A data frame containing the combined data from all matching files
#'
#' @details
#' The function:
#' * Changes the working directory to the specified location
#' * Finds all files matching the specified pattern
#' * Reads each file as a data table with headers
#' * Combines all data tables into a single data frame
#' * Supports parallel processing via parLapply when .parallel = TRUE
#'
#' @note
#' When using parallel processing, make sure to have a cluster object 'cl' initialized beforehand.
#'
#' @examples
#' # Read all .txt files from a directory
#' data <- read.data("path/to/files", lookfor = ".txt")
#'
#' # Read all .csv files using parallel processing
#' data <- read.data("path/to/files", lookfor = ".csv", .parallel = TRUE)
#'
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
