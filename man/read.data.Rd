% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.data.r
\name{read.data}
\alias{read.data}
\title{Read Multiple Data Files from a Directory}
\usage{
read.data(directory, lookfor = ".txt", .parallel = F)
}
\arguments{
\item{directory}{Character string specifying the path to the directory containing the data files}

\item{lookfor}{Character string specifying the file extension or pattern to match (default = ".txt")}

\item{.parallel}{Logical value indicating whether to use parallel processing (default = FALSE)}
}
\value{
A data frame containing the combined data from all matching files
}
\description{
Reads multiple data files from a specified directory and combines them into a single data frame.
The function supports parallel processing for improved performance with large datasets.
}
\details{
The function:
* Changes the working directory to the specified location
* Finds all files matching the specified pattern
* Reads each file as a data table with headers
* Combines all data tables into a single data frame
* Supports parallel processing via parLapply when .parallel = TRUE
}
\note{
When using parallel processing, make sure to have a cluster object 'cl' initialized beforehand.
}
\examples{
# Read all .txt files from a directory
data <- read.data("path/to/files", lookfor = ".txt")

# Read all .csv files using parallel processing
data <- read.data("path/to/files", lookfor = ".csv", .parallel = TRUE)

}
