#' Calculate Within-Subjects Summary Statistics with Error Bars
#'
#' @description
#' A collection of functions for calculating summary statistics and error bars
#' for within-subjects designs, implementing the method described in Morey (2008).
#' This is particularly useful for creating plots with appropriate error bars
#' for within-subjects variables in ggplot2.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{summarySEwithin}: Calculates within-subjects summary statistics
#'   \item \code{summarySE}: Calculates basic summary statistics
#'   \item \code{normDataWithin}: Normalizes data for within-subjects variables
#' }
#'
#' @section summarySEwithin:
#' @param data A data frame containing the variables
#' @param measurevar The name of the measure variable
#' @param betweenvars A vector containing names of between-subjects variables
#' @param withinvars A vector containing names of within-subjects variables
#' @param idvar The name of the variable that identifies subjects
#' @param na.rm Should NA values be removed? (default = FALSE)
#' @param conf.interval Confidence interval level (default = 0.95)
#' @param .drop Should unused factor levels be dropped? (default = TRUE)
#'
#' @return A data frame containing:
#' \itemize{
#'   \item N: number of subjects
#'   \item Original measurevar: raw means
#'   \item sd: standard deviation
#'   \item se: standard error
#'   \item ci: confidence interval
#' }
#'
#' @details
#' This implementation follows Morey's (2008) correction for within-subjects
#' designs. It automatically:
#' \itemize{
#'   \item Converts non-factor variables to factors
#'   \item Normalizes the data within subjects
#'   \item Applies the Morey correction factor
#'   \item Combines normed and un-normed results
#' }
#'
#' @examples
#' # Example with one between-subjects and one within-subjects variable
#' summarySEwithin(data = mydata,
#'                measurevar = "score",
#'                betweenvars = "group",
#'                withinvars = "time",
#'                idvar = "subject")
#'
#' @references
#' Morey, R. D. (2008). Confidence intervals from normalized data: A correction
#' to Cousineau (2005). Tutorial in Quantitative Methods for Psychology, 4(2),
#' 61-64.
#'
#' @source
#' http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#understanding-within-subjects-error-bars
#'
#' @export
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {

  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))

  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }

  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)

  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL

  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)

  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")

  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)

  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )

  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor

  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )

  # Rename the "mean" column
  datac <- plyr::rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}

normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)

  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )

  # Put the subject means with original data
  data <- merge(data, data.subjMean)

  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)

  # Remove this subject mean column
  data$subjMean <- NULL

  return(data)
}
