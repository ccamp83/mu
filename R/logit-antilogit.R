#' Logit Transformation
#' 
#' @description
#' Calculates the logit transformation of a probability (p), defined as log(p/(1-p)).
#' The logit function is the inverse of the logistic function.
#' 
#' @param x Numeric value or vector of probabilities between 0 and 1
#' 
#' @return The logit-transformed value(s)
#' 
#' @examples
#' logit(0.5)  # Returns 0
#' logit(0.75) # Returns ~1.099
#' 
#' @export
logit <- function(x) log(x / (1-x))

#' Anti-logit (Logistic) Transformation
#' 
#' @description
#' Calculates the inverse logit (logistic) transformation of a real number.
#' This function transforms any real number into a probability between 0 and 1.
#' 
#' @param x Numeric value or vector of real numbers
#' 
#' @return Probability value(s) between 0 and 1
#' 
#' @examples
#' antilogit(0)    # Returns 0.5
#' antilogit(1.099) # Returns ~0.75
#' 
#' @export
antilogit <- function(x) {1 / (1 + exp(-x))}