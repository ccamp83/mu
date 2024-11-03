#' Calculate Partial Eta Squared
#' 
#' @description
#' Calculates the partial eta squared (η²p) effect size statistic from an F-value 
#' and its degrees of freedom. Partial eta squared represents the proportion of 
#' variance in the dependent variable that is associated with the independent 
#' variable, when controlling for other variables.
#' 
#' @param Fval Numeric. The F statistic value
#' @param dfEffect Numeric. Degrees of freedom for the effect
#' @param dfError Numeric. Degrees of freedom for the error
#' 
#' @return
#' Returns a numeric value between 0 and 1 representing the partial eta squared effect size
#' 
#' @details
#' The formula used is: η²p = (F * df_effect) / (F * df_effect + df_error)
#' 
#' Partial eta squared values can be interpreted roughly as:
#' * Small effect: ~0.01
#' * Medium effect: ~0.06
#' * Large effect: ~0.14
#' 
#' @examples
#' # Calculate partial eta squared for F(2,97) = 5.4
#' etaSqP(5.4, 2, 97)
#' 
#' @export
etaSqP <- function(Fval, dfEffect, dfError) { (Fval * dfEffect) / ((Fval * dfEffect) + dfError) }
