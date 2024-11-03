#' Calculate Cohen's w Effect Size
#' 
#' @description
#' Calculates Cohen's w, which is a measure of effect size used for chi-square tests
#' of goodness of fit and contingency tables. It measures the discrepancy between 
#' observed and expected proportions.
#' 
#' @param chi_sq Numeric value of the chi-square statistic
#' @param n Integer representing the total sample size
#' 
#' @return
#' Returns a numeric value representing Cohen's w effect size.
#' The interpretation guidelines are:
#' * 0.1: small effect
#' * 0.3: medium effect
#' * 0.5: large effect
#' 
#' @details
#' Cohen's w is calculated as the square root of (chi-square / N), where N is the
#' total sample size. This effect size is particularly useful for chi-square tests
#' as it provides a standardized measure of effect that is independent of sample size.
#' 
#' @examples
#' # For a chi-square value of 10 with 100 observations
#' cohen.w(chi_sq = 10, n = 100)
#' 
#' # For a chi-square value of 25 with 200 observations
#' cohen.w(chi_sq = 25, n = 200)
#' 
#' @references
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.).
#' Lawrence Erlbaum Associates.
#' 
#' @export
cohen.w <- function (chi_sq, n) { sqrt(chi_sq/n) }
